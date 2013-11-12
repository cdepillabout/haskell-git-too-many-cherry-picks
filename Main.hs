{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Color (colorString, Color(..))
import Codec.Binary.UTF8.String (decode)
import Control.Exception (handle, SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
--import Control.Lens
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.Function (on)
import Data.Git (Commit, commitMessage, commitAuthor, commitParents, getCommit, personName, personEmail, Ref, Revision, resolveRevision, withCurrentRepo)
import Data.Git.Storage (Git)
import Data.Git.Storage.Object (toObject, getRaw, objectHash, objectToType)
import Data.List (deleteFirstsBy, insertBy, nubBy)
import Data.String (fromString)
import qualified Data.Text.Lazy as LazyText
import Helpers (mapList, commitTime)
import MergeBase (getMergeBase)
import Options.Applicative ((<$>), (<>), (<*>), argument, completeWith, execParser, flag, fullDesc, header, help, helper, info, long, metavar, nullOption, ParseError(..), Parser, ParserInfo, progDesc, reader, short, showDefault, showDefaultWith, str, value)
import Shelly (Sh, shelly, run_)
import System.Environment (getProgName)

-- | Data construct for what kind of output we want. These correspond to
-- git-log --pretty formats.
data Pretty = OneLinePretty | ShortPretty | MediumPretty deriving (Show, Eq, Read)

-- | Whether or not to show patches (just like git-diff output). This
-- corresponds to git-log -p.
data Patch = NoPatch | Patch

data Options = Options
    { pretty :: Pretty
    , patch :: Patch
    , commandLineCommitA :: String -- the commit passed on the command line
    , commandLineCommitB :: String -- the commit passed on the command line
    }


getAllCommitsFromTo :: Git -> Commit -> Commit -> IO (Bool, [Commit])
getAllCommitsFromTo git fromCommit toCommit = getAllCommitsFromTo' fromCommit [toCommit]
  where
    getAllCommitsFromTo' :: Commit -> [Commit] -> IO (Bool, [Commit])
    getAllCommitsFromTo' _ [] = return (False, [])
    getAllCommitsFromTo' startingCommit (firstUncheckedCommit : remainingUncheckedCommits)
        | startingCommit == firstUncheckedCommit = return (True, [firstUncheckedCommit])
        | otherwise = do
            let parentRefs = commitParents firstUncheckedCommit
            parentCommits <- mapM (getCommit git) parentRefs
            (result, returnedCommitList) <-
                getAllCommitsFromTo' startingCommit $ mapList insertUniqByCommitDate remainingUncheckedCommits parentCommits
            return (result, firstUncheckedCommit : returnedCommitList)

    insertUniqByCommitDate :: [Commit] -> Commit -> [Commit]
    insertUniqByCommitDate commitList commit
        | commit `elem` commitList = commitList
        | otherwise = insertBy (compare `on` commitTime) commit commitList

shortCommitHash :: Commit -> String
shortCommitHash commit = take 8 $ longCommitHash commit

longCommitHash :: Commit -> String
longCommitHash commit = show $ commitRef commit

commitRef :: Commit -> Ref
commitRef commit =
    let commitObject = toObject commit
        objectType = objectToType commitObject
        rawCommit = getRaw commit
    in  objectHash objectType (fromIntegral (L.length rawCommit)) rawCommit

decodeByteString :: B.ByteString -> String
decodeByteString = decode . B.unpack

commitHashColor :: Color
commitHashColor = Yellow

indentLength :: Int
indentLength = 4

formatShortCommitHash :: Commit -> String
formatShortCommitHash commit = colorString commitHashColor $ shortCommitHash commit

formatLongCommitHash :: Commit -> String
formatLongCommitHash commit = colorString commitHashColor $ "commit " ++ longCommitHash commit

formatCommitAuthor :: Commit -> String
formatCommitAuthor commit = "Author: " ++
        decodeByteString (personName $ commitAuthor commit) ++ " " ++
        "<" ++ decodeByteString (personEmail $ commitAuthor commit) ++ ">"

formatCommitDate :: Commit -> String
formatCommitDate commit = "Date:   " ++ show (commitTime commit)

formatOneLineCommit :: Commit -> String
formatOneLineCommit commit =
    formatShortCommitHash commit ++ " " ++ messageFormat commit ++ "\n"
  where
    messageFormat :: Commit -> String
    messageFormat = takeWhile (/= '\n') . decodeByteString . commitMessage

formatShortCommit :: Commit -> String
formatShortCommit commit =
    formatLongCommitHash commit ++ "\n" ++
        formatCommitAuthor commit ++ "\n" ++
        "\n" ++
        replicate indentLength ' ' ++ messageFormat commit ++ "\n\n"
  where
    messageFormat :: Commit -> String
    messageFormat = takeWhile (/= '\n') . decodeByteString . commitMessage

formatNormalCommit :: Commit -> String
formatNormalCommit commit =
    formatLongCommitHash commit ++ "\n" ++
        formatCommitAuthor commit ++ "\n" ++
        formatCommitDate commit ++ "\n" ++
        "\n" ++
        messageFormat commit ++ "\n"
  where
    messageFormat :: Commit -> String
    messageFormat com = unlines $ map (replicate indentLength ' ' ++ ) $ lines $ decodeByteString $ commitMessage com

runGit_ :: LazyText.Text -> [LazyText.Text] -> Sh ()
runGit_ command options = run_ "git" $ command : options

printDiff :: Commit -> IO ()
printDiff commit = shelly $ do
    let hash = LazyText.pack $ longCommitHash commit
    let colorFlag = LazyText.pack "--color"
    runGit_ "diff" [colorFlag, hash `LazyText.append` "^.." `LazyText.append` hash]
    run_ "echo" []

printCommit :: Commit -> ReaderT Options IO ()
printCommit commit = do
    options <- ask
    case pretty options of
        OneLinePretty -> liftIO $ putStr $ formatOneLineCommit commit
        ShortPretty -> liftIO $ putStr $ formatShortCommit commit
        MediumPretty -> liftIO $ putStr $ formatNormalCommit commit
    case patch options of
        Patch -> liftIO $ printDiff commit
        NoPatch -> return ()

printCommitList :: Revision -> [Commit] -> ReaderT Options IO ()
printCommitList rev commitList = do
    let commitsInString = "Commits only in " ++ show rev
    let commitsInStringLen = length commitsInString
    liftIO $ putStrLn $ colorString BoldMagenta commitsInString
    liftIO $ putStrLn $ colorString BoldMagenta $ replicate commitsInStringLen '-'
    case commitList of
        [] -> liftIO $ putStrLn "(NONE)"
        _ -> mapM_ printCommit commitList

showDifferences :: Revision -> [Commit] -> Revision -> [Commit] -> ReaderT Options IO ()
showDifferences revA commitsFromA revB commitsFromB = do
    let commitsOnlyInA = findItemsOnlyIn commitsFromA commitsFromB
    let commitsOnlyInB = findItemsOnlyIn commitsFromB commitsFromA
    printCommitList revA commitsOnlyInA
    liftIO $ putStrLn ""
    liftIO $ putStrLn ""
    printCommitList revB commitsOnlyInB
    --liftIO $ print $ map commitMessage commitsOnlyInA
  where
    findItemsOnlyIn :: [Commit] -> [Commit] -> [Commit]
    findItemsOnlyIn this other =
        let combinedList = nubBy commitMessageEqual (this ++ other)
        in deleteFirstsBy commitMessageEqual combinedList other

    commitMessageEqual :: Commit -> Commit -> Bool
    commitMessageEqual commitA commitB = commitMessage commitA == commitMessage commitB

diffBranches :: Git -> Revision -> Commit -> Revision -> Commit -> ReaderT Options IO ()
diffBranches git revA commitA revB commitB = do
    maybeMergeBase <- liftIO $ getMergeBase git commitA commitB
    case maybeMergeBase of
        Nothing -> liftIO $ putStrLn "ERROR! Couldn't find merge base!"
        Just mergeBase -> do
            --liftIO $ print mergeBase
            (foundA, commitsFromMergeBaseToCommitA) <- liftIO $ getAllCommitsFromTo git mergeBase commitA
            (foundB, commitsFromMergeBaseToCommitB) <- liftIO $ getAllCommitsFromTo git mergeBase commitB
            --liftIO $ print $ map commitMessage commitsFromMergeBaseToCommitB
            case (foundA, foundB) of
                (True, True) -> showDifferences revA commitsFromMergeBaseToCommitA revB commitsFromMergeBaseToCommitB
                _ -> liftIO $ putStrLn "ERROR! Weird internal error where something couldn't be found..."
    return ()

diff :: Git -> ReaderT Options IO ()
diff git = do
    options <- ask

    let revA = fromString $ commandLineCommitA options
    let revB = fromString $ commandLineCommitB options

    --liftIO $ print revA

    maybeRefA <- getRef revA
    maybeRefB <- getRef revB

    --liftIO $ print maybeRefA

    case (maybeRefA, maybeRefB) of
        (Just refA, Just refB) -> do
            commitA <- lift $ getCommit git refA
            commitB <- lift $ getCommit git refB
            diffBranches git revA commitA revB commitB
        _ -> return () -- printing error string is handled in handlerGetRef

  where
    getRef :: Revision -> ReaderT Options IO (Maybe Ref)
    getRef rev = lift $ handle (handlerGetRef rev) (resolveRevision git rev)

    handlerGetRef :: Revision -> SomeException -> IO (Maybe Ref)
    handlerGetRef rev _ = do
        putStrLn $ "ERROR! Not a revision: " ++ show rev
        return Nothing

main :: IO ()
main = do
    progName <- getProgName
    options <- execParser $ opts progName
    withCurrentRepo $ \git -> runReaderT (diff git) options

opts :: String -> ParserInfo Options
opts progName = info (helper <*> options)
    ( fullDesc
   <> progDesc ("Show differences between two branches based not on commit hash,\n" ++
                "  but on commit message. This is mostly used when you've been doing\n" ++
                "  a lot of cherry-picking between two branches (for instance,\n" ++
                "  latest-develop branch and some-old-revision branch) and you don't\n" ++
                "  know which commits you've cherry-picked and which you haven't.")
   <> header (progName ++ " - diff two branches based on commit message" )
    )
 where
    options :: Parser Options
    options = Options
        <$> nullOption
            ( long "pretty"
           <> metavar "<format>"
           <> value MediumPretty
           <> reader parsePretty
           <> showDefault
           <> showDefaultWith showDefaultPretty
           <> completeWith (map showDefaultPretty [OneLinePretty, ShortPretty, MediumPretty])
           <> help "Change output format just like git-log. options: oneline, short, medium" )
        <*> flag NoPatch Patch
            ( long "patch"
           <> short 'p'
           <> help "Whether to show the changed files in the commit." )
        <*> argument str (metavar "COMMIT_A")
        <*> argument str (metavar "COMMIT_B")

    showDefaultPretty :: Pretty -> String
    showDefaultPretty OneLinePretty = "oneline"
    showDefaultPretty ShortPretty = "short"
    showDefaultPretty MediumPretty = "medium"

    parsePretty :: String -> Either ParseError Pretty
    parsePretty "oneline" = Right OneLinePretty
    parsePretty "short" = Right ShortPretty
    parsePretty "medium" = Right MediumPretty
    parsePretty _ = Left $ ErrorMsg "ERROR! In --pretty=<format>, <format> must be one of \"oneline\", \"short\", or \"medium\""


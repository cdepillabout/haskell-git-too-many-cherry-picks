module Helpers
    ( mapList
    , commitTime
    )
    where


import Data.Git (Commit, commitAuthor, personTime)
import Data.Git.Types (toUTCTime)
import Data.Time.Clock (UTCTime)


mapList :: ([a] -> b -> [a]) -> [a] -> [b] -> [a]
mapList _ baseList [] =  baseList
mapList f baseList (c:cs) = mapList f (f baseList c) cs

-- | Take a 'Commit' object and returns it\'s author\'s 'personTime'.
commitTime :: Commit -> UTCTime
commitTime commit = toUTCTime $ personTime $ commitAuthor commit

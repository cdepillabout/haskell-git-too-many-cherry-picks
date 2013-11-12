
module MergeBase
    ( getMergeBase
    )
    where


import Data.Git (Commit, commitParents, getCommit)
import Data.Git.Storage (Git)
import Data.List (insertBy)
import Helpers (mapList, commitTime)



data ParentInfo = Parent1 | Parent2 | Both deriving (Show, Eq)


-- | Insert a 'Commit' and 'ParentInfo' tuple into a list of 'Commit' and
-- 'ParentInfo' tuples.  It is inserted by the the commit's time.
insertByTime :: (Commit, ParentInfo) -> [(Commit, ParentInfo)] -> [(Commit, ParentInfo)]
insertByTime commitAndInfo commitList = insertBy compareCommitByTime commitAndInfo commitList
  where
    compareCommitByTime :: (Commit, ParentInfo) -> (Commit, ParentInfo) -> Ordering
    compareCommitByTime (commitA, _) (commitB, _) =
        let commitATime = commitTime commitA
            commitBTime = commitTime commitB
        in
            compare commitBTime commitATime

-- | Takes a list of '(Commit, ParentInfo)', a 'ParentInfo', and a 'Commit'.
-- Searches for @parent@ in the list, and when it finds it, it adds it's
-- 'ParentInfo' to the existing commit\'s 'ParentInfo'.  For instance, if
-- the existing commit from the list\'s 'ParentInfo' is 'Parent1', and the
-- additional @parent@ passed in is 'Parent2', then the commit from the list
-- will be modified to have it's 'ParentInfo' set to 'Both', and the updated
-- list will be returned.
--
-- If @parent@ doesn't exist in the list, then it will be added to the list
-- with it's 'ParentInfo' set to the passed in 'ParentInfo'.
insertParentOrModify :: ParentInfo -> [(Commit, ParentInfo)] -> Commit -> [(Commit, ParentInfo)]
insertParentOrModify newParentInfo commits parent =
    if parentInCommits commits
        then modifyParentInCommits commits
        else insertByTime (parent, newParentInfo) commits
  where
    -- Return true if 'Commit' exists in a tuple of 'Commit' and 'ParentInfo'.
    parentInCommits :: [(Commit, ParentInfo)] -> Bool
    parentInCommits [] = False
    parentInCommits ((commit, _) : remainingCommits) = (parent == commit) || parentInCommits remainingCommits

    -- Look for @parent@ in a list of tuples of 'Commit' and 'ParentInfo'.  When found, update
    -- it's 'ParentInfo' with 'updateParentInfo' and return the new list.  If not found, then
    -- raise an error.
    modifyParentInCommits :: [(Commit, ParentInfo)] -> [(Commit, ParentInfo)]
    modifyParentInCommits [] = error "Could not find the commit in the list.  Something is horribly wrong..."
    modifyParentInCommits ((commit, parentInfo) : remainingCommits)
        | parent == commit = (commit, updateParentInfo parentInfo newParentInfo) : remainingCommits
        | otherwise = (commit, parentInfo) : modifyParentInCommits remainingCommits

    -- Take in two 'ParentInfo' and return the same 'ParentInfo' if they are the same, or
    -- 'Both' in any other case.
    updateParentInfo :: ParentInfo -> ParentInfo -> ParentInfo
    updateParentInfo Parent1 Parent1 = Parent1
    updateParentInfo Parent2 Parent2 = Parent2
    updateParentInfo _ _ = Both

-- | Take two 'Ref's and return their merge-base.  This is the same thing that should be
-- returned if you use the command \`git merge-base\`.
getMergeBase :: Git -> Commit -> Commit -> IO (Maybe Commit)
getMergeBase git commitA commitB = do
    let commitList = insertByTime (commitA, Parent1) $ insertByTime (commitB, Parent2) []
    getMergeBase' commitList
 where
    -- Breadth-first search on commits and their parents.  Uses a similar algorithm to
    -- the algorithm used by the git "merge-base" command.
    getMergeBase' :: [(Commit, ParentInfo)] -> IO (Maybe Commit)
    getMergeBase' [] = return Nothing
    getMergeBase' ((commit,parentInfo) : commits) = if parentInfo == Both then return (Just commit) else do
        let parentRefs = commitParents commit
        parentCommits <- mapM (getCommit git) parentRefs
        let commitsAndParents = mapList (insertParentOrModify parentInfo) commits parentCommits
        getMergeBase' commitsAndParents


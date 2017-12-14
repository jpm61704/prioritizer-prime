module TaskList where

import           Data.Maybe
import           Task

-- | a collection of tasks over which most of the program's main operations are applied
newtype TaskList = TaskList { unTaskList :: [Task] }

-- | generate a daily list from a general task list
-- this is a regular tasks list containing tasks with their daily targets.
generateDailyList :: Today -> TaskList -> TaskList
generateDailyList today (TaskList tl) = TaskList $ mapMaybe (dailyTask today) tl

-- | makes a daily task with due date of today if the task needs to be work on today
-- otherwise returns nothing
dailyTask :: Today -> Task -> Maybe Task
dailyTask today t@(Task desc _ _ _ _)
  | daily_pomo > 0 = Just $ task desc today today 0 daily_pomo
  | otherwise      = Nothing
  where daily_pomo = pomodorosPerDay today t







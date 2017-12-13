module Task where
{-# LANGUAGE OverloadedStrings #-}

import             Control.Exception
import  qualified  Data.Text as T
import             Data.Time

-- * Task


-- | The main tasks datatype
data Task = Task {
  getDescription        :: Description, -- ^ The descriptor for the task
  getDueDate            :: UTCTime,    -- ^ the date the task is due
  getStartedDate        :: UTCTime,    -- ^ the date the task was started
  getPomodorosCompleted :: Pomodoros,  -- ^ the number of pomodoros completed so far
  getPomodorosEstimated :: Pomodoros   -- ^ the estimated number of pomodoros to complete the task
                 }

instance Show Task where
  show = T.unpack . getDescription

instance Eq Task where
  (==) (Task desc1 due1 _ _ _) (Task desc2 due2 _ _ _) = (desc1 == desc2) && (due1 == due2)

-- | tyoe synonym for description text
type Description = T.Text

-- | type synonym for UTCTime representing the current datetime
type Today = UTCTime

-- | smart constructor for Task
-- checks that due date is after start date
task :: Description -> UTCTime -> UTCTime -> Pomodoros -> Pomodoros -> Task
task desc due start comp est = assert (due > start) $ Task desc due start comp est

-- ** Task Information

-- | get the number of calendar days until something is due
-- this does not factor in hours until something is due, treating the due date as a calendar date only
daysRemaining :: Task -> Today -> Integer
daysRemaining (Task _ (UTCTime due_day _) _ _ _) (UTCTime today _) = diffDays due_day today

-- | the number of pomodoros to complete until the estimated completion is met
pomodorosLeft :: Task -> Integer
pomodorosLeft (Task _ _ _ c e) = e - c




-- * Pomodoros

-- | this datatype is used to keep track of progress towards completing a task
type Pomodoros = Integer







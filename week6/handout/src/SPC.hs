{-# LANGUAGE InstanceSigs #-}

module SPC
  ( -- * SPC startup
    SPC,
    startSPC,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    newChan,
    threadDelay,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void)
import Data.List (partition)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)],
    spcJobCounter :: JobId,
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcWaiting :: [(JobId, ReplyChan (Maybe JobDoneReason))]
  }

-- Then the definition of the glorious SPC.
newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \s -> pure (x, s)
  (<*>) :: SPCM (a -> b) -> SPCM a -> SPCM b
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \s -> do
    (a, s') <- m s
    let SPCM f' = f a
    f' s'

-- instance Functor SPCM where
--   fmap f (SPCM g) = SPCM $ \s -> do
--     (a, s') <- g s
--     pure (f a, s')
-- \| Retrieve the state.
get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

-- | Overwrite the state.
put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

runSPCM :: SPCState -> SPCM a -> IO a
runSPCM s (SPCM f) = fst <$> f s

-- Messages sent to SPC.
data SPCMsg
  = MsgJobAdd Job (ReplyChan JobId)
  | MsgJobStatus JobId (ReplyChan JobStatus)
  | MsgJobCancel JobId
  | MsgJobWait JobId (ReplyChan (Maybe JobDoneReason))

-- | A Handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  | DoneUnknown
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  | JobUnknown
  deriving (Eq, Ord, Show)

-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | Query the job status.
jobStatus :: SPC -> JobId -> IO JobStatus
jobStatus (SPC c) jobid = do
  requestReply c $ MsgJobStatus jobid

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  sendTo c $ MsgJobCancel jobid

-- | Synchronously block until job is done and return the reason.
-- Returns 'Nothing' if job is not known to this SPC instance.
jobWait :: SPC -> JobId -> IO (Maybe JobDoneReason)
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

jobDone :: SPC -> JobId -> JobDoneReason -> IO ()
jobDone (SPC c) jobid reason =
  sendTo c $ MsgJobWait jobid

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $
        state
          { spcJobsPending = (spcJobCounter state, job) : spcJobsPending state,
            spcJobCounter = JobId $ succ jobid
          }
      io $ reply rsvp $ JobId jobid
    MsgJobStatus jobid rsvp -> do
      state <- get
      io $ reply rsvp $ case lookup jobid $ spcJobsPending state of
        Just _ -> JobPending
        _ -> JobUnknown
    MsgJobCancel jobid -> do
      state <- get
      case lookup jobid $ spcJobsPending state of
        Nothing -> pure ()
        Just _ ->
          put $
            state
              { spcJobsPending = removeAssoc jobid $ spcJobsPending state,
                spcJobsDone = (jobid, DoneCancelled) : spcJobsDone state
              }
    MsgJobWait jobid rsvp -> do
      state <- get
      case lookup jobid $ spcJobsDone state of
        Just reason -> do
          io $ reply rsvp $ Just reason
        Nothing ->
          put $ state {spcWaiting = (jobid, rsvp) : spcWaiting state}

-- MsgJobStatus jobId rsvp -> do
--   state <- get
--   let (pending, rest) = partition (\(jobId', _) -> jobId' == jobId) $ spcJobsPending state
--   case pending of
--     [(jid, _)] -> io $ reply rsvp JobPending
--     _ -> io $ reply rsvp JobRunning
--   put $ state {spcJobsPending = rest}

startSPC :: IO SPC
startSPC = do
  let initial_state =
        SPCState
          { spcJobsPending = [],
            spcJobCounter = JobId 0
          }
  server <- spawn $ \c -> runSPCM initial_state $ forever $ handleMsg c
  pure $ SPC server

-- startSPC = do
--   server <- spawn $ \c -> forever $ handle c
--   pure $ SPC server
--   where
--     handle c = do
--       msg <- receive c
--       case msg of
--         MsgPing rsvp ->
--           reply rsvp 1337

-- pingSPC :: SPC -> IO Int
-- pingSPC (SPC c) =
--   requestReply c MsgPing

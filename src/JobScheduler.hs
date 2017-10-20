{-|
Module      : JobScheduler

Provide a queue (implemented as a 'TChan') and a configurable number of
workers.

Use this module like so:

@
    do
        bracket (JobScheduler.init 4) (JobScheduler.shutdown 4) $ \jobQueue ->
            -- code that uses the queue
@

This ensures the resources are cleaned up.
-}

module JobScheduler where


import qualified JobParser
import           Preamble


-------------------------------------------------------------------------------
-- Data types


-- | A 'Task' is just a Maybe-wrapped 'JobParser.JobSpec'. This allows us to
-- switch on "something/nothing to do" in the workers. Receiving a 'Nothing' as
-- a 'Task' means "you're done" to the worker.
type Task = Maybe JobParser.JobSpec


-- | The Queue itself is a 'TChan' of these 'Task's. The semantics are what we
-- would expect (and want). When a 'worker' tries to read from an empty
-- 'JobQueue', that 'worker' will block until a value shows up on the 'TChan'.
type JobQueue = TChan Task


-------------------------------------------------------------------------------
-- Functions


-- | Start up a worker pool of 'k' members. Return the 'TChan' that can be
-- used to assign work to the pool.
init :: Int -> IO JobQueue
init k = do
    reportQueue <- newTChanIO
    jobQueue <- newTChanIO

    -- start the report writer worker listening on the reportQueue
    _ <- forkIO (reportWriter reportQueue)

    -- start worker threads 1 through k, all listening on the jobQueue
    forM_ [1..k] $ \workerNum ->
        forkIO (worker reportQueue jobQueue workerNum)

    -- return the jobQueue (so we can add jobs to it later)
    return jobQueue


-- | Spam 'Nothing' task into the job queue. When a worker receives a 'Nothing'
-- 'Task', it will quit.
shutdown :: Int -> JobQueue -> IO ()
shutdown k jobQueue = atomically $
    replicateM_ k (writeTChan jobQueue Nothing)


-- | Add a job to the queue specified by a 'JobParser.JobSpec'.
enqueue :: MonadIO m => JobParser.JobSpec -> JobQueue -> m ()
enqueue jobSpec jobQueue = liftIO . atomically $
    writeTChan jobQueue (Just jobSpec)


-- | Watch the report queue for messages and write them to the terminal
reportWriter :: TChan String -> IO ()
reportWriter chan =
    forever $ do
        msg <- atomically (readTChan chan)
        putStrLn msg
        hFlush stdout


-- | Run a worker. It can write to the report queue and it can read from the
-- jobQueue. It is also assigned a sequential number as a label, so we
-- know it's "worker 1," for example. The actual job is done in the 'performJob'
-- helper. This prints the job's name from the 'JobSpec' and then waits the
-- given duration in seconds. Then it prints that it's finished.
worker :: TChan String -> JobQueue -> Int -> IO ()
worker reportQueue jobQueue workerNum = loop
    where
        loop = do
            job <- atomically (readTChan jobQueue)
            case job of
                Nothing -> return ()
                Just spec  -> do
                    performJob spec reportQueue workerNum
                    loop


-- | Actually perform the specified job. In this case we just sleep for however
-- long the job specifies.
performJob :: JobParser.JobSpec -> TChan String -> Int -> IO ()
performJob (JobParser.JobSpec name duration) reportQueue workerNum = do
    let startMsg = printf "WORKER %d STARTING JOB: %s" workerNum name
        finishMsg = printf "WORKER %d FINISHED: %s" workerNum name
    atomically (writeTChan reportQueue startMsg)
    threadDelay (1000000 * duration)
    atomically (writeTChan reportQueue finishMsg)

{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan


data ThreadPool = ThreadPool
    { numThreads :: Int
    , taskQueue  :: TChan (Maybe (IO ()))
    , run        :: IO ()
    }

-- pool :: Int -> IO (IO a -> IO a)
-- pool n = do
--     semaphore <- newQSem n
--     pure $ bracket_ (waitQSem semaphore) (signalQSem semaphore)

newThreadPool :: Int -> IO ThreadPool
newThreadPool numThreads = do

    taskQueue <- atomically $ newTChan
    
    semaphore <- newQSem numThreads
    let pool = bracket_ (waitQSem semaphore) (signalQSem semaphore)
    
    let workers = map (\id -> pool $ worker id taskQueue) [1..numThreads]
    let run = foldr concurrently_ (pure ()) workers
    
    pure ThreadPool{..}


worker :: Int -> TChan (Maybe (IO ())) -> IO ()
worker n channel = loop
  where
    loop = do
        task <- atomically $ readTChan channel
        case task of

            Nothing ->
                putStrLn $ "Closing worker " ++ show n

            Just newTask -> do
                putStrLn $ "Worker " ++ show n ++ " got new job."
                newTask
                loop


execute :: ThreadPool -> IO () -> IO ()
execute ThreadPool{..} task = atomically $ writeTChan taskQueue (Just task)


shutdown :: ThreadPool -> IO ()
shutdown ThreadPool{..} = 
    replicateM_ numThreads $ 
        atomically $ writeTChan taskQueue Nothing


main :: IO ()
main = do

    pool <- newThreadPool 4

    concurrently_
        (run pool)
        (tasks pool `finally` shutdown pool)

  where
    tasks pool = do
        execute pool $ print  3
        execute pool $ print  5
        execute pool $ print  7
        execute pool $ print 11
        execute pool $ print 13
        execute pool $ print 17
        execute pool $ print 19
        execute pool $ print 23
        execute pool $ print 29
        execute pool $ print 31
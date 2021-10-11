module Main where

    import Control.Monad
    import Control.Concurrent
    import Control.Concurrent.STM


    main :: IO ()
    main = do
        counter <- atomically (newTVar 0)

        let increment = atomically $ do
                count <- readTVar counter
                writeTVar counter $! count + 1

        replicateM_ 10 (forkOS $ increment)
        result <- atomically $ readTVar counter
        print result

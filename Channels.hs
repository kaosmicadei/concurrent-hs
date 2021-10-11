{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan


data User = User
    { userId  :: Int
    , userChan :: TChan Int
    }


main :: IO ()
main = do
    broadcast <- atomically newBroadcastTChan
    chatroom broadcast


chatroom :: TChan Int -> IO ()
chatroom chan = do
    server <- mapM (atomically . addUserTo chan) [1, 2]
    run server chan


addUserTo :: TChan Int -> Int -> STM (User)
addUserTo chan n = do
    userChan <- dupTChan chan
    pure (User n userChan)


run server chan = do
    concurrently_
        (forever $ mapM_ readMessages server)
        (forever $ mapM_ (writeMessage chan) server)


readMessages :: User -> IO ()
readMessages User{..} = do
    value <- atomically $ readTChan userChan
    putStrLn $ "user " ++ show userId ++ " got: " ++ show value


writeMessage :: TChan Int -> User -> IO ()
writeMessage chan User{..} = atomically $ writeTChan chan $! userId * 10

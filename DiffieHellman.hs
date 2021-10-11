{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent.Async     (concurrently_)
import Control.Concurrent.STM       
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import System.Random

-- This is a test
main :: IO ()
main = do

    let maxPrimeNumber = 11
    alice <- newUser "Alice" maxPrimeNumber
    bob   <- newUser "Bob"   maxPrimeNumber
    
    baseNumber <- randomRIO (2, maxPrimeNumber)
    broadcast baseNumber [alice, bob]
    
    concurrently_
        (run alice bob  )
        (run bob   alice)


data User = User
    { username          :: String
    , serverPrimeNumber :: Int
    , userPrivateNumber :: TVar (Maybe Int)
    , userChannel       :: TChan Int
    }


newUser :: String -> Int -> IO User
newUser username serverPrimeNumber = do
    userPrivateNumber <- atomically $ newTVar Nothing
    userChannel <- atomically newTChan
    pure User{..}


send :: Int -> User -> IO ()
send n User{..} = atomically $ writeTChan userChannel n


broadcast :: Int -> [User] -> IO ()
broadcast n userList = mapM_ (send n) userList


run :: User -> User -> IO ()
run User{..} toUser  = do
    setUserPrivateNumber
    k0 <- gotNumber
    send k0 toUser
    key <- gotNumber
    putStrLn $ username ++ "'s secret key is " ++ show key
  where
    setUserPrivateNumber = do
        private <- randomRIO (1, serverPrimeNumber)
        atomically $ modifyTVar' userPrivateNumber $ \_ -> Just private

    gotNumber = do
        base <- atomically $ readTChan userChannel
        privateNumber <- atomically $ getPrivateNumber
        pure $! (base ^ privateNumber) `mod` serverPrimeNumber

    getPrivateNumber = do
        private <- readTVar userPrivateNumber
        case private of
            Just n  -> pure n
            Nothing -> error "No private number"

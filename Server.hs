import System.IO
import Network
import Control.Monad
import Control.Concurrent

main :: IO ()
main = do
    socket <- listenOn (PortNumber 44444)
    putStrLn $ "Listen at 44444"
    forever $ do
        (handle, host, port) <- accept socket
        putStrLn $ host ++ ": " ++ show port
        forkFinally
            (talk handle)
            (\_ -> hClose handle)


talk :: Handle -> IO ()
talk handle = do
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    hPutStrLn handle "Welcome!"
    loop
  where
    loop = do
        line <- hGetLine handle
        if line == "end" then
            hPutStrLn handle "Bye!"
        else do
            hPutStrLn handle $! show (2 * (read line :: Int))
            loop
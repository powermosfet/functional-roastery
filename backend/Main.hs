module Main where 

import Network.Wai.Handler.Warp
import System.Environment
import System.IO

import Config
import Api

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    env <- getEnvironment 
    putStr "DATABASE_URL = "
    print (lookup "DATABASE_URL" env)
    let config = fromEnvironment env 
    putStrLn "Using config:"
    print config
    let port = configServerPort config
    run port =<< mkApp config

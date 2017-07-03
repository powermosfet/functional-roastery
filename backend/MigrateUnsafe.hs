module Main where 

import Database.Persist.Sql
import System.Environment
import System.IO

import Model
import Config

main :: IO ()
main = do
    putStrLn "PERFORMING UNSAFE MIGRATION!!!!"
    putStrLn "*******************************"
    putStrLn ""
    hSetBuffering stdout LineBuffering
    env <- getEnvironment 
    putStr "DATABASE_URL = "
    print (lookup "DATABASE_URL" env)
    let config = fromEnvironment env 
    putStrLn "Using config:"
    print config
    pool <- makeDbPool config
    runSqlPool (runMigrationUnsafe migrateAll) pool

module EA.CLI.Auth.Run (
  runAuthCommand,
) where

import EA.CLI.Auth.Command (AuthCommand)

runAuthCommand :: AuthCommand -> IO ()
runAuthCommand _ = do
  putStrLn "Hello runAuthCommand"
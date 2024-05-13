module EA.CLI.Oracle.Run (
  runOracleCommand,
) where

import EA.CLI.Oracle.Command (OracleCommand)

runOracleCommand :: OracleCommand -> IO ()
runOracleCommand _ = do
  putStrLn "Hello runOracleCommand"
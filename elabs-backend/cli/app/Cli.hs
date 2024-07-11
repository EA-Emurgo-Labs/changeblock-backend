import EA.CLI.Parser (parseRootCommand, pref)
import EA.CLI.Run (runRootCommand)
import Options.Applicative (customExecParser)

main :: IO ()
main = do
  cmd <- customExecParser pref parseRootCommand
  runRootCommand cmd
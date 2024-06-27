import EA.CLI.Parser (parseRootCommand)
import EA.CLI.Run (runRootCommand)
import Options.Applicative (execParser, helper, info, progDesc)

main :: IO ()
main = do
  cmd <- execParser $ info (parseRootCommand <**> helper) $ progDesc "EA CLI"
  runRootCommand cmd
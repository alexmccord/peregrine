module Main (main) where

import Language.Build
import Options.Applicative

newtype Command
  = Build FilePath

main :: IO ()
main = do
  options <- execParser parserInfo'
  runCommand options
  where
    parserInfo' :: ParserInfo Command
    parserInfo' = info (parseCommand <**> helper) (progDesc "Main.")

parseCommand :: Parser Command
parseCommand =
  (subparser . foldMap command') commands
  where
    commands :: [(String, String, Parser Command)]
    commands =
      [ ("build", "Build something.", buildP)
      ]

    buildP = Build <$> argument str (metavar "input")

    command' :: (String, String, Parser a) -> Mod CommandFields a
    command' (name, desc, p) = command name $ info' p desc

    info' :: Parser a -> String -> ParserInfo a
    info' p desc = info (p <**> helper) (progDesc desc)

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  Build b -> build b

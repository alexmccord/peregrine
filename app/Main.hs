module Main (main) where

import Language.Build
import Options.Applicative

data Command
  = Build FilePath

main :: IO ()
main = do
  options <- execParser parserInfo'
  runCommand options

parserInfo' :: ParserInfo Command
parserInfo' = info' (parser' <**> helper) "Main."
  where
    parser' :: Parser Command
    parser' =
      (subparser . foldMap command')
        [ ("build", "Build something.", buildP)
        ]

    buildP = Build <$> argument str (metavar "")

    command' :: (String, String, Parser a) -> Mod CommandFields a
    command' (name, desc, p) = command name $ info' p desc

    info' :: Parser a -> String -> ParserInfo a
    info' p desc = info p (progDesc desc)

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  Build path -> build path

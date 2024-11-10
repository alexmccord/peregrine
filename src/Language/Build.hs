module Language.Build (build) where

import Language.Core.State

build :: FilePath -> IO ()
build _ = do
  let (lst, _) = runAnalysisState fakeAnalysis
  putStrLn (foldMap show lst)

data CoreLang
  = CLInt Int

type AnalysisState = AnalysisStateM CoreLang

fakeAnalysis :: AnalysisState [String]
fakeAnalysis = do
  id1 <- add (CLInt 5)
  id2 <- add (CLInt 7)
  id3 <- add (CLInt 10)
  return $ [show id1, show id2, show id3]

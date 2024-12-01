{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.Build (build) where

import Data.Void (Void)
import Language.Core.Analysis

build :: FilePath -> IO ()
build _ = do
  fakeAnalysis
  return ()

data Value
  = VInt Int
  | VStr String

type Components = '[Value]

type MyAnalysis = AnalysisM Void Components

fakeAnalysis :: IO (AnalysisResult Void Components)
fakeAnalysis = do
  let (res, analysis) = runAnalysis $ do
        id1 <- fresh (VInt 5)
        id2 <- fresh (VStr "a")
        id3 <- fresh (VInt 10)
        list <- doSomethingWithAnalysis id1 id2 id3
        Just (VInt i) <- retrieve id1
        Just (VStr s) <- retrieve id2
        return $ list ++ [show i, show s]

  case res of
    Just xs -> mapM_ putStrLn xs
    Nothing -> putStrLn "aw darn"

  return analysis

doSomethingWithAnalysis :: Node -> Node -> Node -> MyAnalysis [String]
doSomethingWithAnalysis a b c = do
  return [show a, show b, show c]

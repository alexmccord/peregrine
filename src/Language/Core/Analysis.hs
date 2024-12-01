{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Core.Analysis
  ( Node,
    Analysis,
    AnalysisM,
    AnalysisResult (..),
    MkCol (..),
    ColumnOps,
    retrieve,
    fresh,
    bind,
    report,
    runAnalysis,
  )
where

import Control.Monad.State (MonadState (get, put), State, gets, lift, modify, runState)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Prelude hiding (lookup)

data Column cs where
  CCons :: M.Map Node c -> Column cs -> Column (c : cs)
  CNil :: Column '[]

class MkCol cs where
  mkColumns :: Column cs

instance MkCol '[] where
  mkColumns = CNil

instance (MkCol cs) => MkCol (c : cs) where
  mkColumns = CCons M.empty mkColumns

class ColumnOps cs a where
  cget :: Column cs -> Node -> Maybe a
  cput :: Column cs -> Node -> a -> Column cs

instance {-# OVERLAPPING #-} ColumnOps (c : cs) c where
  cget (CCons m _) k = M.lookup k m
  cput (CCons m cs) k a = CCons (M.insert k a m) cs

instance (ColumnOps cs a) => ColumnOps (c : cs) a where
  cget (CCons _ cs) = cget cs
  cput (CCons m cs) k a = CCons m (cput cs k a)

data Node = Id Int
  deriving (Eq, Ord, Show)

data Analysis cs = Analysis
  { table :: Column cs,
    dirty :: Set Node,
    id :: Int
  }

insert :: (ColumnOps cs a) => Node -> a -> AnalysisM e cs ()
insert k a = modify $ \analysis -> analysis {table = cput analysis.table k a}

newtype AnalysisM e cs a = AnalysisStateM {runAnalysisStateM :: MaybeT (WriterT [e] (State (Analysis cs))) a}
  deriving stock (Functor)
  deriving newtype (Applicative, Monad, MonadFail)

instance MonadState (Analysis cs) (AnalysisM e cs) where
  get = AnalysisStateM get
  put = AnalysisStateM . put

data AnalysisResult e rec = AnalysisResult
  { errors :: [e],
    analysis :: Analysis rec
  }

nextId :: AnalysisM e cs Node
nextId = do
  analysis <- get
  modify $ \a -> a {id = a.id + 1}
  pure $ Id analysis.id

-- Given an `Id` `k`, try to retrieve the data bound by `k`. Returns `Just` iff `k` is indeed bound to `a`.
retrieve :: (ColumnOps cs a) => Node -> AnalysisM e cs (Maybe a)
retrieve k = gets (cget . table <*> pure k)

-- Returns a fresh `Id` bound to the provided `value`.
fresh :: (ColumnOps cs a) => a -> AnalysisM e cs Node
fresh value = do
  id' <- nextId
  bind id' value
  return id'

-- Ascribes `value` to the provided `k`.
bind :: (ColumnOps cs a) => Node -> a -> AnalysisM e cs ()
bind k value = do
  insert k value

-- Report any errors that may happen during analysis.
-- Strictly only about analysis, e.g. pattern matching failure in MonadFail needn't apply.
report :: e -> AnalysisM e rec ()
report e = AnalysisStateM . lift . tell $ [e]

-- Run it until saturation.
-- TODO: Saturation.
runAnalysis :: (MkCol cs) => AnalysisM e cs a -> (Maybe a, AnalysisResult e cs)
runAnalysis c = toResult $ go c initialAnalysis
  where
    go = runState . runWriterT . runMaybeT . runAnalysisStateM
    initialAnalysis = Analysis {table = mkColumns, dirty = S.empty, id = 0}
    toResult ((a, e), finalAnalysis) = (a, AnalysisResult {errors = e, analysis = finalAnalysis})

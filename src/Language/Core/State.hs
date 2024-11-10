module Language.Core.State
  ( Id,
    Analysis,
    AnalysisStateM,
    lookup,
    add,
    runAnalysisState,
  )
where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Prelude hiding (lookup)

data Id = Id Int
  deriving (Eq, Ord, Show)

data Analysis ud = Analysis
  { map :: Map Id ud,
    dirty :: Set Id,
    id :: Int
  }

lookup :: Id -> Analysis ud -> Maybe ud
lookup k analysis = M.lookup k analysis.map

newtype AnalysisStateM ud a = CoreM {runCoreM :: MaybeT (State (Analysis ud)) a}
  deriving (Functor, Applicative, Monad, MonadFail, MonadState (Analysis ud))

nextId :: AnalysisStateM ud Id
nextId = do
  analysis <- get
  let id' = Id analysis.id
  put analysis {id = analysis.id + 1}
  return id'

add :: ud -> AnalysisStateM ud Id
add value = do
  id' <- nextId
  analysis <- get
  put $ insert id' value analysis
  return id'
  where
    insert :: Id -> ud -> Analysis ud -> Analysis ud
    insert k a analysis = analysis {map = M.insert k a (analysis.map)}

runAnalysisState :: AnalysisStateM ud a -> (Maybe a, Analysis ud)
runAnalysisState c = go c initial
  where
    go = runState . runMaybeT . runCoreM
    initial = Analysis {map = M.empty, dirty = S.empty, id = 0}

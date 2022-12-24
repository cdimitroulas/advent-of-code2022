module Lib.Search where

import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS
import           Data.Heap           (MinPrioHeap)
import qualified Data.Heap           as H
-- import           Data.Maybe          (isNothing)

data DijkstraState a = DijkstraState
                         { visited   :: HashSet a
                         , distances :: HashMap a (Maybe Int)
                         , queue     :: MinPrioHeap (Maybe Int) a
                         }

initialDijkstraState :: Hashable a => a -> DijkstraState a
initialDijkstraState src = DijkstraState
  { visited = HS.empty
  , distances = HM.singleton src (Just 0)
  , queue = H.fromList [(Just 0, src)]
  }

-- TODO exercise to implement my own dijkstra algorithm and breadth-first search algorithm
-- by hand in Haskell.
-- dijkstra :: (Foldable f, Num cost, Ord cost, Ord state)
--   =>
--   (state -> f state) -- Function to generate list of neighboring states given the current state
--   -> (state -> state -> cost) -- Function to generate transition costs between neighboring states. This is only called for adjacent states, so it is safe to have this function be partial for non-neighboring states.
--   -> (state -> Bool) -- Predicate to determine if solution found. dijkstra returns the shortest path to the first state for which this predicate returns True.
--   -> state -- Initial state
--   -> Maybe (cost, [state]) -- (Total cost, list of steps) for the first path found which satisfies the given predicate
-- dijkstra getNeighbors getCost isFound initial = _
--   where
--     processQueue :: DijkstraState state -> HashMap state (Maybe Int)
--     processQueue state@DijkstraState{..} =
--       case H.view queue of
--         Nothing -> distances
--         Just ((minDist, nextNode), newQueue) ->
--           if isFound nextNode then distances else _

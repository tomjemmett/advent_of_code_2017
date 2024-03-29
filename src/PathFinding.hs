{-# OPTIONS_GHC -Wno-x-partial #-}

-- credit to https://github.com/nicuveo/advent-of-code/blob/main/2022/haskell/lib/AOC/PathFinding.hs

module PathFinding
  ( -- top level usage
    findPath
  , findPathWith
  , unsafeFindPath
  , unsafeFindPathWith
    -- internal state
  , PFState (..)
  , costSoFar
  , currentNode
  , hasFoundAnswer
    -- processing
  , mkPFState
  , pathFindingStep
  , reconstructPath
  ) where


-- imports

import Data.Function       (on)
import Data.Hashable       (Hashable)
import Data.HashMap.Strict (HashMap, (!), (!?))
import Data.HashMap.Strict qualified as M
import Data.List           qualified as L
import Data.Maybe
import Data.PQueue.Min     as Q


-- library usage

-- | Returns the shortest path from start point to end point, if
-- any. Uses the provided function to find all the neighbours of any
-- given point. Based on Dijkstra's algorithm (doesn't use a
-- heuristic).
findPath
  :: Hashable a
  => (a -> [(Int, a)])  -- ^ all edges from a given point, with cost
  -> a                  -- ^ starting point
  -> a                  -- ^ end point
  -> Maybe [(Int, a)]
findPath edges = findPathWith edges (const 0)

-- | Returns the shortest path from start point to end point, if
-- any. Uses the provided function to find all the neighbours of any
-- given point. Based on the A* algorithm, uses the provided
-- heuristic.
-- It is extremely important that the heuristic function does not
-- overestimate the cost of the rest of the path, otherwise this
-- function might return a sub-obtimal path.
findPathWith
  :: Hashable a
  => (a -> [(Int, a)]) -- ^ all edges from a given point, with cost
  -> (a -> Int)        -- ^ heuristic function
  -> a                 -- ^ starting point
  -> a                 -- ^ end point
  -> Maybe [(Int, a)]
findPathWith edges heuristic start end = reconstructPath
  $ until hasFoundAnswer pathFindingStep
  $ mkPFState edges heuristic start end

-- | Like 'findPath', but errors if no path is found.
unsafeFindPath
  :: Hashable a
  => (a -> [(Int, a)])
  -> a
  -> a
  -> [(Int, a)]
unsafeFindPath edges = unsafeFindPathWith edges (const 0)

-- | Like 'findPathWith', but errors if no path is found.
unsafeFindPathWith
  :: Hashable a
  => (a -> [(Int, a)])
  -> (a -> Int)
  -> a
  -> a
  -> [(Int, a)]
unsafeFindPathWith edges heuristic start end =
  fromMaybe (error "findPath: no path found") $
  findPathWith edges heuristic start end


-- path finder's internal state

-- | Internal state of the pathfinder.
data PFState a = PFState
  { pfEdges     :: a -> [(Int, a)]
    -- ^ outward edges with cost
  , pfHeuristic :: a -> Int
    -- ^ heuristic function
  , pfStart     :: a
    -- ^ start point
  , pfEnd       :: a
    -- ^ target point
  , pfQueue     :: Q.MinQueue (Cell a)
    -- ^ internal queue of cells
  , pfNodeInfo  :: HashMap a (a, Int)
    -- ^ cell's parent & cost
  }

-- | Initializes the pathfinder's state.
mkPFState
  :: (Hashable a)
  => (a -> [(Int, a)]) -- ^ edges function
  -> (a -> Int)        -- ^ heuristic
  -> a                 -- ^ start
  -> a                 -- ^ end
  -> PFState a
mkPFState edges heuristic start end =
  PFState edges heuristic start end queue nodeMap
  where
    queue = Q.singleton $ Cell (heuristic start) 0 start
    nodeMap = M.singleton start (start, 0)

-- | Gets the best node to be processed.
currentNode :: PFState a -> a
currentNode = cellNode . Q.findMin . pfQueue

-- | Extracts the cost of reaching the given point, at that point in
-- the search.
costSoFar
  :: Hashable a
  => PFState a  -- ^ the pathfinder's state
  -> a          -- ^ the node
  -> Maybe Int
costSoFar state x = snd <$> pfNodeInfo state !? x

-- | Returns whether we found a path, or a lack thereof. In practice,
-- that means that there are no more points to process.
hasFoundAnswer :: PFState a -> Bool
hasFoundAnswer = Q.null . pfQueue

-- | Returns whether we found an actual path.
hasFoundPath :: Hashable a => PFState a -> Bool
hasFoundPath s = hasFoundAnswer s && pfEnd s `M.member` pfNodeInfo s


-- cell type

-- | Internal representation of a candidate in our priority queue.
data Cell a = Cell
  { cellEstimatedTotalCost :: Int
  , cellCostSoFar          :: Int
  , cellNode               :: a
  }
  deriving (Eq)

-- | We sort cells by how likely they are to yield the shortest path.
-- First, we sort by the estimated total cost, which is the cost so
-- far + the heuristic; in case of equality, we consider the further
-- point from the start.
instance Eq a => Ord (Cell a) where
  compare = compare `on` \Cell{..} -> (cellEstimatedTotalCost, -cellCostSoFar)

instance Show a => Show (Cell a) where
  show (Cell e c a) = concat
    [ "{"
    , show e
    , ", "
    , show c
    , ", "
    , show a
    , "}"
    ]


-- path finding functions

-- | Performs one step of the computation: takes the most likely
-- candidate, and if it isn't the goal, add its neighbours to the
-- queue.
pathFindingStep :: Hashable a => PFState a -> PFState a
pathFindingStep !s
  | hasFoundAnswer s = s
  | otherwise        =
    let
      (candidate@Cell{..}, newQueue) = Q.deleteFindMin $ pfQueue s
    in
      -- have we reached the end?
      if cellNode == pfEnd s
      then
        -- we clear the queue to indicate we're done
        s { pfQueue = Q.empty }
      else
        let
          newState = s { pfQueue = newQueue }
          edges = pfEdges s cellNode
        in
          -- we process each edge, then clean the beginning of the queue
          skipToNextValidCandidate $ L.foldl' (insertEdge candidate) newState edges

-- | Reconstruct a path from a successful state
reconstructPath :: Hashable a => PFState a -> Maybe [(Int, a)]
reconstructPath state
  | hasFoundPath state = Just $ go (pfEnd state) []
  | otherwise = Nothing
  where
    go cell path
      | cell == pfStart state = newPath
      | otherwise             = go parent newPath
      where
        (parent, cost) = pfNodeInfo state ! cell
        newPath = (cost, cell) : path


-- internal helpers

-- | Inserts a candidate cell in the queue, if needed.
insertEdge
  :: Hashable a
  => Cell a      -- ^ cell being considered
  -> PFState a   -- ^ current state
  -> (Int, a)    -- ^ edge to process
  -> PFState a
insertEdge Cell{..} state (distance, neighbour) =
  case costSoFar state neighbour of
    -- we haven't seen this neighbour before
    Nothing -> stateWithNeighbour
    Just oldNeighbourCost
      -- we have, but the new cost is lower
      | newNeighbourCost < oldNeighbourCost -> stateWithNeighbour
      -- the cost is not lower, ignore the neighbour
      | otherwise -> state
  where
    stateWithNeighbour = state
      { pfQueue = Q.insert neighbourCell $ pfQueue state
      , pfNodeInfo = M.insert neighbour (cellNode, newNeighbourCost) $ pfNodeInfo state
      }
    newNeighbourCost = cellCostSoFar + distance
    neighbourCell = Cell
      (newNeighbourCost + pfHeuristic state neighbour)
      newNeighbourCost
      neighbour

-- | Clears all elements at the beginning of the queue that are
-- obsolete / irrelevant.
skipToNextValidCandidate
  :: Hashable a
  => PFState a
  -> PFState a
skipToNextValidCandidate state =
  state { pfQueue = Q.dropWhile shouldIgnore $ pfQueue state }
  where
    -- we ignore a cell if its cost so far is greater than the one
    -- registered in the nodeInfo map of the state
    shouldIgnore Cell{..} = cellCostSoFar > fromJust (costSoFar state cellNode)
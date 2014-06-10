module QTree (
    QTree(..),
    QLeaf(..),
    empty,
    insert
) where

import Urza.Math.Rectangle
import Urza.Types
import Data.Maybe
import Debug.Trace

type Extents a = (QTree a, QTree a, QTree a, QTree a)

data QTree a = QTree { _qtBounds   :: BoundingBox
                     , _qtBranches :: Maybe (Extents a)
                     , _qtLeaves   :: [QLeaf a]
                     } deriving (Show)

data QLeaf a = QLeaf { _qlBounds :: BoundingBox
                     , _qlEntry  :: a
                     } deriving (Show)


insert :: BoundingBox -> a -> QTree a -> QTree a
insert bb a q = fromMaybe q $ tryInsert bb a q

empty :: BoundingBox -> QTree a
empty = emptyBranch

tryInsert :: BoundingBox -> a -> QTree a -> Maybe (QTree a)
tryInsert bb1 a (QTree bb2 Nothing ls)
    | bb2 `containsRect` bb1 = tryInsert bb1 a $ QTree bb2 (Just $ boundsToQuads bb2) ls
    | otherwise = Nothing
tryInsert bb1 a (QTree bb2 (Just bs) ls)
    | bb2 `containsRect` bb1 = Just $ case tryInsertingInBranches bb1 a bs of
                                   Nothing -> QTree bb2 (Just bs) (leaf bb1 a:ls)
                                   mBs     -> QTree bb2 mBs ls
    | otherwise = Nothing

tryInsertingInBranches :: BoundingBox -> a -> Extents a -> Maybe (Extents a)
tryInsertingInBranches bb a (q1,q2,q3,q4) =
    case catMaybes mQuads of
        [] -> Nothing
        _  -> Just (q1',q2',q3',q4')
      where quads = [q1,q2,q3,q4]
            mQuads = map (tryInsert bb a) quads
            [q1',q2',q3',q4'] = zipWith fromMaybe quads mQuads

boundsToQuads :: BoundingBox -> (QTree a, QTree a, QTree a, QTree a)
boundsToQuads bb = (q1',q2',q3',q4')
    where [q1',q2',q3',q4'] = map emptyBranch [q1,q2,q3,q4]
          (q1,q2,q3,q4) = quadrants bb

leaf :: BoundingBox -> a -> QLeaf a
leaf bb a = QLeaf bb a

emptyBranch :: BoundingBox -> QTree a
emptyBranch bb = QTree bb Nothing []

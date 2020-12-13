{-# LANGUAGE NamedFieldPuns #-}
module CountSeq
  ( CountSeq
  , toList
  , fromList
  , shiftInR  
  , lookupCount
  ) where

import Protolude hiding (toList)
import qualified Protolude

import qualified Data.IntMap.Strict as IntMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))

import Util (tally)

data CountSeq = CountSeq
  { _size :: Int
  , _counts :: IntMap Int
  , _seq :: Seq Int }

-------- INTERNAL FUNCTIONS --------

addToCounts :: Int -> IntMap Int -> IntMap Int
addToCounts = IntMap.alter go
 where 
  go :: Maybe Int -> Maybe Int
  go Nothing = Just 1
  go (Just a) = Just (a + 1)

removeFromCounts :: HasCallStack => Int -> IntMap Int -> IntMap Int
removeFromCounts = IntMap.alter go
 where
  go :: HasCallStack => Maybe Int -> Maybe Int
  go Nothing = panic "This shouldn't be possible"
  go (Just 1) = Nothing
  go (Just a) = Just (a - 1)

-------- PUBLIC FUNCTIONS --------

fromList :: NonEmpty Int -> CountSeq
fromList xs = CountSeq
  { _size = NonEmpty.length xs
  , _counts = tally (NonEmpty.toList xs)
  , _seq = Seq.fromList (NonEmpty.toList xs) }

toList :: CountSeq -> [Int]
toList = Protolude.toList . _seq

shiftInR :: CountSeq -> Int -> CountSeq
shiftInR cseq@CountSeq{_counts,_seq} a = cseq
  { _counts = addToCounts a (removeFromCounts (Seq.index _seq 0) _counts)
  , _seq = Seq.deleteAt 0 _seq |> a }

lookupCount :: Int -> CountSeq -> Int
lookupCount a = IntMap.findWithDefault 0 a . _counts
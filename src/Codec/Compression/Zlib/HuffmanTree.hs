module Codec.Compression.Zlib.HuffmanTree (
  HuffmanTree,
  AdvanceResult (..),
  createHuffmanTree,
  advanceTree,
) where

import Data.Bits (testBit)
import Data.Word (Word8)

data HuffmanTree a
  = HuffmanNode (HuffmanTree a) (HuffmanTree a)
  | HuffmanValue a
  | HuffmanEmpty
  deriving (Show)

data AdvanceResult a
  = AdvanceError String
  | NewTree (HuffmanTree a)
  | Result a

emptyHuffmanTree :: HuffmanTree a
emptyHuffmanTree = HuffmanEmpty

createHuffmanTree ::
  Show a =>
  [(a, Int, Int)] ->
  Either String (HuffmanTree a)
createHuffmanTree = foldr addHuffmanNode' (Right emptyHuffmanTree)
 where
  addHuffmanNode' (a, b, c) acc =
    case acc of
      Left err -> Left err
      Right tree -> addHuffmanNode a b c tree

addHuffmanNode ::
  Show a =>
  a ->
  Int ->
  Int ->
  HuffmanTree a ->
  Either String (HuffmanTree a)
addHuffmanNode val len code node =
  case node of
    HuffmanEmpty
      | len == 0 ->
        Right (HuffmanValue val)
    HuffmanEmpty ->
      case addHuffmanNode val (len - 1) code HuffmanEmpty of
        Left err -> Left err
        Right newNode
          | testBit code (len - 1) -> Right (HuffmanNode HuffmanEmpty newNode)
          | otherwise -> Right (HuffmanNode newNode HuffmanEmpty)
    --
    HuffmanValue _
      | len == 0 ->
        Left "Two values point to the same place!"
    HuffmanValue _ ->
      Left "HuffmanValue hit while inserting a value!"
    --
    HuffmanNode _ _
      | len == 0 ->
        Left ("Tried to add where the leaf is a node: " ++ show val)
    HuffmanNode l r | testBit code (len - 1) ->
      case addHuffmanNode val (len - 1) code r of
        Left err -> Left err
        Right r' -> Right (HuffmanNode l r')
    HuffmanNode l r ->
      case addHuffmanNode val (len - 1) code l of
        Left err -> Left err
        Right l' -> Right (HuffmanNode l' r)

advanceTree :: Word8 -> HuffmanTree a -> AdvanceResult a
advanceTree x node =
  case node of
    HuffmanEmpty -> AdvanceError "Tried to advance empty tree!"
    HuffmanValue _ -> AdvanceError "Tried to advance value!"
    HuffmanNode l r ->
      case if (x == 1) then r else l of
        HuffmanEmpty -> AdvanceError "Advanced to empty tree!"
        HuffmanValue y -> Result y
        t -> NewTree t
{-# INLINE advanceTree #-}

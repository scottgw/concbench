{-# LANGUAGE BangPatterns #-}
module Cache where

import Control.Monad

import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as UMV

innerSize :: Int
innerSize = 512

bigMult :: Int
bigMult = 1024

type Memory = MV.IOVector (UMV.IOVector Int)

friendlyAlloc :: Int -> IO Memory
friendlyAlloc size = do
  smallVector <- UMV.replicate size (1 :: Int)
  bigVector <- MV.replicate (bigMult*size) smallVector
  print (MV.length bigVector)
  return bigVector

unfriendlyAlloc :: Int -> IO Memory
unfriendlyAlloc size = do
  bigVector <- MV.new (bigMult*size)
  forM_ [0 .. (bigMult*size - 1)] $ \i ->
      UMV.replicate size i >>= MV.unsafeWrite bigVector i
  print (MV.length bigVector)
  return bigVector

friendlyAlloc' :: IO Memory
friendlyAlloc' = friendlyAlloc innerSize

unfriendlyAlloc' :: IO Memory
unfriendlyAlloc' = unfriendlyAlloc innerSize

memTask :: Memory -> IO ()
memTask mem =
    let subTask v = 
            forM_ [0 .. UMV.length v - 1] $ \ i ->
                UMV.unsafeRead v i >>= UMV.unsafeWrite v i . (*2)
    in forM_ [0 .. MV.length mem - 1] $ \ i ->
        MV.unsafeRead mem i >>= subTask
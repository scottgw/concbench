{-# LANGUAGE BangPatterns #-}

import Control.Monad

import Data.IORef
import Data.List (foldl')

import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as UMV

import           System.TimeIt

import           System.Environment

bigMult = 1024

type MemType = MV.IOVector (UMV.IOVector Int)

friendlyAlloc :: Int -> IO MemType
friendlyAlloc size = do
  smallVector <- UMV.replicate size (1 :: Int)
  bigVector <- MV.replicate (bigMult*size) smallVector
  print (MV.length bigVector)
  return bigVector

unfriendlyAlloc :: Int -> IO MemType
unfriendlyAlloc size = do
  bigVector <- MV.new (bigMult*size)
  forM_ [0 .. (bigMult*size - 1)] $ \i ->
      UMV.replicate size i >>= MV.unsafeWrite bigVector i
  print (MV.length bigVector)
  return bigVector

memTask mem =
    let subTask v = 
            forM_ [0 .. UMV.length v - 1] $ \ i ->
                UMV.unsafeRead v i >>= UMV.unsafeWrite v i . (*2)
    in forM_ [0 .. MV.length mem - 1] $ \ i ->
        MV.unsafeRead mem i >>= subTask

main = do
  args : _ <- getArgs
  !mem <- case args of
            "f" -> friendlyAlloc 512
            _ -> unfriendlyAlloc 512
  (time, !a) <- timeItT (memTask mem)
  print time
  return  ()

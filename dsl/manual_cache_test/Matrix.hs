{-# LANGUAGE BangPatterns #-}

import           Control.Applicative
import           Control.Monad

import qualified Data.Vector.Unboxed.Mutable as V
import           Data.Vector.Unboxed.Mutable (Unbox, IOVector)

import           System.Environment

data Matrix a = 
    Matrix { m_width  :: Int
           , m_height :: Int
           , m_vec    :: V.IOVector a
           }

initMatrix :: Unbox a => Int -> Int -> a -> IO (Matrix a)
initMatrix w h v = 
    Matrix w h <$> V.replicate (w*h) v

idxOf m x y = x + y * m_width m

get :: Unbox a => Matrix a -> Int -> Int -> IO a
get m x y = V.unsafeRead (m_vec m) (idxOf m x y)

put m x y = V.unsafeWrite (m_vec m) (idxOf m x y)

dbl_outer_y :: (Num a, Unbox a) => Matrix a -> IO ()
dbl_outer_y a = outer 0
    where
      outer !y
          | y >= m_height a = return ()
          | otherwise = inner y 0 >> outer (y+1)
      inner y !x
          | x >= m_width a = return ()
          | otherwise = 
              do
                v <- get a x y
                put a x y (v*2)
                inner y (x+1)

dbl_outer_x :: (Num a, Unbox a) => Matrix a -> IO ()
dbl_outer_x a = outer 0
    where
      outer !x
          | x >= m_width a = return ()
          | otherwise = inner x 0 >> outer (x+1)
      inner x !y
          | y >= m_height a = return ()
          | otherwise = 
              do
                v <- get a x y
                put a x y (v*2)
                inner x (y+1)

main = do
  [str] <- getArgs
  m <- initMatrix 10000 10000 (0::Int)
  if str == "local"
    then dbl_outer_y m
    else dbl_outer_x m 
  return ()

-- matrixMult :: (Num a, Unbox a) 
--               => Matrix a
--            -> Matrix a
--            -> IO (Matrix a)
-- matrixMult a b = 
--     where
--       mm c = 
--           forM_ [0 .. m_width a] $ do
--             forM_ [0 .. m_height b] $ do
--               forM_ [0 .. 
                                        

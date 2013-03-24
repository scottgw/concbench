{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module FlowHask where

import Control.Parallel

import Flow

-- instance IdxCategory IdxFun where
--   idIdx = IdxFun id
--   compIdx (IdxFun f) (IdxFun g) = IdxFun $ f . g
  
-- instance IdxArrow (->) where
--   f &&& g = \x -> 
--     let b = f x 
--         c = g x
--     in b `par` c `seq` (b,c)
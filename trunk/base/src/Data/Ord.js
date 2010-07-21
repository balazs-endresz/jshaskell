/// <reference path="../../../jshaskell/src/Haskell/Main.js" local/>
/// <reference path="../../../base/src/GHC/Base.js" local />

//{-# OPTIONS_GHC -XNoImplicitPrelude #-}
//-----------------------------------------------------------------------------
//-- |
//-- Module      :  Data.Ord
//-- Copyright   :  (c) The University of Glasgow 2005
//-- License     :  BSD-style (see the file libraries/base/LICENSE)
//-- 
//-- Maintainer  :  libraries@haskell.org
//-- Stability   :  stable
//-- Portability :  portable
//--
//-- Orderings
//--
//-----------------------------------------------------------------------------

//module Data.Ord (
//   Ord(..),
//   Ordering(..),
//   comparing,
// ) where

//#if __GLASGOW_HASKELL__
//import GHC.Base
//#endif

//-- | 
//-- > comparing p x y = compare (p x) (p y)
//--
//-- Useful combinator for use in conjunction with the @xxxBy@ family
//-- of functions from "Data.List", for example:
//--
//-- >   ... sortBy (comparing fst) ...
//comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
//comparing p x y = compare (p x) (p y)
function comparing(p, x, y){
    return Ord.compare(p(x), p(y));
}

namespace("Data_Ord", {
     Ord        : Ord
    ,Ordering   : Ordering
    ,comparing  : comparing
})
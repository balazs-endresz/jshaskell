/// <reference path="../../../jshaskell/src/Haskell/Main.js" local/>
/// <reference path="../../../base/src/GHC/Base.js" local />


//-----------------------------------------------------------------------------
//-- |
//-- Module      :  Data.Function
//-- Copyright   :  Nils Anders Danielsson 2006
//-- License     :  BSD-style (see the LICENSE file in the distribution)
//--
//-- Maintainer  :  libraries@haskell.org
//-- Stability   :  experimental
//-- Portability :  portable
//--
//-- Simple combinators working solely on and with functions.

//module Data.Function
//  ( -- * "Prelude" re-exports
//    id, const, (.), flip, ($)
//    -- * Other combinators
//  , fix
//  , on
//  ) where

//import Prelude

//infixl 0 `on`

//-- | @'fix' f@ is the least fixed point of the function @f@,
//-- i.e. the least defined @x@ such that @f x = x@.
//fix :: (a -> a) -> a
//fix f = let x = f x in x

function fix_(f) {
    return function () { return f(fix_(f)) }
}
/*
//since it's not lazy this `fix` function doesn't terminate:
function fix_original(f){ return f(fix_original(f)) }

//so we have to use an expanded version:
function fix_(f){ 
    return function(){ return f(fix_(f)) }
}

//and call it first to acces the actual one:
function fact_rhs(getRec){
    return function(n){
        return n == 1 ? 1 : (n * getRec()(n-1))
    }
}
//where
//getRec   == fix_ (f)
//getRec() == fix_ (f) ()
//getRec() == fix_original (f)

//an extra call is need here too:
var fact = fix_(fact_rhs)();

fact(4);
*/

//the Y-combmiator
function fix(f){
    return  (function(g){ return g(g) })
            (function(h){
                return function(){ return f(h(h)).apply(null, arguments) }
            });
}
/*
function fact_rhs(rec){
return function(n){
return n == 1 ? 1 : (n * rec(n-1))
}
}

var fact = fix(fact_rhs);

fact(4);

*/

//#region proofs
//-- | @(*) \`on\` f = \\x y -> f x * f y@.
//--
//-- Typical usage: @'Data.List.sortBy' ('compare' \`on\` 'fst')@.
//--
//-- Algebraic properties:
//--
//-- * @(*) \`on\` 'id' = (*)@ (if @(*) &#x2209; {&#x22a5;, 'const' &#x22a5;}@)
//--
//-- * @((*) \`on\` f) \`on\` g = (*) \`on\` (f . g)@
//--
//-- * @'flip' on f . 'flip' on g = 'flip' on (g . f)@

//-- Proofs (so that I don't have to edit the test-suite):

//--   (*) `on` id
//-- =
//--   \x y -> id x * id y
//-- =
//--   \x y -> x * y
//-- = { If (*) /= _|_ or const _|_. }
//--   (*)

//--   (*) `on` f `on` g
//-- =
//--   ((*) `on` f) `on` g
//-- =
//--   \x y -> ((*) `on` f) (g x) (g y)
//-- =
//--   \x y -> (\x y -> f x * f y) (g x) (g y)
//-- =
//--   \x y -> f (g x) * f (g y)
//-- =
//--   \x y -> (f . g) x * (f . g) y
//-- =
//--   (*) `on` (f . g)
//-- =
//--   (*) `on` f . g

//--   flip on f . flip on g
//-- =
//--   (\h (*) -> (*) `on` h) f . (\h (*) -> (*) `on` h) g
//-- =
//--   (\(*) -> (*) `on` f) . (\(*) -> (*) `on` g)
//-- =
//--   \(*) -> (*) `on` g `on` f
//-- = { See above. }
//--   \(*) -> (*) `on` g . f
//-- =
//--   (\h (*) -> (*) `on` h) (g . f)
//-- =
//--   flip on (g . f)
//#endregion


//on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
//(.*.) `on` f = \x y -> f x .*. f y
function on(op, f){
    return function(x, y){
        return op(f(x), f(y));
    }
}

namespace("Data_Function", {
    //-- * "Prelude" re-exports
     id     : id 
    ,const_ : const_
    //TODO: , (.)
    ,flip   : flip
    //TODO: , ($)

    //-- * Other combinators
    ,fix_   : fix_
    ,fix    : fix
    ,on     : on
})
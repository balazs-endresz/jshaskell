/// <reference path="../../../jshaskell/src/Haskell.js" local />
/// <reference path="Ordering.js" local />

var a = "a";

//{-# OPTIONS_GHC -XNoImplicitPrelude #-}
//{-# OPTIONS_HADDOCK hide #-}
//-----------------------------------------------------------------------------
//-- |
//-- Module      :  GHC.Classes
//-- Copyright   :  (c) The University of Glasgow, 1992-2002
//-- License     :  see libraries/base/LICENSE
//--
//-- Maintainer  :  cvs-ghc@haskell.org
//-- Stability   :  internal
//-- Portability :  non-portable (GHC extensions)
//--
//-- Basic classes.
//--
//-----------------------------------------------------------------------------

//module GHC.Classes where

//import GHC.Bool
//import GHC.Ordering

//TODO:
//infix  4  ==, /=, <, <=, >=, >
//infixr 3  &&
//infixr 2  ||

//default ()              -- Double isn't available yet

//-- | The 'Eq' class defines equality ('==') and inequality ('/=').
//-- All the basic datatypes exported by the "Prelude" are instances of 'Eq',
//-- and 'Eq' may be derived for any datatype whose constituents are also
//-- instances of 'Eq'.
//--
//-- Minimal complete definition: either '==' or '/='.
//--
//class  Eq a  where
//    (==), (/=)           :: a -> a -> Bool

//    x /= y               = not (x == y)
//    x == y               = not (x /= y)
var Eq = typeclass("Eq", a)
    .types({
        eq: [a, a, Boolean],
        ne: [a, a, Boolean]
    })
    .impl(function(inst){ return {
        eq: function (a, b) { return !inst.ne(a, b) },
        ne: function (a, b) { return !inst.eq(a, b) }
    }});


//-- | The 'Ord' class is used for totally ordered datatypes.
//--
//-- Instances of 'Ord' can be derived for any user-defined
//-- datatype whose constituent types are in 'Ord'.  The declared order
//-- of the constructors in the data declaration determines the ordering
//-- in derived 'Ord' instances.  The 'Ordering' datatype allows a single
//-- comparison to determine the precise ordering of two objects.
//--
//-- Minimal complete definition: either 'compare' or '<='.
//-- Using 'compare' can be more efficient for complex types.
//--
//class  (Eq a) => Ord a  where
//    compare              :: a -> a -> Ordering
//    (<), (<=), (>), (>=) :: a -> a -> Bool
//    max, min             :: a -> a -> a

//    compare x y = if x == y then EQ
//                  -- NB: must be '<=' not '<' to validate the
//                  -- above claim about the minimal things that
//                  -- can be defined for an instance of Ord:
//                  else if x <= y then LT
//                  else GT

//    x <  y = case compare x y of { LT -> True;  _ -> False }
//    x <= y = case compare x y of { GT -> False; _ -> True }
//    x >  y = case compare x y of { GT -> True;  _ -> False }
//    x >= y = case compare x y of { LT -> False; _ -> True }

//        -- These two default methods use '<=' rather than 'compare'
//        -- because the latter is often more expensive
//    max x y = if x <= y then y else x
//    min x y = if x <= y then x else y

var Ord = typeclass("Ord", a)
    .context(Eq)
    .types({
        compare : [a, a, Ordering],
        "<"     : [a, a, Boolean],
        "<="    : [a, a, Boolean],
        ">"     : [a, a, Boolean],
        ">="    : [a, a, Boolean],
        max     : [a, a, a],
        min     : [a, a, a]
    })
    .impl(function(inst){ 
      var eq = getInstance(Eq, inst._type).eq;
      return {
        compare: function (x, y) {
            return  eq(x, y)         ? Ordering.EQ :
                    inst["<="](x, y) ? Ordering.LT :
                                       Ordering.GT ;
        },
        "<"  : function (x, y) { return inst.compare(x, y).LT ? true  : false },
        "<=" : function (x, y) { return inst.compare(x, y).GT ? false :  true },
        ">"  : function (x, y) { return inst.compare(x, y).GT ? true  : false },
        ">=" : function (x, y) { return inst.compare(x, y).LT ? false :  true },
        max  : function (x, y) { return inst["<="](x, y) ? y : x },
        min  : function (x, y) { return inst["<="](x, y) ? x : y }
    }});

//-- OK, so they're technically not part of a class...:

//-- Boolean functions

//-- | Boolean \"and\"
//(&&)                    :: Bool -> Bool -> Bool
//True  && x              =  x
//False && _              =  False
function andOp(a, b){
    return (a === true)  ? b :
           (a === false) ? false : error(and);
}

//-- | Boolean \"or\"
//(||)                    :: Bool -> Bool -> Bool
//True  || _              =  True
//False || x              =  x
function orOp(a, b){
    return (a === true)  ? true :
           (a === false) ? b : error(or);
}

//-- | Boolean \"not\"
//not                     :: Bool -> Bool
//not True                =  False
//not False               =  True
function not(a){
    return (a === true)  ? false :
           (a === false) ? true  : error(not);
}


namespace("GHC_Classes", {
     Eq    : Eq 
    ,Ord   : Ord
    ,andOp : andOp
    ,orOp  : orOp
    ,not   : not
})
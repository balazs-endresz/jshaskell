/// <reference path="../../../jshaskell/src/Haskell.js" local />
/// <reference path="../../../base/src/GHC/Maybe.js" local />


//{-# OPTIONS_GHC -XNoImplicitPrelude #-}
//-----------------------------------------------------------------------------
//-- |
//-- Module      :  Data.Maybe
//-- Copyright   :  (c) The University of Glasgow 2001
//-- License     :  BSD-style (see the file libraries/base/LICENSE)
//-- 
//-- Maintainer  :  libraries@haskell.org
//-- Stability   :  stable
//-- Portability :  portable
//--
//-- The Maybe type, and associated operations.
//--
//-----------------------------------------------------------------------------

//module Data.Maybe
//   (
//     Maybe(Nothing,Just)-- instance of: Eq, Ord, Show, Read,
//                        --              Functor, Monad, MonadPlus

//   , maybe              -- :: b -> (a -> b) -> Maybe a -> b

//   , isJust             -- :: Maybe a -> Bool
//   , isNothing          -- :: Maybe a -> Bool
//   , fromJust           -- :: Maybe a -> a
//   , fromMaybe          -- :: a -> Maybe a -> a
//   , listToMaybe        -- :: [a] -> Maybe a
//   , maybeToList        -- :: Maybe a -> [a]
//   , catMaybes          -- :: [Maybe a] -> [a]
//   , mapMaybe           -- :: (a -> Maybe b) -> [a] -> [b]
//   ) where

//#ifdef __GLASGOW_HASKELL__
//import GHC.Base
//#endif

//#ifdef __NHC__
//import Prelude
//import Prelude (Maybe(..), maybe)
//import Maybe
//    ( isJust
//    , isNothing
//    , fromJust
//    , fromMaybe
//    , listToMaybe
//    , maybeToList
//    , catMaybes
//    , mapMaybe
//    )
//#else

//#ifndef __HUGS__
//-- ---------------------------------------------------------------------------
//-- The Maybe type, and instances

//-- | The 'Maybe' type encapsulates an optional value.  A value of type
//-- @'Maybe' a@ either contains a value of type @a@ (represented as @'Just' a@), 
//-- or it is empty (represented as 'Nothing').  Using 'Maybe' is a good way to 
//-- deal with errors or exceptional cases without resorting to drastic
//-- measures such as 'error'.
//--
//-- The 'Maybe' type is also a monad.  It is a simple kind of error
//-- monad, where all errors are represented by 'Nothing'.  A richer
//-- error monad can be built using the 'Data.Either.Either' type.

//data  Maybe a  =  Nothing | Just a
//  deriving (Eq, Ord)

//declared in GHC.Maybe
//function Maybe(){}
//data(Maybe, [["Just", "a"], "Nothing"]);

//instance  Functor Maybe  where
//    fmap _ Nothing       = Nothing
//    fmap f (Just a)      = Just (f a)
instance(Functor, Maybe, {
    fmap: function(f, m){
        return  m.Nothing ? Maybe.Nothing : 
                m.Just ? Maybe.Just(f(m[0])) : error("Functor.Maybe.fmap")
    }
})

//instance  Monad Maybe  where
//    (Just x) >>= k      = k x
//    Nothing  >>= _      = Nothing

//    (Just _) >>  k      = k
//    Nothing  >>  _      = Nothing

//    return              = Just
//    fail _              = Nothing
instance(Monad, Maybe, {
    ">>=" : function(m, k){
        return  m.Just    ? k(m[0]) :
                m.Nothing ? Maybe.Nothing :
                error("Monad.Maybe.>>=");
    },
    ">>" : function(m, k){
        return  m.Just    ? k :
                m.Nothing ? Maybe.Nothing :
                error("Monad.Maybe.>>");
    },
    return_ : Maybe.Just,
    fail    : Maybe.Nothing
})

//-- ---------------------------------------------------------------------------
//-- Functions over Maybe

//-- | The 'maybe' function takes a default value, a function, and a 'Maybe'
//-- value.  If the 'Maybe' value is 'Nothing', the function returns the
//-- default value.  Otherwise, it applies the function to the value inside
//-- the 'Just' and returns the result.
//maybe :: b -> (a -> b) -> Maybe a -> b
//maybe n _ Nothing  = n
//maybe _ f (Just x) = f x
//#endif  /* __HUGS__ */
function maybe(n, f, m){
    if(m.Nothing)
        return n;
    if(m.Just)
        return f(m[0]);
}

//-- | The 'isJust' function returns 'True' iff its argument is of the
//-- form @Just _@.
//isJust         :: Maybe a -> Bool
//isJust Nothing = False
//isJust _       = True
function isJust(a){
    return !!a.Just;
}

//-- | The 'isNothing' function returns 'True' iff its argument is 'Nothing'.
//isNothing         :: Maybe a -> Bool
//isNothing Nothing = True
//isNothing _       = False
function isNothing(a){
    return !!a.Nothing;
}

//-- | The 'fromJust' function extracts the element out of a 'Just' and
//-- throws an error if its argument is 'Nothing'.
//fromJust          :: Maybe a -> a
//fromJust Nothing  = error "Maybe.fromJust: Nothing" -- yuck
//fromJust (Just x) = x
function fromJust(a){
    return a.Nothing ? error("Maybe.fromJust: Nothing") : 
           a.Just ? a[0] : error();
}

//-- | The 'fromMaybe' function takes a default value and and 'Maybe'
//-- value.  If the 'Maybe' is 'Nothing', it returns the default values;
//-- otherwise, it returns the value contained in the 'Maybe'.
//fromMaybe     :: a -> Maybe a -> a
//fromMaybe d x = case x of {Nothing -> d;Just v  -> v}
function fromMaybe(d, x){
    return x.Nothing ? d : x.Just ? x[0] : error(fromMaybe);
}

//-- | The 'maybeToList' function returns an empty list when given
//-- 'Nothing' or a singleton list when not given 'Nothing'.
//maybeToList            :: Maybe a -> [a]
//maybeToList  Nothing   = []
//maybeToList  (Just x)  = [x]
function maybeToList(m){
    return m.Nothing ? [] : m.Just ? [m[0]] : error(maybeToList);
}

//-- | The 'listToMaybe' function returns 'Nothing' on an empty list
//-- or @'Just' a@ where @a@ is the first element of the list.
//listToMaybe           :: [a] -> Maybe a
//listToMaybe []        =  Nothing
//listToMaybe (a:_)     =  Just a
function listToMaybe(l){
    if(!l.length)
        return emptyListOf(l);
    return Maybe.Just(l[0]);
}

//-- | The 'catMaybes' function takes a list of 'Maybe's and returns
//-- a list of all the 'Just' values. 
//catMaybes              :: [Maybe a] -> [a]
//catMaybes ls = [x | Just x <- ls]
function catMaybes(ls){
    var acc = [];
    for(var i = 0, l = ls.length; i < l; ++i)
        if(ls[i].Just)
            acc.push(ls[i][0]);
    return acc;
}

//-- | The 'mapMaybe' function is a version of 'map' which can throw
//-- out elements.  In particular, the functional argument returns
//-- something of type @'Maybe' b@.  If this is 'Nothing', no element
//-- is added on to the result list.  If it just @'Just' b@, then @b@ is
//-- included in the result list.
//mapMaybe          :: (a -> Maybe b) -> [a] -> [b]
//mapMaybe _ []     = []
//mapMaybe f (x:xs) =
// let rs = mapMaybe f xs in
// case f x of
//  Nothing -> rs
//  Just r  -> r:rs
function mapMaybe(f, xs){
    var a = uncons(xs),
        rs = mapMaybe(f, a.tail),
        fx = f(a.head);
    return  fx.Nothing ? rs :
            fx.Just    ? cons(r, rs) :
            error(mapMaybe);
}

//#endif /* else not __NHC__ */


namespace("Data_Maybe", {
     Maybe       : Maybe        //instance of: Eq, Ord, Show, Read, 
                                //        Functor, Monad, MonadPlus
                            
    ,maybe       : maybe        //:: b -> (a -> b) -> Maybe a -> b
                            
    ,isJust      : isJust       //:: Maybe a -> Bool
    ,isNothing   : isNothing    //:: Maybe a -> Bool
    ,fromJust    : fromJust     //:: Maybe a -> a
    ,fromMaybe   : fromMaybe    //:: a -> Maybe a -> a
    ,listToMaybe : listToMaybe  //:: [a] -> Maybe a
    ,maybeToList : maybeToList  //:: Maybe a -> [a]
    ,catMaybes   : catMaybes    //:: [Maybe a] -> [a]
    ,mapMaybe    : mapMaybe     //:: (a -> Maybe b) -> [a] -> [b]
})
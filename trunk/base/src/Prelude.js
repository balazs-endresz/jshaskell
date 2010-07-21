/// <reference path="../../jshaskell/src/Haskell.js" local />
/// <reference path="Data/Either.js" local />
/// <reference path="Data/Tuple.js" local />
/// <reference path="Data/List.js" local />
/// <reference path="Data/Maybe.js" local />
/// <reference path="GHC/Base.js" local />
/// <reference path="GHC/Show.js" local />



/// <reference path="Control/Monad.js" local />
/// <reference path="Data/.js" local />
/// <reference path="GHC/.js" local />

//import Text.Read
//import GHC.Enum
//import GHC.Num
//import GHC.Real
//import GHC.Float
//import GHC.Show
//import GHC.Err   ( undefined )



//{-# OPTIONS_GHC -XNoImplicitPrelude #-}
//-----------------------------------------------------------------------------
//-- |
//-- Module      :  Prelude
//-- Copyright   :  (c) The University of Glasgow 2001
//-- License     :  BSD-style (see the file libraries/base/LICENSE)
//-- 
//-- Maintainer  :  libraries@haskell.org
//-- Stability   :  stable
//-- Portability :  portable
//--
//-- The Prelude: a standard module imported by default into all Haskell
//-- modules.  For more documentation, see the Haskell 98 Report
//-- <http://www.haskell.org/onlinereport/>.
//--
//-----------------------------------------------------------------------------

//module Prelude (

//    -- * Standard types, classes and related functions

//    -- ** Basic data types
//    Bool(False, True),
//    (&&), (||), not, otherwise,

//    Maybe(Nothing, Just),
//    maybe,

//    Either(Left, Right),
//    either,

//    Ordering(LT, EQ, GT),
//    Char, String,

//    -- *** Tuples
//    fst, snd, curry, uncurry,

//#if defined(__NHC__)
//    []((:), []),        -- Not legal Haskell 98;
//                        -- ... available through built-in syntax
//    module Data.Tuple,  -- Includes tuple types
//    ()(..),             -- Not legal Haskell 98
//    (->),               -- ... available through built-in syntax
//#endif
//#ifdef __HUGS__
//    (:),                -- Not legal Haskell 98
//#endif

//    -- ** Basic type classes
//    Eq((==), (/=)),
//    Ord(compare, (<), (<=), (>=), (>), max, min),
//    Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen,
//         enumFromTo, enumFromThenTo),
//    Bounded(minBound, maxBound),

//    -- ** Numbers

//    -- *** Numeric types
//    Int, Integer, Float, Double,
//    Rational,

//    -- *** Numeric type classes
//    Num((+), (-), (*), negate, abs, signum, fromInteger),
//    Real(toRational),
//    Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
//    Fractional((/), recip, fromRational),
//    Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
//             asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
//    RealFrac(properFraction, truncate, round, ceiling, floor),
//    RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
//              encodeFloat, exponent, significand, scaleFloat, isNaN,
//              isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2),

//    -- *** Numeric functions
//    subtract, even, odd, gcd, lcm, (^), (^^),
//    fromIntegral, realToFrac,

//    -- ** Monads and functors
//    Monad((>>=), (>>), return, fail),
//    Functor(fmap),
//    mapM, mapM_, sequence, sequence_, (=<<),

//    -- ** Miscellaneous functions
//    id, const, (.), flip, ($), until,
//    asTypeOf, error, undefined,
//    seq, ($!),

//    -- * List operations
//    map, (++), filter,
//    head, last, tail, init, null, length, (!!),
//    reverse,
//    -- ** Reducing lists (folds)
//    foldl, foldl1, foldr, foldr1,
//    -- *** Special folds
//    and, or, any, all,
//    sum, product,
//    concat, concatMap,
//    maximum, minimum,
//    -- ** Building lists
//    -- *** Scans
//    scanl, scanl1, scanr, scanr1,
//    -- *** Infinite lists
//    iterate, repeat, replicate, cycle,
//    -- ** Sublists
//    take, drop, splitAt, takeWhile, dropWhile, span, break,
//    -- ** Searching lists
//    elem, notElem, lookup,
//    -- ** Zipping and unzipping lists
//    zip, zip3, zipWith, zipWith3, unzip, unzip3,
//    -- ** Functions on strings
//    lines, words, unlines, unwords,

//    -- * Converting to and from @String@
//    -- ** Converting to @String@
//    ShowS,
//    Show(showsPrec, showList, show),
//    shows,
//    showChar, showString, showParen,
//    -- ** Converting from @String@
//    ReadS,
//    Read(readsPrec, readList),
//    reads, readParen, read, lex,

//    -- * Basic Input and output
//    IO,
//    -- ** Simple I\/O operations
//    -- All I/O functions defined here are character oriented.  The
//    -- treatment of the newline character will vary on different systems.
//    -- For example, two characters of input, return and linefeed, may
//    -- read as a single newline character.  These functions cannot be
//    -- used portably for binary I/O.
//    -- *** Output functions
//    putChar,
//    putStr, putStrLn, print,
//    -- *** Input functions
//    getChar,
//    getLine, getContents, interact,
//    -- *** Files
//    FilePath,
//    readFile, writeFile, appendFile, readIO, readLn,
//    -- ** Exception handling in the I\/O monad
//    IOError, ioError, userError, catch

//  ) where

//#ifndef __HUGS__
//import Control.Monad
//import System.IO
//import System.IO.Error
//import Data.List
//import Data.Either
//import Data.Maybe
//import Data.Tuple
//#endif

//#ifdef __GLASGOW_HASKELL__
//import GHC.Base
//-- import GHC.IO
//-- import GHC.IO.Exception
//import Text.Read
//import GHC.Enum
//import GHC.Num
//import GHC.Real
//import GHC.Float
//import GHC.Show
//import GHC.Err   ( undefined )
//#endif

//#ifdef __HUGS__
//import Hugs.Prelude
//#endif

//#ifndef __HUGS__
//infixr 0 $!

//-- -----------------------------------------------------------------------------
//-- Miscellaneous functions

//-- | Strict (call-by-value) application, defined in terms of 'seq'.
//($!)    :: (a -> b) -> a -> b
//f $! x  = x `seq` f x
//#endif

//#ifdef __HADDOCK__
//-- | The value of @'seq' a b@ is bottom if @a@ is bottom, and otherwise
//-- equal to @b@.  'seq' is usually introduced to improve performance by
//-- avoiding unneeded laziness.
//seq :: a -> b -> b
//seq _ y = y
//#endif

namespace("Prelude", {
    //non-standard functions
     cons       : cons
    ,consJoin   : consJoin
    ,uncons     : uncons
    ,elemIndex      : elemIndex
    ,unsafeCompare  : unsafeCompare

    ,readHex        : readHex
    ,readOct        : readOct
    ,round          : round
    ,toInteger      : toInteger
    ,fromInteger    : fromInteger
    ,fromIntegral   : fromIntegral


    //-- * Standard types, classes and related functions

    //-- ** Basic data types
    ,Bool       : Bool
    //,(&&)
    ,andOp      : andOp
    //,(||)
    ,orOp       : orOp
    ,not        : not
    ,otherwise  : otherwise

    ,Maybe      : Maybe
    ,maybe      : maybe

    ,Either     : Either
    ,either     : either

    ,Ordering   : Ordering
    //,Char
    ,String     : String

    //-- *** Tuples
    ,fst        : fst
    ,snd        : snd
    ,curry      : curry
    ,curry_     : curry_
    ,uncurry    : uncurry

    //#if defined(__NHC__)
    //    []((:), []),        -- Not legal Haskell 98;
    //                        -- ... available through built-in syntax
    //    module Data.Tuple,  -- Includes tuple types
    //    ()(..),             -- Not legal Haskell 98
    //    (->),               -- ... available through built-in syntax
    //#endif
    //#ifdef __HUGS__
    //    (:),                -- Not legal Haskell 98
    //#endif



    //-- ** Basic type classes
    ,Eq         : Eq
    ,Ord        : Ord
    
    //TODO!!!
    //,Enum       : Enum
    //,Bounded    : Bounded

    //-- ** Numbers

    //-- *** Numeric types
    //,Int
    //,Integer
    //,Float
    //,Double
    //,Rational

    //-- *** Numeric type classes
    //Num((+), (-), (*), negate, abs, signum, fromInteger),
    //Real(toRational),
    //Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
    //Fractional((/), recip, fromRational),
    //Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
    //         asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
    //RealFrac(properFraction, truncate, round, ceiling, floor),
    //RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
    //          encodeFloat, exponent, significand, scaleFloat, isNaN,
    //          isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2),

    //-- *** Numeric functions

    //TODO!!!
//    ,subtract   : subtract
//    ,even       : even
//    ,odd        : odd
//    ,gcd        : gcd
//    ,lcm        : lcm

    //,(^)
    //,(^^)
    ,fromIntegral : fromIntegral
    //,realToFrac

    //-- ** Monads and functors
    ,Monad      : Monad
    ,Functor    : Functor
    //,mapM
    //,mapM_
    //,sequence
    //,sequence_
    //,(=<<)

    //-- ** Miscellaneous functions
    ,id         : id
    ,const_     : const_
    //,(.)
    ,compose    : compose
    ,compose1   : compose1
    ,flip       : flip
    //,($)
    ,call       : call
    ,until      : until
    ,asTypeOf   : asTypeOf
    ,error      : error
    //,undefined
    //,seq
    //,($!)

    //-- * List operations
    ,map        : map
    //,(++)
    ,append     : append
    ,filter     : filter
    ,head       : head
    ,last       : last
    ,tail       : tail
    ,init       : init
    ,null_      : null_
    ,length     : length
    //,(!!)
    ,index      : index
    ,reverse    : reverse

    //-- ** Reducing lists (folds)
    ,foldl      : foldl
    ,foldl1     : foldl1
    ,foldr      : foldr
    ,foldr1     : foldr1

    //-- *** Special folds
    ,and        : and
    ,or         : or
    ,any        : any
    ,all        : all
    ,sum        : sum
    ,product    : product
    ,concat     : concat
    ,concatMap  : concatMap
    ,maximum    : maximum
    ,minimum    : minimum

    //-- ** Building lists

    //-- *** Scans
    ,scanl      : scanl
    ,scanl1     : scanl1
    ,scanr      : scanr
    ,scanr1     : scanr1

    //-- *** Infinite lists
    //,iterate
    //,repeat
    ,replicate  : replicate
    //,cycle

    //-- ** Sublists
    ,take       : take
    ,drop       : drop
    ,splitAt    : splitAt
    ,takeWhile  : takeWhile
    ,dropWhile  : dropWhile
    ,span       : span
    ,break_     : break_

    //-- ** Searching lists
    ,elem       : elem
    ,notElem    : notElem
    ,lookup     : lookup

    //-- ** Zipping and unzipping lists
    ,zip        : zip
    ,zip3       : zip3
    ,zipWith    : zipWith
    ,zipWith3   : zipWith3
    ,unzip      : unzip
    ,unzip3     : unzip3

    //-- ** Functions on strings
    ,lines      : lines
    ,words      : words
    ,unlines    : unlines
    ,unwords    : unwords

    //-- * Converting to and from @String@

    //-- ** Converting to @String@
    //,ShowS
    ,Show       : Show
    //,shows
    //,showChar
    //,showString
    //,showParen

    //-- ** Converting from @String@
    //,ReadS
    //,Read
    //,reads
    //,readParen
    //,read
    //,lex

    //-- * Basic Input and output
    //,IO

    //-- ** Simple I\/O operations
    //-- All I/O functions defined here are character oriented.  The
    //-- treatment of the newline character will vary on different systems.
    //-- For example, two characters of input, return and linefeed, may
    //-- read as a single newline character.  These functions cannot be
    //-- used portably for binary I/O.

    //-- *** Output functions
    //,putChar
    //,putStr
    //,putStrLn
    //,print

    //-- *** Input functions
    //,getChar
    //,getLine
    //,getContents
    //,interact

    //-- *** Files
    //,FilePath
    //,readFile
    //,writeFile
    //,appendFile
    //,readIO
    //,readLn

    //-- ** Exception handling in the I\/O monad
    //,IOError
    //,ioError
    //,userError
    //,catch_
});
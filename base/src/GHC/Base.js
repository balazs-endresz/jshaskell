/// <reference path="../../../jshaskell/src/Haskell.js" local />
/// <reference path="Unit.js" local />
/// <reference path="Tuple.js" local />
/// <reference path="Ordering.js" local />
/// <reference path="Classes.js" local />


//GHC.Show is not imported

// -------------------------------------------------
// The base package
// -------------------------------------------------


//\section[GHC.Base]{Module @GHC.Base@}

//#region Structure

//The overall structure of the GHC Prelude is a bit tricky.

//  a) We want to avoid "orphan modules", i.e. ones with instance
//        decls that don't belong either to a tycon or a class
//        defined in the same module

//  b) We want to avoid giant modules

//So the rough structure is as follows, in (linearised) dependency order


//GHC.Prim                Has no implementation.  It defines built-in things, and
//                by importing it you bring them into scope.
//                The source file is GHC.Prim.hi-boot, which is just
//                copied to make GHC.Prim.hi

//GHC.Base        Classes: Eq, Ord, Functor, Monad
//                Types:   list, (), Int, Bool, Ordering, Char, String

//Data.Tuple      Types: tuples, plus instances for GHC.Base classes

//GHC.Show        Class: Show, plus instances for GHC.Base/GHC.Tup types

//GHC.Enum        Class: Enum,  plus instances for GHC.Base/GHC.Tup types

//Data.Maybe      Type: Maybe, plus instances for GHC.Base classes

//GHC.List        List functions

//GHC.Num         Class: Num, plus instances for Int
//                Type:  Integer, plus instances for all classes so far (Eq, Ord, Num, Show)

//                Integer is needed here because it is mentioned in the signature
//                of 'fromInteger' in class Num

//GHC.Real        Classes: Real, Integral, Fractional, RealFrac
//                         plus instances for Int, Integer
//                Types:  Ratio, Rational
//                        plus intances for classes so far

//                Rational is needed here because it is mentioned in the signature
//                of 'toRational' in class Real

//GHC.ST  The ST monad, instances and a few helper functions

//Ix              Classes: Ix, plus instances for Int, Bool, Char, Integer, Ordering, tuples

//GHC.Arr         Types: Array, MutableArray, MutableVar

//                Arrays are used by a function in GHC.Float

//GHC.Float       Classes: Floating, RealFloat
//                Types:   Float, Double, plus instances of all classes so far

//                This module contains everything to do with floating point.
//                It is a big module (900 lines)
//                With a bit of luck, many modules can be compiled without ever reading GHC.Float.hi

//#endregion

//Other Prelude modules are much easier with fewer complex dependencies.

//\begin{code}
//{-# OPTIONS_GHC -XNoImplicitPrelude #-}
//{-# OPTIONS_GHC -fno-warn-orphans #-}
//{-# OPTIONS_HADDOCK hide #-}
//-----------------------------------------------------------------------------
//-- |
//-- Module      :  GHC.Base
//-- Copyright   :  (c) The University of Glasgow, 1992-2002
//-- License     :  see libraries/base/LICENSE
//-- 
//-- Maintainer  :  cvs-ghc@haskell.org
//-- Stability   :  internal
//-- Portability :  non-portable (GHC extensions)
//--
//-- Basic data types and classes.
//-- 
//-----------------------------------------------------------------------------

//#include "MachDeps.h"

//-- #hide
//module GHC.Base
//        (
//        module GHC.Base,
//        module GHC.Bool,
//        module GHC.Classes,
//        module GHC.Generics,
//        module GHC.Ordering,
//        module GHC.Types,
//        module GHC.Prim,        -- Re-export GHC.Prim and GHC.Err, to avoid lots
//        module GHC.Err          -- of people having to import it explicitly
//  ) 
//        where

//import GHC.Types
//import GHC.Bool
//import GHC.Classes
//import GHC.Generics
//import GHC.Ordering
//import GHC.Prim
//import {-# SOURCE #-} GHC.Show
//import {-# SOURCE #-} GHC.Err
//import {-# SOURCE #-} GHC.IO (failIO)

//-- These two are not strictly speaking required by this module, but they are
//-- implicit dependencies whenever () or tuples are mentioned, so adding them
//-- as imports here helps to get the dependencies right in the new build system.
//import GHC.Tuple ()
//import GHC.Unit ()

//infixr 9  .
//infixr 5  ++
//infixl 4  <$
//infixl 1  >>, >>=
//infixr 0  $

//default ()              -- Double isn't available yet
//\end{code}



//{- | The 'Functor' class is used for types that can be mapped over.
//Instances of 'Functor' should satisfy the following laws:

//> fmap id  ==  id
//> fmap (f . g)  ==  fmap f . fmap g

//The instances of 'Functor' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
//defined in the "Prelude" satisfy these laws.
//-}

//class  Functor f  where
//    fmap        :: (a -> b) -> f a -> f b

//    -- | Replace all locations in the input with the same value.
//    -- The default definition is @'fmap' . 'const'@, but this may be
//    -- overridden with a more efficient version.
//    (<$)        :: a -> f b -> f a
//    (<$)        =  fmap . const

var Functor = typeclass("Functor", "f")
    .types({
        fmap: [Function, "f", "f"],
        "<$": ["a", "f", "f"]
    })
    .impl(function(inst){ return {
        "<$": function (a, b) { return inst.fmap(const_(a), b) }
    }})


//{- | The 'Monad' class defines the basic operations over a /monad/,
//a concept from a branch of mathematics known as /category theory/.
//From the perspective of a Haskell programmer, however, it is best to
//think of a monad as an /abstract datatype/ of actions.
//Haskell's @do@ expressions provide a convenient syntax for writing
//monadic expressions.

//Minimal complete definition: '>>=' and 'return'.

//Instances of 'Monad' should satisfy the following laws:

//> return a >>= k  ==  k a
//> m >>= return  ==  m
//> m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h

//Instances of both 'Monad' and 'Functor' should additionally satisfy the law:

//> fmap f xs  ==  xs >>= return . f

//The instances of 'Monad' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
//defined in the "Prelude" satisfy these laws.
//-}

//class  Monad m  where
//    -- | Sequentially compose two actions, passing any value produced
//    -- by the first as an argument to the second.
//    (>>=)       :: forall a b. m a -> (a -> m b) -> m b
//    -- | Sequentially compose two actions, discarding any value produced
//    -- by the first, like sequencing operators (such as the semicolon)
//    -- in imperative languages.
//    (>>)        :: forall a b. m a -> m b -> m b
//        -- Explicit for-alls so that we know what order to
//        -- give type arguments when desugaring

//    -- | Inject a value into the monadic type.
//    return      :: a -> m a
//    -- | Fail with a message.  This operation is not part of the
//    -- mathematical definition of a monad, but is invoked on pattern-match
//    -- failure in a @do@ expression.
//    fail        :: String -> m a

//    m >> k      = m >>= \_ -> k
//    fail s      = error s

var Monad = typeclass("Monad", "m")
    .types({
        ">>="     : ["m", Function, "m"],
        ">>"      : ["m", "m", "m"],
        return_   : ["a", "m"],
        fail      : [String, "m"],
        
        do_ : VARARG,
        do$ : VARARG,
        //TODO
        run : VARARG
    })    
    .impl(function(inst){ return {
        ">>": function(m, k){
            var monad = inst[">>="](m, const_(k));
            monad.constructor = inst._type;
            return monad;
        },
        fail: function(s){ return error(s) },

        //this doesn't have to be modified
        do_ : function(m1, m2, m3 /* ... */){
            var args = arguments,
                monadBind = inst[">>"];

            function monad(outerScope /* ... */){
                var scope = new Scope(outerScope), //see: Haskell/Do.js
                    i = 1,
                    l = args.length,
                    result = args[0];

                for(; i < l; ++i)
                    result = monadBind(result, args[i]);
                
                //add the new scope
                var newArgs = _slice.call(arguments, 1);
                newArgs.unshift(scope);

                return result.apply(null, newArgs);
            }
            
            monad.constructor = inst._type

            return monad;
        },
        
        //#region comment
        //this doesn't have to be modified
        //
        //`run` creates a new scope object, which should be the first argument to every monad,
        //also, it might transform the arguments, e.g. create a ParseState object from the supplied string
        //
        //  var ParserMonad = getInstance(Monad, Parser);
        //  var parser = ParserMonad.do$("a" ,"<-", anyChar)(ret, "a");
        //  ParserMonad.run(parser, "aa", function(result){ alert(result) });
        //
        //Here the the parser recieved a string and a callback function, since it's asynchronous
        //but normally `run` simply returns the result. Without `run` it would be much verbose:
        //  trampoline(parser(new Scope(), new ParseState("aa"), function(result){ alert(result) }))
        //#endregion
        run : function(m /* args */){
            var args = slice(arguments, 1);
            args.unshift(new Scope());
            return m.apply(null, args);
        },

        //Callstream interface for the do notation - inspired by: http://dbj.org/dbj/?p=514
        //TODO
        //this shouldn't be modified, because the code might change in the future,
        //though using explicit arguments instead of monad.apply can speed it up a bit
        do$ : createDo(inst)

    }})



//-- do explicitly: deriving (Eq, Ord)
//-- to avoid weird names like con2tag_[]#

//instance (Eq a) => Eq [a] where
//    {-# SPECIALISE instance Eq [Char] #-}
//    []     == []     = True
//    (x:xs) == (y:ys) = x == y && xs == ys
//    _xs    == _ys    = False

//instance (Ord a) => Ord [a] where
//    {-# SPECIALISE instance Ord [Char] #-}
//    compare []     []     = EQ
//    compare []     (_:_)  = LT
//    compare (_:_)  []     = GT
//    compare (x:xs) (y:ys) = case compare x y of
//                                EQ    -> compare xs ys
//                                other -> other

//instance Functor [] where
//    fmap = map

//instance  Monad []  where
//    m >>= k             = foldr ((++) . k) [] m
//    m >> k              = foldr ((++) . (\ _ -> k)) [] m
//    return x            = [x]
//    fail _              = []

//TODO: Functor and Monad inst. for Array and String


//A few list functions that appear here because they are used here.
//The rest of the prelude list functions are in GHC.List.

function emptyListOf(xs){
    return xs.charAt ? "" : [];
}

function cons(x, xs){
    if(x.charAt && xs.charAt)
        return x + xs;
    return [x].concat(xs);
}

//TODO: remove
function consJoin(x, xs){
    if(x.charAt && xs.charAt)
        return x + xs;
    return x + xs.join("");
}


function uncons(a){
    var isStr = a.charAt,
        head = isStr ? a.charAt(0) : a[0],
        tail = isStr ? a.substr(1) : slice(a, 1),
        res = [head, tail];
    
    res.head = head;
    res.tail = tail;
    res.constructor = Tuple.Tuple2;
    return res;
}

var augment = cons;
//build g = g (:) []
function build(g){
    return error("Use cons(g, emptyListOf(g))");
}

function map(f, xs){
    var isStr = xs.charAt,
        res = [], i = 0, l = xs.length;
    for(; i < l; ++i)
        res[i] = f(isStr ? xs.charAt(i) : xs[i]);
    return res;
}

function foldr(f, z, xs) {
    var isStr = xs.charAt,
        i = xs.length - 1;
    for(; i > -1 ; --i) 
        z = f(isStr ? xs.charAt(i) : xs[i], z);
    return z;
}


function append(xs, ys){
    var xsIsStr = xs.charAt,
        ysIsStr = ys.charAt;

    //if at least one is string
    if(xsIsStr)
        return xs.concat(ysIsStr ? ys : ys.join(""));
    if(ysIsStr)
        return (xsIsStr ? xs : xs.join("")).concat(ys);

    //if both are arrays
    return xs.concat(ys);
}

//-- |The 'Bool' type is an enumeration.  It is defined with 'False'
//-- first so that the corresponding 'Prelude.Enum' instance will give
//-- 'Prelude.fromEnum' 'False' the value zero, and
//-- 'Prelude.fromEnum' 'True' the value 1.
//-- The actual definition is in the ghc-prim package.

//-- XXX These don't work:
//-- deriving instance Eq Bool
//-- deriving instance Ord Bool
//-- <wired into compiler>:
//--     Illegal binding of built-in syntax: con2tag_Bool#

//instance Eq Bool where
//    True  == True  = True
//    False == False = True
//    _     == _     = False
instance(Eq, Boolean, { eq: strictEq, ne: strictNe })

//instance Ord Bool where
//    compare False True  = LT
//    compare True  False = GT
//    compare _     _     = EQ
instance(Ord, Boolean, {
    compare: function (a, b) {
        return  ((a === true)  && (b === false)) ? Ordering.LT :
                ((a === false) &&  (b === true)) ? Ordering.GT :
                Ordering.EQ;
    }
})

//-- Read is in GHC.Read, Show in GHC.Show

//-- |'otherwise' is defined as the value 'True'.  It helps to make
//-- guards more readable.  eg.
//--
//-- >  f x | x < 0     = ...
//-- >      | otherwise = ...
//otherwise               :: Bool
//otherwise               =  True
var otherwise = true;




//-- | Represents an ordering relationship between two values: less
//-- than, equal to, or greater than.  An 'Ordering' is returned by
//-- 'compare'.
//-- XXX These don't work:
//-- deriving instance Eq Ordering
//-- deriving instance Ord Ordering
//-- Illegal binding of built-in syntax: con2tag_Ordering#

//instance Eq Ordering where
//    EQ == EQ = True
//    LT == LT = True
//    GT == GT = True
//    _  == _  = False
//        -- Read in GHC.Read, Show in GHC.Show
instance(Eq, Ordering, {
    eq: function (a, b) {
        return  (a.EQ && b.EQ) ? true :
                (a.LT && b.LT) ? true :
                (a.GT && b.GT) ? true :
                false;
    }
})

//instance Ord Ordering where
//    LT <= _  = True
//    _  <= LT = False
//    EQ <= _  = True
//    _  <= EQ = False
//    GT <= GT = True
instance(Ord, Ordering, {
    "<=": function (a, b) {
        return  a.LT ? true  :
                b.LT ? false :
                a.EQ ? true  :
                b.EQ ? false :
                (a.GT && b.GT) ? true :
                error();
    }
})



//{-| The character type 'Char' is an enumeration whose values represent
//Unicode (or equivalently ISO\/IEC 10646) characters
//(see <http://www.unicode.org/> for details).
//This set extends the ISO 8859-1 (Latin-1) character set
//(the first 256 charachers), which is itself an extension of the ASCII
//character set (the first 128 characters).
//A character literal in Haskell has type 'Char'.

//To convert a 'Char' to or from the corresponding 'Int' value defined
//by Unicode, use 'Prelude.toEnum' and 'Prelude.fromEnum' from the
//'Prelude.Enum' class respectively (or equivalently 'ord' and 'chr').
//-}

//-- We don't use deriving for Eq and Ord, because for Ord the derived
//-- instance defines only compare, which takes two primops.  Then
//-- '>' uses compare, and therefore takes two primops instead of one.

//instance Eq Char where
//    (C# c1) == (C# c2) = c1 `eqChar#` c2
//    (C# c1) /= (C# c2) = c1 `neChar#` c2
instance(Eq, String,  { eq: strictEq, ne: strictNe })

//instance Ord Char where
//    (C# c1) >  (C# c2) = c1 `gtChar#` c2
//    (C# c1) >= (C# c2) = c1 `geChar#` c2
//    (C# c1) <= (C# c2) = c1 `leChar#` c2
//    (C# c1) <  (C# c2) = c1 `ltChar#` c2

//TODO: String instances



//-- | The 'Prelude.toEnum' method restricted to the type 'Data.Char.Char'.
//chr :: Int -> Char
//chr i@(I# i#)
// | int2Word# i# `leWord#` int2Word# 0x10FFFF# = C# (chr# i#)
// | otherwise
//    = error ("Prelude.chr: bad argument: " ++ showSignedInt (I# 9#) i "")
var chr = String.fromCharCode;

//#region String.fromCharCode
// https://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Objects/String/fromCharCode
// String.fromCharCode() alone cannot get the character at such a high code point  
// The following, on the other hand, can return a 4-byte character as well as the   
//   usual 2-byte ones (i.e., it can return a single character which actually has   
//   a string length of 2 instead of 1!)  
//alert(fixedFromCharCode(0x2F804)); // or 194564 in decimal 
//
//function fixedFromCharCode (codePt) {  
//    if (codePt > 0xFFFF) {  
//        codePt -= 0x10000;  
//        return String.fromCharCode(0xD800 + (codePt >> 10), 0xDC00 +  
//(codePt & 0x3FF));  
//    }  
//    else {  
//        return String.fromCharCode(codePt);  
//    }  
//}
//#endregion



//-- | The 'Prelude.fromEnum' method restricted to the type 'Data.Char.Char'.
//ord :: Char -> Int
//ord (C# c#) = I# (ord# c#)
// https://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Global_Objects/String/charCodeAt
function ord(s){
    return s.charCodeAt(0);
}


//String equality is used when desugaring pattern-matches against strings.

//eqString :: String -> String -> Bool
//eqString []       []       = True
//eqString (c1:cs1) (c2:cs2) = c1 == c2 && cs1 `eqString` cs2
//eqString _        _        = False
var eqString = strictEq;

//{-# RULES "eqString" (==) = eqString #-}
//-- eqString also has a BuiltInRule in PrelRules.lhs:
//--      eqString (unpackCString# (Lit s1)) (unpackCString# (Lit s2) = s1==s2





//zeroInt, oneInt, twoInt, maxInt, minInt :: Int
//zeroInt = I# 0#
//oneInt  = I# 1#
//twoInt  = I# 2#

//{- Seems clumsy. Should perhaps put minInt and MaxInt directly into MachDeps.h -}

//minInt  = I# (-0x40000000#)
var minInt = Number.MIN_VALUE;

//maxInt  = I# 0x3FFFFFFF#
var maxInt = Number.MAX_VALUE;



//instance Eq Int where
//    (==) = eqInt
//    (/=) = neInt
instance(Eq, Number,  { eq: strictEq, ne: strictNe })

//instance Ord Int where
//    compare = compareInt
//    (<)     = ltInt
//    (<=)    = leInt
//    (>=)    = geInt
//    (>)     = gtInt
instance(Ord, Number, {
    compare: function (a, b) {
        return  a === b ? Ordering.EQ :
                a <= b  ? Ordering.LT :
                          Ordering.GT;
    },
    "<":lt, "<=":le, ">":gt, ">=":ge,
    max: function (x, y) { return x <= y ? y : x },
    min: function (x, y) { return x <= y ? x : y }
})



//%*********************************************************
//%*                                                      *
//\subsection{The function type}
//%*                                                      *
//%*********************************************************


//-- | Identity function.
//id                      :: a -> a
//id x                    =  x
var id = function(x){ return x };

//-- | The call '(lazy e)' means the same as 'e', but 'lazy' has a 
//-- magical strictness property: it is lazy in its first argument, 
//-- even though its semantics is strict.
//lazy :: a -> a
//lazy x = x
//-- Implementation note: its strictness and unfolding are over-ridden
//-- by the definition in MkId.lhs; in both cases to nothing at all.
//-- That way, 'lazy' does not get inlined, and the strictness analyser
//-- sees it as lazy.  Then the worker/wrapper phase inlines it.
//-- Result: happiness
function lazy(f){
    return function(){
        return f().apply(null, arguments);
    }
}
/* 
Of course, this is not exactly the same function but it's somewhat similar:
lazy :: (-> a) -> a
This pseudo type signature says that `lazy` recieves a function, 
which returns `a`, and then we get back a "lazy version" of `a`,
i.e. it's wrapped in function, and the arguments are handled automatically.

usage:

var parser = lazy(function(){ return parser });

//later this can be redifined, but before the following assignment
//`parser` would be undefined, but this way the above function is used,
//which will evalute only later, pointing to the new `parser` function

parser = do$("a" ,"<-", char_("a"))
            ("b" ,"<-", doSthRecursively, parser)


//the alternative of lazy would be:

var parser = function(scope, state, k){ return parser(scope, state, k) };

//where the arguments have to be specified explicitly

Sometimes this is needed even if the actual function is already in scope,
for instance, see the `sepEndBy1` combinator in jsparsec.

*/


//-- | The call '(inline f)' reduces to 'f', but 'inline' has a BuiltInRule
//-- that tries to inline 'f' (if it has an unfolding) unconditionally
//-- The 'NOINLINE' pragma arranges that inline only gets inlined (and
//-- hence eliminated) late in compilation, after the rule has had
//-- a god chance to fire.
//inline :: a -> a
//{-# NOINLINE[0] inline #-}
//inline x = x

//-- Assertion function.  This simply ignores its boolean argument.
//-- The compiler may rewrite it to @('assertError' line)@.

//-- | If the first argument evaluates to 'True', then the result is the
//-- second argument.  Otherwise an 'AssertionFailed' exception is raised,
//-- containing a 'String' with the source file and line number of the
//-- call to 'assert'.
//--
//-- Assertions can normally be turned on or off with a compiler flag
//-- (for GHC, assertions are normally on unless optimisation is turned on 
//-- with @-O@ or the @-fignore-asserts@
//-- option is given).  When assertions are turned off, the first
//-- argument to 'assert' is ignored, and the second argument is
//-- returned as the result.

//--      SLPJ: in 5.04 etc 'assert' is in GHC.Prim,
//--      but from Template Haskell onwards it's simply
//--      defined here in Base.lhs
//assert :: Bool -> a -> a
//assert _pred r = r
function assert(_pred, r){
    if(!ignoreAsserts && !_pred) //TODO
        error("AssertionFailed")
    return r;
}

//breakpoint :: a -> a
//breakpoint r = r

//breakpointCond :: Bool -> a -> a
//breakpointCond _ r = r

//data Opaque = forall a. O a

//-- | Constant function.
//const                   :: a -> b -> a
//const x _               =  x

//const_ can be called only with one argument at a time:
function const_(x){ return function(_){ return x } }
//but the following would work with two arguments as well: 
//var const_ = curry(function(x, _){ return x }); -> const_(x, y);

//-- | Function composition.
//{-# INLINE (.) #-}
//(.)       :: (b -> c) -> (a -> b) -> a -> c
//(.) f g x = f (g x)

//this is the same as (.) in Haskell:
//the inner function receives only the first argument, 
//and all the rest is applied to the outer one
function compose1(fst, snd){
    return function(a){ 
        var args = slice(arguments, 1);
        args.unshift(snd(a));
        return fst.apply(null, args);
    };
}

//this is usually more useful in actual JavaScript, 
//might be removed/renamed, just as compose1!
function compose(fst, snd){
    return function(){
        return fst(snd.apply(null, arguments));
    };
}


//-- | @'flip' f@ takes its (first) two arguments in the reverse order of @f@.
//flip                    :: (a -> b -> c) -> b -> a -> c
//flip f x y              =  f y x
function flip(fn){
    return function(a, b){ return fn(b, a) };
}

//-- | Application operator.  This operator is redundant, since ordinary
//-- application @(f x)@ means the same as @(f '$' x)@. However, '$' has
//-- low, right-associative binding precedence, so it sometimes allows
//-- parentheses to be omitted; for example:
//--
//-- >     f $ g $ h x  =  f (g (h x))
//--
//-- It is also useful in higher-order situations, such as @'map' ('$' 0) xs@,
//-- or @'Data.List.zipWith' ('$') fs xs@.
//{-# INLINE ($) #-}
//($)                     :: (a -> b) -> a -> b
//f $ x                   =  f x
var call = curry(function(a, b){ return a(b) });

//-- | @'until' p f@ yields the result of applying @f@ until @p@ holds.
//until                   :: (a -> Bool) -> (a -> a) -> a -> a
//until p f x | p x       =  x
//            | otherwise =  until p f (f x)
function until(p, f, x) {
    return p(x) ? x : until(p, f, f(x));
}

//-- | 'asTypeOf' is a type-restricted version of 'const'.  It is usually
//-- used as an infix operator, and its typing forces its first argument
//-- (which is usually overloaded) to have the same type as the second.
//asTypeOf                :: a -> a -> a
//asTypeOf                =  const
/*
//definied in Haskell.TypeClass with some explanation
function asTypeOf(tcls, method, value) {
    return getInstance(tcls, typeOf(value))[method];
}
*/




























// -------------------------------------------------
// Eq instances
// -------------------------------------------------

instance(Eq, RegExp, {
    eq: function (a, b) {
        return a.source === b.source &&
               a.global === b.global &&
               a.ignoreCase === b.ignoreCase &&
               a.multiline === b.multiline;
    }
})

instance(Eq, Array, {
    eq: function (a, b) {
        var l = a.length;
        if (l != b.length)
            return false;

        for (var i = 0; i < l; ++i)
            if (!Eq.eq(a[i], b[i]))
                return false;

        return true;
    }
})

instance(Eq, Object, {
    eq: function (a, b) {
        var key, union = {};

        for (key in a)
            union[key] = true;
        for (key in b)
            union[key] = true;

        for (key in union)
            if (!Eq.eq(a[key], b[key]))
                return false;

        return true;
    }
})




// -------------------------------------------------
// Unclassified (TODO)
// -------------------------------------------------

function elemIndex(value, l) {
    var length = l.length;   
    if(!length)
        return Maybe.Nothing;
    var isStr = !!l.charAt;
    var iEq = getInstance(Eq, typeOf(isStr ? l.charAt(0) : l[0]));
    
    for(var i = 0; i < length; ++i)  
        if(iEq.eq(isStr ? l.charAt(i) : l[i], value))  
            return Maybe.Just(i);  
    
    return Maybe.Nothing;
}
    




//  compare x y = if x == y then EQ
//                  -- NB: must be '<=' not '<' to validate the
//                  -- above claim about the minimal things that
//                  -- can be defined for an instance of Ord:
//                  else if x <= y then LT
//                  else GT
function unsafeCompare(x, y){
    return x === y ? Ordering.EQ : 
           x <=  y ? Ordering.LT :
                     Ordering.GT ;
}



function readHex(str){
    return parseInt(str.join ? str.join("") : str, 16);
}

function readOct(str){
    return parseInt(str.join ? str.join("") : str, 8);
}



var round = Math.round;

var toInteger = parseInt; //TODO

var fromInteger = id; //TODO

var fromIntegral = id; //TODO



extend(operators, {
    "$" : {
        func:   call,
        fixity: infixr(0)
        //,type:    [Function, "*", "*"]
    },
    "." : {
        func:   compose1,
        fixity: infixr(9)
        //,type:    [Function, Function, Function]
    },
    ":" : {
        func:   cons,
        fixity: infixr(5)
    },
    "++" : {
        func:   append,
        fixity: infixr(5)
    }
});

//TODO:
//infix  4  ==, /=, <, <=, >=, >
//infixr 3  &&
//infixr 2  ||


namespace("GHC_Base", {
     Unit       : Unit
    ,Tuple      : Tuple
    ,Bool       : Bool
    ,Ordering   : Ordering

    ,Eq         : Eq
    ,Ord        : Ord
    ,andOp      : andOp
    ,orOp       : orOp
    ,not        : not

    ,Functor    : Functor
    ,Monad      : Monad
    //,Show       : Show

    ,emptyListOf: emptyListOf
    ,cons       : cons
    ,consJoin   : consJoin
    ,uncons     : uncons
    ,augment    : augment
    ,map        : map
    ,foldr      : foldr
    ,append     : append
    ,otherwise  : otherwise
    ,ord        : ord
    ,chr        : chr
    ,eqString   : eqString
    ,minInt     : minInt
    ,maxInt     : maxInt
    
    ,id         : id
    ,lazy       : lazy
    ,assert     : assert
    ,const_     : const_
    ,compose1   : compose1
    ,compose    : compose
    ,flip       : flip
    ,call       : call
    ,until      : until
    ,asTypeOf   : asTypeOf
    
    ,elemIndex      : elemIndex
    ,unsafeCompare  : unsafeCompare

    ,readHex        : readHex
    ,readOct        : readOct
    ,round          : round
    ,toInteger      : toInteger
    ,fromInteger    : fromInteger
    ,fromIntegral   : fromIntegral



    //,foldl      : foldl
    //,zip        : zip
    //,replicate  : replicate
    //,sort       : sort //Data.List
    //,nub        : nub //Data.List
    //,maybe      : maybe
    //,lookup     : lookup
    //,span       : span
    //,elemIndex  : elemIndex
    //,compare    : compare
    //,fst        : fst
    //,snd        : snd
    //,uncurry    : uncurry
    //,until      : until
    //,fix        : fix
    //,fix_       : fix_


})

/// <reference path="../../../jshaskell/src/Haskell.js" local />
/// <reference path="Base.js" />
/// <reference path="Maybe.js" local />


//\begin{code}
//{-# OPTIONS_GHC -XNoImplicitPrelude #-}
//{-# OPTIONS_HADDOCK hide #-}
//-----------------------------------------------------------------------------
//-- |
//-- Module      :  GHC.List
//-- Copyright   :  (c) The University of Glasgow 1994-2002
//-- License     :  see libraries/base/LICENSE
//-- 
//-- Maintainer  :  cvs-ghc@haskell.org
//-- Stability   :  internal
//-- Portability :  non-portable (GHC Extensions)
//--
//-- The List data type and its operations
//--
//-----------------------------------------------------------------------------

//-- #hide
//module GHC.List (
//   -- [] (..),          -- Not Haskell 98; built in syntax

//   map, (++), filter, concat,
//   head, last, tail, init, null, length, (!!),
//   foldl, scanl, scanl1, foldr, foldr1, scanr, scanr1,
//   iterate, repeat, replicate, cycle,
//   take, drop, splitAt, takeWhile, dropWhile, span, break,
//   reverse, and, or,
//   any, all, elem, notElem, lookup,
//   concatMap,
//   zip, zip3, zipWith, zipWith3, unzip, unzip3,
//   errorEmptyList,

//#ifndef USE_REPORT_PRELUDE
//   -- non-standard, but hidden when creating the Prelude
//   -- export list.
//   takeUInt_append
//#endif

// ) where

//import Data.Maybe
//import GHC.Base

//infixl 9  !!
//infix  4 `elem`, `notElem` //TODO
//\end{code}

//%*********************************************************
//%*                                                      *
//\subsection{List-manipulation functions}
//%*                                                      *
//%*********************************************************

//\begin{code}
//-- | Extract the first element of a list, which must be non-empty.
//head                    :: [a] -> a
//head (x:_)              =  x
//head []                 =  badHead
function head(xs){
    if(!xs.length)
        errorEmptyList("head");
    return xs.charAt ? xs.charAt(0) : xs[0];
}

//badHead :: a
//badHead = errorEmptyList "head"

//-- This rule is useful in cases like 
//--      head [y | (x,y) <- ps, x==t]
//{-# RULES
//"head/build"    forall (g::forall b.(a->b->b)->b->b) .
//                head (build g) = g (\x _ -> x) badHead
//"head/augment"  forall xs (g::forall b. (a->b->b) -> b -> b) . 
//                head (augment g xs) = g (\x _ -> x) (head xs)
// #-}

//-- | Extract the elements after the head of a list, which must be non-empty.
//tail                    :: [a] -> [a]
//tail (_:xs)             =  xs
//tail []                 =  errorEmptyList "tail"
function tail(xs){
    if(!xs.length)
        errorEmptyList("tail");
    return xs.substr ? xs.substr(1) : slice(xs, 1);
}

//-- | Extract the last element of a list, which must be finite and non-empty.
//last                    :: [a] -> a
//#ifdef USE_REPORT_PRELUDE
//last [x]                =  x
//last (_:xs)             =  last xs
//last []                 =  errorEmptyList "last"
//#else
//-- eliminate repeated cases
//last []                 =  errorEmptyList "last"
//last (x:xs)             =  last' x xs
//  where last' y []     = y
//        last' _ (y:ys) = last' y ys
//#endif
function last(xs){
    var length = xs.length;
    if(!length)
        errorEmptyList("last");
    var i = length - 1;
    return xs.charAt ? xs.charAt(i) : xs[i];
}

//-- | Return all the elements of a list except the last one.
//-- The list must be non-empty.
//init                    :: [a] -> [a]
//#ifdef USE_REPORT_PRELUDE
//init [x]                =  []
//init (x:xs)             =  x : init xs
//init []                 =  errorEmptyList "init"
//#else
//-- eliminate repeated cases
//init []                 =  errorEmptyList "init"
//init (x:xs)             =  init' x xs
//  where init' _ []     = []
//        init' y (z:zs) = y : init' z zs
//#endif
function init(xs){
    var length = xs.length;
    if(!length)
        errorEmptyList("init");
    var i = length - 1;
    return xs.substring ? xs.substring(0, i) : [].slice.call(xs, 0, i);
}


//-- | Test whether a list is empty.
//null                    :: [a] -> Bool
//null []                 =  True
//null (_:_)              =  False
function null_(xs){
    return !xs.length;
}

//-- | /O(n)/. 'length' returns the length of a finite list as an 'Int'.
//-- It is an instance of the more general 'Data.List.genericLength',
//-- the result type of which may be any kind of number.
//length                  :: [a] -> Int
//length l                =  len l 0#
//  where
//    len :: [a] -> Int# -> Int
//    len []     a# = I# a#
//    len (_:xs) a# = len xs (a# +# 1#)
function length(xs){
    return xs.length;
}

//-- | 'filter', applied to a predicate and a list, returns the list of
//-- those elements that satisfy the predicate; i.e.,
//--
//-- > filter p xs = [ x | x <- xs, p x]

//filter :: (a -> Bool) -> [a] -> [a]
//filter _pred []    = []
//filter pred (x:xs)
//  | pred x         = x : filter pred xs
//  | otherwise      = filter pred xs
function filter(pred, xs) {
    var res = [], i = 0, l = xs.length, isStr = xs.charAt;
    for(; i < l; ++i)
        if(pred(isStr ? xs.charAt(i) : xs[i]))
            res.push(xs[i]);
    return isStr ? res.join("") : res;
}

//{-# NOINLINE [0] filterFB #-}
//filterFB :: (a -> b -> b) -> (a -> Bool) -> a -> b -> b
//filterFB c p x r | p x       = x `c` r
//                 | otherwise = r

//{-# RULES
//"filter"     [~1] forall p xs.  filter p xs = build (\c n -> foldr (filterFB c p) n xs)
//"filterList" [1]  forall p.     foldr (filterFB (:) p) [] = filter p
//"filterFB"        forall c p q. filterFB (filterFB c p) q = filterFB c (\x -> q x && p x)
// #-}

//-- Note the filterFB rule, which has p and q the "wrong way round" in the RHS.
//--     filterFB (filterFB c p) q a b
//--   = if q a then filterFB c p a b else b
//--   = if q a then (if p a then c a b else b) else b
//--   = if q a && p a then c a b else b
//--   = filterFB c (\x -> q x && p x) a b
//-- I originally wrote (\x -> p x && q x), which is wrong, and actually
//-- gave rise to a live bug report.  SLPJ.


//-- | 'foldl', applied to a binary operator, a starting value (typically
//-- the left-identity of the operator), and a list, reduces the list
//-- using the binary operator, from left to right:
//--
//-- > foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
//--
//-- The list must be finite.

//-- We write foldl as a non-recursive thing, so that it
//-- can be inlined, and then (often) strictness-analysed,
//-- and hence the classic space leak on foldl (+) 0 xs

//foldl        :: (a -> b -> a) -> a -> [b] -> a
//foldl f z0 xs0 = lgo z0 xs0
//             where
//                lgo z []     =  z
//                lgo z (x:xs) = lgo (f z x) xs
function foldl(f, z, xs){
    var i = 0, l = xs.length, isStr = xs.charAt;
    for(; i < l; ++i) 
        z = f(z, isStr ? xs.charAt(i) : xs[i]);
    return z;
}


//-- | 'scanl' is similar to 'foldl', but returns a list of successive
//-- reduced values from the left:
//--
//-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
//--
//-- Note that
//--
//-- > last (scanl f z xs) == foldl f z xs.

//scanl                   :: (a -> b -> a) -> a -> [b] -> [a]
//scanl f q ls            =  q : (case ls of
//                                []   -> []
//                                x:xs -> scanl f (f q x) xs)
function scanl(f, q, ls){
    var rest;
    if(!ls.length){
        rest = emptyListOf(ls);
    }else{
        var a = uncons(ls);
        rest = scanl(f, f(q, a.head), a.tail);
    }
    return cons(q, rest);
}

//-- | 'scanl1' is a variant of 'scanl' that has no starting value argument:
//--
//-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]

//scanl1                  :: (a -> a -> a) -> [a] -> [a]
//scanl1 f (x:xs)         =  scanl f x xs
//scanl1 _ []             =  []
function scanl1(f, xs){
    if(!xs.length)
        return emptyListOf(xs);
    var a = uncons(xs);
    return scanl(f, a.head, a.tail);
}

//-- foldr, foldr1, scanr, and scanr1 are the right-to-left duals of the
//-- above functions.

//-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
//-- and thus must be applied to non-empty lists.

//foldr1                  :: (a -> a -> a) -> [a] -> a
//foldr1 _ [x]            =  x
//foldr1 f (x:xs)         =  f x (foldr1 f xs)
//foldr1 _ []             =  errorEmptyList "foldr1"
function foldr1(f, xs){
    var length = xs.length;
    if(length == 0)
        errorEmptyList("foldr1");
    if(length == 1)
        return xs.charAt ? xs.charAt(0) : xs[0];
    var a = uncons(xs);
    return f(a.head, foldr1(f, a.tail));
}

//-- | 'scanr' is the right-to-left dual of 'scanl'.
//-- Note that
//--
//-- > head (scanr f z xs) == foldr f z xs.

//scanr                   :: (a -> b -> b) -> b -> [a] -> [b]
//scanr _ q0 []           =  [q0]
//scanr f q0 (x:xs)       =  f x q : qs
//                           where qs@(q:_) = scanr f q0 xs 
function scanr(f, q0, xs){
    if(!xs.length)
        return cons(q0, emptyListOf(xs));
    
    var a = uncons(xs),
        qs = scanr(f, q0, a.tail);
    
    return cons(f(a.head, qs.head), qs);
}

//-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.

//scanr1                  :: (a -> a -> a) -> [a] -> [a]
//scanr1 _ []             =  []
//scanr1 _ [x]            =  [x]
//scanr1 f (x:xs)         =  f x q : qs
//                           where qs@(q:_) = scanr1 f xs 
function scanr1(f, xs){
    var length = xs.length;
    if(length == 0)
        return emptyListOf(xs);
    if(length == 1)
        return cons(xs.charAt ? xs.charAt(0) : xs[0], emptyListOf(xs));
    
    var a = uncons(xs),
        qs = scanr1(f, a.tail);
    
    return cons(f(a.head, qs.head), qs);
}

//-- | 'iterate' @f x@ returns an infinite list of repeated applications
//-- of @f@ to @x@:
//--
//-- > iterate f x == [x, f x, f (f x), ...]

//iterate :: (a -> a) -> a -> [a]
//iterate f x =  x : iterate f (f x)

//TODO

//iterateFB :: (a -> b -> b) -> (a -> a) -> a -> b
//iterateFB c f x = x `c` iterateFB c f (f x)


//{-# RULES
//"iterate"    [~1] forall f x.   iterate f x = build (\c _n -> iterateFB c f x)
//"iterateFB"  [1]                iterateFB (:) = iterate
// #-}


//-- | 'repeat' @x@ is an infinite list, with @x@ the value of every element.
//repeat :: a -> [a]
//{-# INLINE [0] repeat #-}
//-- The pragma just gives the rules more chance to fire
//repeat x = xs where xs = x : xs

//TODO
//repilcate should be used instead

//{-# INLINE [0] repeatFB #-}     -- ditto
//repeatFB :: (a -> b -> b) -> a -> b
//repeatFB c x = xs where xs = x `c` xs


//{-# RULES
//"repeat"    [~1] forall x. repeat x = build (\c _n -> repeatFB c x)
//"repeatFB"  [1]  repeatFB (:)       = repeat
// #-}

//-- | 'replicate' @n x@ is a list of length @n@ with @x@ the value of
//-- every element.
//-- It is an instance of the more general 'Data.List.genericReplicate',
//-- in which @n@ may be of any integral type.
//{-# INLINE replicate #-}
//replicate               :: Int -> a -> [a]
//replicate n x           =  take n (repeat x)
function replicate(n, x){
    for(var ret = [], i = 0; i < n; ++i)
        ret[i] = x;
    return ret;
}

//-- | 'cycle' ties a finite list into a circular one, or equivalently,
//-- the infinite repetition of the original list.  It is the identity
//-- on infinite lists.

//cycle                   :: [a] -> [a]
//cycle []                = error "Prelude.cycle: empty list"
//cycle xs                = xs' where xs' = xs ++ xs'

//TODO

//-- | 'takeWhile', applied to a predicate @p@ and a list @xs@, returns the
//-- longest prefix (possibly empty) of @xs@ of elements that satisfy @p@:
//--
//-- > takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
//-- > takeWhile (< 9) [1,2,3] == [1,2,3]
//-- > takeWhile (< 0) [1,2,3] == []
//--

//takeWhile               :: (a -> Bool) -> [a] -> [a]
//takeWhile _ []          =  []
//takeWhile p (x:xs) 
//            | p x       =  x : takeWhile p xs
//            | otherwise =  []
function takeWhile(p, xs){
    var isStr = !!xs.charAt, i = 0, l = xs.length;
    for(; i < l; ++i)
        if(!p(isStr ? xs.charAt(i) : xs[i]))
            break;
    return isStr ? xs.substring(0, i) : [].slice.call(xs, 0, i);
}

//-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@:
//--
//-- > dropWhile (< 3) [1,2,3,4,5,1,2,3] == [3,4,5,1,2,3]
//-- > dropWhile (< 9) [1,2,3] == []
//-- > dropWhile (< 0) [1,2,3] == [1,2,3]
//--

//dropWhile               :: (a -> Bool) -> [a] -> [a]
//dropWhile _ []          =  []
//dropWhile p xs@(x:xs')
//            | p x       =  dropWhile p xs'
//            | otherwise =  xs
function dropWhile(p, xs){
    var isStr = !!xs.charAt, i = 0, l = xs.length;
    for(; i < l; ++i)
        if(!p(isStr ? xs.charAt(i) : xs[i]))
            break;
    return isStr ? xs.substr(i) : slice(xs, i);
}

//-- | 'take' @n@, applied to a list @xs@, returns the prefix of @xs@
//-- of length @n@, or @xs@ itself if @n > 'length' xs@:
//--
//-- > take 5 "Hello World!" == "Hello"
//-- > take 3 [1,2,3,4,5] == [1,2,3]
//-- > take 3 [1,2] == [1,2]
//-- > take 3 [] == []
//-- > take (-1) [1,2] == []
//-- > take 0 [1,2] == []
//--
//-- It is an instance of the more general 'Data.List.genericTake',
//-- in which @n@ may be of any integral type.
//take                   :: Int -> [a] -> [a]
function take(n, xs){
    return xs.substring ?
        xs.substring(0, n) :
        [].slice.call(xs, 0, n);
}

//-- | 'drop' @n xs@ returns the suffix of @xs@
//-- after the first @n@ elements, or @[]@ if @n > 'length' xs@:
//--
//-- > drop 6 "Hello World!" == "World!"
//-- > drop 3 [1,2,3,4,5] == [4,5]
//-- > drop 3 [1,2] == []
//-- > drop 3 [] == []
//-- > drop (-1) [1,2] == [1,2]
//-- > drop 0 [1,2] == [1,2]
//--
//-- It is an instance of the more general 'Data.List.genericDrop',
//-- in which @n@ may be of any integral type.
//drop                   :: Int -> [a] -> [a]
function drop(n, xs){
    return xs.substring ?
        xs.substring(n) :
        [].slice.call(xs, n);
}

//-- | 'splitAt' @n xs@ returns a tuple where first element is @xs@ prefix of
//-- length @n@ and second element is the remainder of the list:
//--
//-- > splitAt 6 "Hello World!" == ("Hello ","World!")
//-- > splitAt 3 [1,2,3,4,5] == ([1,2,3],[4,5])
//-- > splitAt 1 [1,2,3] == ([1],[2,3])
//-- > splitAt 3 [1,2,3] == ([1,2,3],[])
//-- > splitAt 4 [1,2,3] == ([1,2,3],[])
//-- > splitAt 0 [1,2,3] == ([],[1,2,3])
//-- > splitAt (-1) [1,2,3] == ([],[1,2,3])
//--
//-- It is equivalent to @('take' n xs, 'drop' n xs)@.
//-- 'splitAt' is an instance of the more general 'Data.List.genericSplitAt',
//-- in which @n@ may be of any integral type.
//splitAt                :: Int -> [a] -> ([a],[a])
function splitAt(n, xs){
    var t = [take(n, xs), drop(n, xs)];
    t.constructor = Tuple.Tuple2;
    return t;
}


//#region hack away
//#ifdef USE_REPORT_PRELUDE
//take n _      | n <= 0 =  []
//take _ []              =  []
//take n (x:xs)          =  x : take (n-1) xs

//drop n xs     | n <= 0 =  xs
//drop _ []              =  []
//drop n (_:xs)          =  drop (n-1) xs

//splitAt n xs           =  (take n xs, drop n xs)

//#else /* hack away */
//{-# RULES
//"take"     [~1] forall n xs . take n xs = takeFoldr n xs 
//"takeList"  [1] forall n xs . foldr (takeFB (:) []) (takeConst []) xs n = takeUInt n xs
// #-}

//{-# INLINE takeFoldr #-}
//takeFoldr :: Int -> [a] -> [a]
//takeFoldr (I# n#) xs
//  = build (\c nil -> if n# <=# 0# then nil else
//                     foldr (takeFB c nil) (takeConst nil) xs n#)

//{-# NOINLINE [0] takeConst #-}
//-- just a version of const that doesn't get inlined too early, so we
//-- can spot it in rules.  Also we need a type sig due to the unboxed Int#.
//takeConst :: a -> Int# -> a
//takeConst x _ = x

//{-# NOINLINE [0] takeFB #-}
//takeFB :: (a -> b -> b) -> b -> a -> (Int# -> b) -> Int# -> b
//takeFB c n x xs m | m <=# 1#  = x `c` n
//                  | otherwise = x `c` xs (m -# 1#)

//{-# INLINE [0] take #-}
//take (I# n#) xs = takeUInt n# xs

//-- The general code for take, below, checks n <= maxInt
//-- No need to check for maxInt overflow when specialised
//-- at type Int or Int# since the Int must be <= maxInt

//takeUInt :: Int# -> [b] -> [b]
//takeUInt n xs
//  | n >=# 0#  =  take_unsafe_UInt n xs
//  | otherwise =  []

//take_unsafe_UInt :: Int# -> [b] -> [b]
//take_unsafe_UInt 0#  _  = []
//take_unsafe_UInt m   ls =
//  case ls of
//    []     -> []
//    (x:xs) -> x : take_unsafe_UInt (m -# 1#) xs

//takeUInt_append :: Int# -> [b] -> [b] -> [b]
//takeUInt_append n xs rs
//  | n >=# 0#  =  take_unsafe_UInt_append n xs rs
//  | otherwise =  []

//take_unsafe_UInt_append :: Int# -> [b] -> [b] -> [b]
//take_unsafe_UInt_append 0#  _ rs  = rs
//take_unsafe_UInt_append m  ls rs  =
//  case ls of
//    []     -> rs
//    (x:xs) -> x : take_unsafe_UInt_append (m -# 1#) xs rs

//drop (I# n#) ls
//  | n# <# 0#    = ls
//  | otherwise   = drop# n# ls
//    where
//        drop# :: Int# -> [a] -> [a]
//        drop# 0# xs      = xs
//        drop# _  xs@[]   = xs
//        drop# m# (_:xs)  = drop# (m# -# 1#) xs

//splitAt (I# n#) ls
//  | n# <# 0#    = ([], ls)
//  | otherwise   = splitAt# n# ls
//    where
//        splitAt# :: Int# -> [a] -> ([a], [a])
//        splitAt# 0# xs     = ([], xs)
//        splitAt# _  xs@[]  = (xs, xs)
//        splitAt# m# (x:xs) = (x:xs', xs'')
//          where
//            (xs', xs'') = splitAt# (m# -# 1#) xs

//#endif /* USE_REPORT_PRELUDE */
//#endregion 

//-- | 'span', applied to a predicate @p@ and a list @xs@, returns a tuple where
//-- first element is longest prefix (possibly empty) of @xs@ of elements that
//-- satisfy @p@ and second element is the remainder of the list:
//-- 
//-- > span (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
//-- > span (< 9) [1,2,3] == ([1,2,3],[])
//-- > span (< 0) [1,2,3] == ([],[1,2,3])
//-- 
//-- 'span' @p xs@ is equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@

//span                    :: (a -> Bool) -> [a] -> ([a],[a])
//span _ xs@[]            =  (xs, xs)
//span p xs@(x:xs')
//         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
//         | otherwise    =  ([],xs)
function span(p, xs){
    var t = [takeWhile(p, xs), dropWhile(p, xs)];
    t.constructor = Tuple.Tuple2;
    return t;
}


//-- | 'break', applied to a predicate @p@ and a list @xs@, returns a tuple where
//-- first element is longest prefix (possibly empty) of @xs@ of elements that
//-- /do not satisfy/ @p@ and second element is the remainder of the list:
//-- 
//-- > break (> 3) [1,2,3,4,1,2,3,4] == ([1,2,3],[4,1,2,3,4])
//-- > break (< 9) [1,2,3] == ([],[1,2,3])
//-- > break (> 9) [1,2,3] == ([1,2,3],[])
//--
//-- 'break' @p@ is equivalent to @'span' ('not' . p)@.
function break_(p, xs){
    return span(function(x){ return !p(x) }, xs);
}

//break                   :: (a -> Bool) -> [a] -> ([a],[a])
//#ifdef USE_REPORT_PRELUDE
//break p                 =  span (not . p)
//#else
//-- HBC version (stolen)
//break _ xs@[]           =  (xs, xs)
//break p xs@(x:xs')
//           | p x        =  ([],xs)
//           | otherwise  =  let (ys,zs) = break p xs' in (x:ys,zs)
//#endif

//-- | 'reverse' @xs@ returns the elements of @xs@ in reverse order.
//-- @xs@ must be finite.
//reverse                 :: [a] -> [a]
//#ifdef USE_REPORT_PRELUDE
//reverse                 =  foldl (flip (:)) []
//#else
//reverse l =  rev l []
//  where
//    rev []     a = a
//    rev (x:xs) a = rev xs (x:a)
//#endif
function reverse(l){
    var isStr = l.charAt,
        r = toArray(r);
    r.reverse();
    return isStr ? r.join("") : r;
}

//-- | 'and' returns the conjunction of a Boolean list.  For the result to be
//-- 'True', the list must be finite; 'False', however, results from a 'False'
//-- value at a finite index of a finite or infinite list.
//and                     :: [Bool] -> Bool

//-- | 'or' returns the disjunction of a Boolean list.  For the result to be
//-- 'False', the list must be finite; 'True', however, results from a 'True'
//-- value at a finite index of a finite or infinite list.
//or                      :: [Bool] -> Bool
//#ifdef USE_REPORT_PRELUDE
//and                     =  foldr (&&) True
function and(xs){
    return foldr(andOp, true, xs);
}

//or                      =  foldr (||) False
function or(xs){
    return foldr(orOp, false, xs);
}

//#else
//and []          =  True
//and (x:xs)      =  x && and xs
//or []           =  False
//or (x:xs)       =  x || or xs

//{-# RULES
//"and/build"     forall (g::forall b.(Bool->b->b)->b->b) . 
//                and (build g) = g (&&) True
//"or/build"      forall (g::forall b.(Bool->b->b)->b->b) . 
//                or (build g) = g (||) False
// #-}
//#endif

//-- | Applied to a predicate and a list, 'any' determines if any element
//-- of the list satisfies the predicate.
//any                     :: (a -> Bool) -> [a] -> Bool

//-- | Applied to a predicate and a list, 'all' determines if all elements
//-- of the list satisfy the predicate.
//all                     :: (a -> Bool) -> [a] -> Bool
//#ifdef USE_REPORT_PRELUDE

//any p                   =  or . map p
function any(p, xs){
    return or(map(p, xs));
}

//all p                   =  and . map p
function all(p, xs){
    return and(map(p, xs));
}

//#else
//any _ []        = False
//any p (x:xs)    = p x || any p xs

//all _ []        =  True
//all p (x:xs)    =  p x && all p xs
//{-# RULES
//"any/build"     forall p (g::forall b.(a->b->b)->b->b) . 
//                any p (build g) = g ((||) . p) False
//"all/build"     forall p (g::forall b.(a->b->b)->b->b) . 
//                all p (build g) = g ((&&) . p) True
// #-}
//#endif

//-- | 'elem' is the list membership predicate, usually written in infix form,
//-- e.g., @x \`elem\` xs@.
//elem                    :: (Eq a) => a -> [a] -> Bool
function elem(x, xs){
    return indexOf(x, xs) != -1;
}

//-- | 'notElem' is the negation of 'elem'.
//notElem                 :: (Eq a) => a -> [a] -> Bool
function notElem(x, xs){
    return indexOf(x, xs) == -1;
}

//#ifdef USE_REPORT_PRELUDE
//elem x                  =  any (== x)
//notElem x               =  all (/= x)
//#else
//elem _ []       = False
//elem x (y:ys)   = x==y || elem x ys

//notElem _ []    =  True
//notElem x (y:ys)=  x /= y && notElem x ys
//#endif

//-- | 'lookup' @key assocs@ looks up a key in an association list.
//lookup                  :: (Eq a) => a -> [(a,b)] -> Maybe b
//lookup _key []          =  Nothing
//lookup  key ((x,y):xys)
//    | key == x          =  Just y
//    | otherwise         =  lookup key xys
function lookup(key, l){
    var length = l.length;   
    if (!length)
        return Maybe.Nothing;
    var isStr = !!l.charAt,
        eq = asTypeOf(Eq, "eq", key);

    for(var i = 0; i < length; ++i){
        var e = isStr ? l.charAt(i) : l[i];
        if(eq(e[0], key))
            return Maybe.Just(e[1]);  
    }
    return Maybe.Nothing;
}

//-- | Map a function over a list and concatenate the results.
//concatMap               :: (a -> [b]) -> [a] -> [b]
//concatMap f             =  foldr ((++) . f) []
function concatMap(f, xs){
    return foldr(function(a, b){ return append(f(a), b) }, [], xs);
}

//-- | Concatenate a list of lists.
//concat :: [[a]] -> [a]
//concat = foldr (++) []
function concat(xss){
    return foldr(append, [], xss);
}

//{-# RULES
//  "concat" forall xs. concat xs = build (\c n -> foldr (\x y -> foldr c y x) n xs)
//-- We don't bother to turn non-fusible applications of concat back into concat
// #-}

//\end{code}


//\begin{code}
//-- | List index (subscript) operator, starting from 0.
//-- It is an instance of the more general 'Data.List.genericIndex',
//-- which takes an index of any integral type.
//(!!)                    :: [a] -> Int -> a
//#ifdef USE_REPORT_PRELUDE
//xs     !! n | n < 0 =  error "Prelude.!!: negative index"
//[]     !! _         =  error "Prelude.!!: index too large"
//(x:_)  !! 0         =  x
//(_:xs) !! n         =  xs !! (n-1)
//#else
//-- HBC version (stolen), then unboxified
//-- The semantics is not quite the same for error conditions
//-- in the more efficient version.
//--
//xs !! (I# n0) | n0 <# 0#   =  error "Prelude.(!!): negative index\n"
//               | otherwise =  sub xs n0
//                         where
//                            sub :: [a] -> Int# -> a
//                            sub []     _ = error "Prelude.(!!): index too large\n"
//                            sub (y:ys) n = if n ==# 0#
//                                           then y
//                                           else sub ys (n -# 1#)
function index(xs, n){
    return xs.charAt ? xs.charAt(n) : xs[n];
}

//#endif
//\end{code}


//%*********************************************************
//%*                                                      *
//\subsection{The zip family}
//%*                                                      *
//%*********************************************************

//\begin{code}

//#region foldr2

//foldr2 :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
//foldr2 _k z []    _ys    = z
//foldr2 _k z _xs   []     = z
//foldr2 k z (x:xs) (y:ys) = k x y (foldr2 k z xs ys)

//foldr2_left :: (a -> b -> c -> d) -> d -> a -> ([b] -> c) -> [b] -> d
//foldr2_left _k  z _x _r []     = z
//foldr2_left  k _z  x  r (y:ys) = k x y (r ys)

//foldr2_right :: (a -> b -> c -> d) -> d -> b -> ([a] -> c) -> [a] -> d
//foldr2_right _k z  _y _r []     = z
//foldr2_right  k _z  y  r (x:xs) = k x y (r xs)

//-- foldr2 k z xs ys = foldr (foldr2_left k z)  (\_ -> z) xs ys
//-- foldr2 k z xs ys = foldr (foldr2_right k z) (\_ -> z) ys xs
//{-# RULES
//"foldr2/left"   forall k z ys (g::forall b.(a->b->b)->b->b) . 
//                  foldr2 k z (build g) ys = g (foldr2_left  k z) (\_ -> z) ys

//"foldr2/right"  forall k z xs (g::forall b.(a->b->b)->b->b) . 
//                  foldr2 k z xs (build g) = g (foldr2_right k z) (\_ -> z) xs
// #-}
//\end{code}

//The foldr2/right rule isn't exactly right, because it changes
//the strictness of foldr2 (and thereby zip)

//E.g. main = print (null (zip nonobviousNil (build undefined)))
//          where   nonobviousNil = f 3
//                  f n = if n == 0 then [] else f (n-1)

//I'm going to leave it though.

//#endregion

//Zips for larger tuples are in the List module.

//\begin{code}
//----------------------------------------------
//-- | 'zip' takes two lists and returns a list of corresponding pairs.
//-- If one input list is short, excess elements of the longer list are
//-- discarded.
//zip :: [a] -> [b] -> [(a,b)]
//zip (a:as) (b:bs) = (a,b) : zip as bs
//zip _      _      = []
function zip(as, bs){
    var res = [],
        aIsStr = as.charAt,
        bIsStr = bs.charAt,
        i = 0,
        l = Math.min(as.length, bs.length);
    for(; i < l; ++i)
        res[i] = [aIsStr ? as.charAt(i) : as[i], 
                  bIsStr ? bs.charAt(i) : bs[i]];
    return res;
}

//{-# INLINE [0] zipFB #-}
//zipFB :: ((a, b) -> c -> d) -> a -> b -> c -> d
//zipFB c x y r = (x,y) `c` r

//{-# RULES
//"zip"      [~1] forall xs ys. zip xs ys = build (\c n -> foldr2 (zipFB c) n xs ys)
//"zipList"  [1]  foldr2 (zipFB (:)) []   = zip
// #-}
//\end{code}

//\begin{code}
//----------------------------------------------
//-- | 'zip3' takes three lists and returns a list of triples, analogous to
//-- 'zip'.
//zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
//-- Specification
//-- zip3 =  zipWith3 (,,)
//zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
//zip3 _      _      _      = []
function zip3(as, bs, cs){
    var res = [],
        aIsStr = as.charAt,
        bIsStr = bs.charAt,
        cIsStr = cs.charAt,
        i = 0,
        l = Math.min(as.length, bs.length);
    for(; i < l; ++i)
        res[i] = [aIsStr ? as.charAt(i) : as[i], 
                  bIsStr ? bs.charAt(i) : bs[i],
                  cIsStr ? cs.charAt(i) : cs[i]];
    return res;
}

//\end{code}


//-- The zipWith family generalises the zip family by zipping with the
//-- function given as the first argument, instead of a tupling function.

//\begin{code}
//----------------------------------------------
//-- | 'zipWith' generalises 'zip' by zipping with the function given
//-- as the first argument, instead of a tupling function.
//-- For example, @'zipWith' (+)@ is applied to two lists to produce the
//-- list of corresponding sums.
//zipWith :: (a->b->c) -> [a]->[b]->[c]
//zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
//zipWith _ _      _      = []
function zipWith(f, as, bs){
    var res = [],
        aIsStr = as.charAt,
        bIsStr = bs.charAt,
        i = 0,
        l = Math.min(as.length, bs.length);
    for(; i < l; ++i)
        res[i] = f(aIsStr ? as.charAt(i) : as[i], 
                   bIsStr ? bs.charAt(i) : bs[i]);
    return res;
}

//{-# INLINE [0] zipWithFB #-}
//zipWithFB :: (a -> b -> c) -> (d -> e -> a) -> d -> e -> b -> c
//zipWithFB c f x y r = (x `f` y) `c` r

//{-# RULES
//"zipWith"       [~1] forall f xs ys.    zipWith f xs ys = build (\c n -> foldr2 (zipWithFB c f) n xs ys)
//"zipWithList"   [1]  forall f.  foldr2 (zipWithFB (:) f) [] = zipWith f
//  #-}
//\end{code}

//\begin{code}
//-- | The 'zipWith3' function takes a function which combines three
//-- elements, as well as three lists and returns a list of their point-wise
//-- combination, analogous to 'zipWith'.
//zipWith3                :: (a->b->c->d) -> [a]->[b]->[c]->[d]
//zipWith3 z (a:as) (b:bs) (c:cs)
//                        =  z a b c : zipWith3 z as bs cs
//zipWith3 _ _ _ _        =  []
function zipWith3(f, as, bs, cs){
    var res = [],
        aIsStr = as.charAt,
        bIsStr = bs.charAt,
        cIsStr = cs.charAt,
        i = 0,
        l = Math.min(as.length, bs.length);
    for(; i < l; ++i)
        res[i] = f(aIsStr ? as.charAt(i) : as[i], 
                   bIsStr ? bs.charAt(i) : bs[i],
                   cIsStr ? cs.charAt(i) : cs[i]);
    return res;
}

//-- | 'unzip' transforms a list of pairs into a list of first components
//-- and a list of second components.
//unzip    :: [(a,b)] -> ([a],[b])
//{-# INLINE unzip #-}
//unzip    =  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])
function unzip(ts){
    return foldr(function(t, acc){
        return [cons(t[0], acc[0]), cons(t[1], acc[1])];
    }, [[],[]], ts);
}

//-- | The 'unzip3' function takes a list of triples and returns three
//-- lists, analogous to 'unzip'.
//unzip3   :: [(a,b,c)] -> ([a],[b],[c])
//{-# INLINE unzip3 #-}
//unzip3   =  foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs))
//                  ([],[],[])
function unzip3(ts){
    return foldr(function(t, acc){
        return [cons(t[0], acc[0]), cons(t[1], acc[1]), cons(t[2], acc[2])];
    }, [[],[],[]], ts);
}
//\end{code}


//%*********************************************************
//%*                                                      *
//\subsection{Error code}
//%*                                                      *
//%*********************************************************

//Common up near identical calls to `error' to reduce the number
//constant strings created when compiled:

//\begin{code}
//errorEmptyList :: String -> a
//errorEmptyList fun =
//  error (prel_list_str ++ fun ++ ": empty list")
function errorEmptyList(fun){
    return error("Prelude." + fun + ": empty list") 
}

//prel_list_str :: String
//prel_list_str = "Prelude."
//\end{code}


namespace("GHC_List", {
     map        : map
    //TODO: , (++)
    ,append     : append
    ,filter     : filter
    ,concat     : concat
    ,head       : head
    ,last       : last
    ,tail       : tail
    ,init       : init
    ,null_      : null_
    ,length     : length
    //TODO: , (!!)
    ,index      : index
    ,foldl      : foldl
    ,scanl      : scanl
    ,scanl1     : scanl1
    ,foldr      : foldr
    ,foldr1     : foldr1
    ,scanr      : scanr
    ,scanr1     : scanr1
    //,iterate    : iterate
    //,repeat     : repeat
    ,replicate  : replicate
    //,cycle      : cycle
    ,take       : take
    ,drop       : drop
    ,splitAt    : splitAt
    ,takeWhile  : takeWhile
    ,dropWhile  : dropWhile
    ,span       : span
    ,break_     : break_
    ,reverse    : reverse
    ,and        : and //TODO
    ,or         : or
    ,any        : any
    ,all        : all
    ,elem       : elem
    ,notElem    : notElem
    ,lookup     : lookup
    ,concatMap  : concatMap
    ,zip        : zip
    ,zip3       : zip3
    ,zipWith    : zipWith
    ,zipWith3   : zipWith3
    ,unzip      : unzip
    ,unzip3     : unzip3
    ,errorEmptyList : errorEmptyList
})
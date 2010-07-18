/// <reference path="../../jshaskell/src/Haskell.js" local />


// This module currently contains only some random generic functions and type classes,
// later on some of these will be moved to separate modules.
// Also, for now, most of the list functions doesn't work with strings in _some_ browsers.



// tuples are not defined with `data`, they are just simple classes
var Tuple = {};
imap(function (e) {
    var ntuple = "Tuple" + e;
    Tuple[ntuple] = function () { };
    Tuple[ntuple]._name = ntuple;
    Tuple["tuple" + e] = function (t) {
        t.constructor = Tuple[ntuple];
        return t;
    }
}, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15])

//#region comment

//var tuple2 = Tuple.tuple2([1,2]);
//tuple2.constructor == Tuple.Tuple2; //==> true
//
//to avoid creating a new object, only the constructor is replaced,
//so instanceof gives wrong results:
//
//tuple2 instanceof Tuple.Tuple2; //==> false
//tuple2 instanceof Array; //==> true

//#endregion

//generic tuple "data constructor", which transforms any array to a tuple
function tuple(t) {
    var ntuple = "Tuple" + t.length;
    t.constructor = Tuple[ntuple];
    return t;
}

Tuple.tuple = tuple;


//the () type
function Unit(){}
data(Unit, ["Unit"])
var unit = Unit.Unit;


function Maybe(){}
data(Maybe, [["Just", "a"], "Nothing"]);

function Ordering(){}
data(Ordering, ["LT", "EQ", "GT"]);

function Either(){}
data(Either, [["Left", "a"], ["Right", "b"]]);




// -------------------------------------------------
// Eq type class and instances
// -------------------------------------------------

var a = "a";

//{-
//-- | The 'Eq' class defines equality ('==') and inequality ('/=').
//-- All the basic datatypes exported by the "Prelude" are instances of 'Eq',
//-- and 'Eq' may be derived for any datatype whose constituents are also
//-- instances of 'Eq'.
//--
//-- Minimal complete definition: either '==' or '/='.
//--
//-}

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

instance(Eq, Number,  { eq: strictEq, ne: strictNe })

instance(Eq, String,  { eq: strictEq, ne: strictNe })

instance(Eq, Boolean, { eq: strictEq, ne: strictNe })

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



// -------------------------------------------------
// Ord type class and instances
// -------------------------------------------------

//{-
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
//-}

//{-
//class  (Eq a) => Ord a  where
//    compare              :: a -> a -> Ordering
//    (<), (<=), (>), (>=) :: a -> a -> Bool
//    max, min             :: a -> a -> a
//
//    compare x y = if x == y then EQ
//                  -- NB: must be '<=' not '<' to validate the
//                  -- above claim about the minimal things that
//                  -- can be defined for an instance of Ord:
//                  else if x <= y then LT
//                  else GT
//
//    x <  y = case compare x y of { LT -> True;  _ -> False }
//    x <= y = case compare x y of { GT -> False; _ -> True }
//    x >  y = case compare x y of { GT -> True;  _ -> False }
//    x >= y = case compare x y of { LT -> False; _ -> True }
//
//        -- These two default methods use '<=' rather than 'compare'
//        -- because the latter is often more expensive
//    max x y = if x <= y then y else x
//    min x y = if x <= y then x else y
//-}

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
    .impl(function(inst){ return {
        compare: function (x, y) {
            return  Eq.eq(x, y)      ? Ordering.EQ : //TODO: extend subclass(?), use `inst`
                    inst["<="](x, y) ? Ordering.LT :
                                       Ordering.GT;
        },
        "<"  : function (x, y) { return inst.compare(x, y).LT ? true  : false },
        "<=" : function (x, y) { return inst.compare(x, y).GT ? false :  true },
        ">"  : function (x, y) { return inst.compare(x, y).GT ? true  : false },
        ">=" : function (x, y) { return inst.compare(x, y).LT ? false :  true },
        max  : function (x, y) { return inst["<="](x, y) ? y : x },
        min  : function (x, y) { return inst["<="](x, y) ? x : y }
    }});


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

//TODO: Char/String instance


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


// -------------------------------------------------
// Functor type class
// -------------------------------------------------

//{- | The 'Functor' class is used for types that can be mapped over.
//Instances of 'Functor' should satisfy the following laws:
//
//> fmap id  ==  id
//> fmap (f . g)  ==  fmap f . fmap g
//
//The instances of 'Functor' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
//defined in the "Prelude" satisfy these laws.
//-}

//{-
//class  Functor f  where
//    fmap        :: (a -> b) -> f a -> f b
//
//    -- | Replace all locations in the input with the same value.
//    -- The default definition is @'fmap' . 'const'@, but this may be
//    -- overridden with a more efficient version.
//    (<$)        :: a -> f b -> f a
//    (<$)        =  fmap . const
//-}

var Functor = typeclass("Functor", "f")
    .types({
        fmap: [Function, "f", "f"],
        "<$": ["a", "f", "f"]
    })
    .impl(function(inst){ return {
        "<$": function (a, b) { return inst.fmap(const_(a), b) }
    }})


// -------------------------------------------------
// Monad type class
// -------------------------------------------------

//{- | The 'Monad' class defines the basic operations over a /monad/,
//a concept from a branch of mathematics known as /category theory/.
//From the perspective of a Haskell programmer, however, it is best to
//think of a monad as an /abstract datatype/ of actions.
//Haskell's @do@ expressions provide a convenient syntax for writing
//monadic expressions.
//
//Minimal complete definition: '>>=' and 'return'.
//
//Instances of 'Monad' should satisfy the following laws:
//
//> return a >>= k  ==  k a
//> m >>= return  ==  m
//> m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h
//
//Instances of both 'Monad' and 'Functor' should additionally satisfy the law:
//
//> fmap f xs  ==  xs >>= return . f
//
//The instances of 'Monad' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
//defined in the "Prelude" satisfy these laws.
//-}

//{-
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
//
//    -- | Inject a value into the monadic type.
//    return      :: a -> m a
//    -- | Fail with a message.  This operation is not part of the
//    -- mathematical definition of a monad, but is invoked on pattern-match
//    -- failure in a @do@ expression.
//    fail        :: String -> m a
//
//    m >> k      = m >>= \_ -> k
//    fail s      = error s
//-}

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



// -------------------------------------------------
// Show type class (simplified)
// -------------------------------------------------

var Show = typeclass("Show", "a")
    .types({
        show      : ["a", String]
    })
    .impl({
        show      : function(a){ return a.toString() }
    })

instance(Show, Number)
instance(Show, Boolean)
instance(Show, RegExp)

instance(Show, Function, {
    show: function(a){
        var name = a.name || a._name || "\u03BB";
        if(a.constructor)
            return name + " :: " + a.constructor.name;
        var args = a.toString().match(/\([^\)]*\)/);
        args = args && args[0] || "";
        return name + args;
    }
})

instance(Show, String, {
    show: function(a){ return '"' + a.toString() + '"' }
})

instance(Show, Array, {
    show: function(a){
        return (typeof a[0] == String) ? 
             '"' + a.join("") + '"' :
             '[' + map(Show.show, a).join(", ") + ']';
    }
})

instance(Show, Object, {
    show: function(a){
        var s = []
        for(var key in a)
            s.push("  " + key + ": " + Show.show(a[key]));
        return "{\n" + a.join(",\n") + "\n}";
    }
})





function uncons(a){
    var isArray = stringAsArray || !(typeof arr == "string"),
        head = isArray ? a[0] : a.charAt(0),
        tail = slice(a, 1),
        res = [head, tail];
    
    res.head = head;
    res.tail = tail;
    res.constructor = Tuple.Tuple2;
    return res;
}




function elemIndex(value, arr) {
    var length = arr.length;   
    if(!length)
        return Maybe.Nothing;
    
    var iEq = getInstance(Eq, typeOf(arr[0]));
    
    for(var i = 0; i < length; ++i)  
      if(iEq.eq(arr[i], value))  
        return  Maybe.Just(i);  
    
    return Maybe.Nothing;
}
    

//lookup :: (Eq a) => a -> [(a,b)] -> Maybe b
function lookup(key, arr){
    var length = arr.length;   
    if (!length)
        return Maybe.Nothing;
    
    for (var i = 0; i < length; ++i)  
      if (arr[i][0] === key)
        return Maybe.Just(arr[i][1]);  
   
    return Maybe.Nothing;
}


function sort(arr){
    function sortFn(a, b){
        var res = Ord.compare(a, b);
        return  res.LT ? -1 :
                res.GT ?  1 :
                res.EQ ?  0 :
                error(sort);
    }
    if(arr.sort)
        return arr.sort(sortFn);
    return slice(arr).sort(sortFn).join("");
}





//-- | The 'nub' function removes duplicate elements from a list.
//-- In particular, it keeps only the first occurrence of each element.
//-- (The name 'nub' means \`essence\'.)
//-- It is a special case of 'nubBy', which allows the programmer to supply
//-- their own equality test.
//nub                     :: (Eq a) => [a] -> [a]
//#ifdef USE_REPORT_PRELUDE
//nub                     =  nubBy (==)
//#else
//-- stolen from HBC
//nub l                   = nub' l []             -- '
//  where
//    nub' [] _           = []                    -- '
//    nub' (x:xs) ls                              -- '
//        | x `elem` ls   = nub' xs ls            -- '
//        | otherwise     = x : nub' xs (x:ls)    -- '
//#endif
function nub(arr){
    function nub_(arr, ls){        
        var isArray = arr.constructor == Array;
            x  = isArray ? arr[0] : arr.charAt(0),
            xs = isArray ? arr.slice(1) : arr.substr(1);
        
        return !arr.length ? [] :
                elem(x, ls) ? nub_(xs, ls) : 
                cons(x, nub_(xs, cons(x,ls)) );
    }
    return nub_(arr, []);
}

//-- | The 'maybe' function takes a default value, a function, and a 'Maybe'
//-- value.  If the 'Maybe' value is 'Nothing', the function returns the
//-- default value.  Otherwise, it applies the function to the value inside
//-- the 'Just' and returns the result.
//maybe :: b -> (a -> b) -> Maybe a -> b
//maybe n _ Nothing  = n
//maybe _ f (Just x) = f x

function maybe(n, f, m){
    if(m.Nothing)
        return n;
    if(m.Just)
        return f(m[0]);
}

//  compare x y = if x == y then EQ
//                  -- NB: must be '<=' not '<' to validate the
//                  -- above claim about the minimal things that
//                  -- can be defined for an instance of Ord:
//                  else if x <= y then LT
//                  else GT

function compare(x, y){
    return x === y ? Ordering.EQ : 
           x <=  y ? Ordering.LT :
                     Ordering.GT;
}

//-- | 'span', applied to a predicate @p@ and a list @xs@, returns a tuple where
//-- first element is longest prefix (possibly empty) of @xs@ of elements that
//-- satisfy @p@ and second element is the remainder of the list:
//-- 
//-- > span (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
//-- > span (< 9) [1,2,3] == ([1,2,3],[])
//-- > span (< 0) [1,2,3] == ([],[1,2,3])
//-- 
//-- 'span' @p xs@ is equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
//
//span                    :: (a -> Bool) -> [a] -> ([a],[a])
//span _ xs@[]            =  (xs, xs)
//span p xs@(x:xs')
//         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
//         | otherwise    =  ([],xs)
function span(p, xs){
    var ret;
    if(!xs.length){
        ret = [xs, xs];
    }else{
        if(p(xs[0])){
            var tmp = span(p, slice(xs, 1))
            ret = [cons(xs[0], tmp[0]), tmp[1]]
        }else
            ret = [[], xs];
    }
    ret.constructor = Tuple.Tuple2;
    return ret;
}


function fst(tuple){
    return tuple[0];
}

function snd(tuple){
    return tuple[1];
}


//-- | 'uncurry' converts a curried function to a function on pairs.
//uncurry                 :: (a -> b -> c) -> ((a, b) -> c)
//uncurry f p             =  f (fst p) (snd p)
function uncurry(f){
    return function(p){
        return f(p[0], p[1]);
    }
}


//-- | @'until' p f@ yields the result of applying @f@ until @p@ holds.
//until                   :: (a -> Bool) -> (a -> a) -> a -> a
//until p f x | p x       =  x
//            | otherwise =  until p f (f x)
function until(p, f, x) {
    return p(x) ? x : until(p, f, f(x));
}


function fix_(f) {
    return function () { return rhs(fix_(f)) }
}
/*
//since it's not lazy this `fix` function doesn't terminate:
function fix_original(f){ return rhs(fix_original(f)) }

//so we have to use an expanded version:
function fix_(f){ 
return function(){ return rhs(fix_(f)) }
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
function fix(f) {
    return (function (g) { return g(g) })
            (function (h) {
                return function () { return f(h(h)).apply(null, arguments) }
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


function isSpace(c){
    return /^\s$/.test(c);
}
function isUpper(c){
    return c.toUpperCase() == c;
}
function isLower(c){
    return c.toLowerCase() == c;
}
function isAlphaNum(c){
    return /^\w$/.test(c);
}
function isAlpha(c){
    return /^\w$/.test(c) && /^\D$/.test(c);
}
function isDigit(c){
    return /^\d$/.test(c);
}
function isHexDigit(c){
    return /^[0-9A-Fa-f]$/.test(c);
}
function isOctDigit(c){
    return /^[0-7]$/.test(c);
}



function foldl(f, initial, arr) {
    for(var i = 0, l = arr.length; i < l; ++i) 
        initial = f(initial, arr[i]);
    return initial;
}

function foldr(f, initial, arr) {
    for(var l = arr.length - 1; l > -1 ; --l) 
        initial = f(arr[l], initial);
    return initial;
}

//-- | 'zip' takes two lists and returns a list of corresponding pairs.
//-- If one input list is short, excess elements of the longer list are
//-- discarded.
//zip :: [a] -> [b] -> [(a,b)]
//zip (a:as) (b:bs) = (a,b) : zip as bs
//zip _      _      = []
function zip(arr1, arr2){
    var res = [], i = 0, l = Math.min(arr1.length, arr2.length);
    for (; i < l; ++i)
        res[i] = [arr1[i], arr2[i]];
    return res;
}

function replicate(n, x){
    for (var ret = [], i = 0; i < n; ++i)
        ret[i] = x;
    return ret;
}

namespace("Prelude", {
     Unit       : Unit
    ,Tuple      : Tuple
    ,Maybe      : Maybe
    ,Ordering   : Ordering
    ,Either     : Either
    ,Eq         : Eq
    ,Ord        : Ord
    ,Functor    : Functor
    ,Monad      : Monad
    ,Show       : Show

    ,foldl      : foldl
    ,foldr      : foldr
    ,zip        : zip
    ,replicate  : replicate
    ,sort       : sort
    ,nub        : nub
    ,maybe      : maybe
    ,lookup     : lookup
    ,span       : span
    ,elemIndex  : elemIndex
    ,uncons     : uncons
    ,compare    : compare
    ,fst        : fst
    ,snd        : snd
    ,uncurry    : uncurry
    ,until      : until
    ,fix        : fix
    ,fix_       : fix_
    ,isSpace    : isSpace
    ,isUpper    : isUpper
    ,isLower    : isLower
    ,isAlpha    : isAlpha
    ,isAlphaNum : isAlphaNum
    ,isDigit    : isDigit
    ,isHexDigit : isHexDigit
    ,isOctDigit : isOctDigit

})
//TODO:
//infix  4  ==, /=, <, <=, >=, >
//infixr 3  &&
//infixr 2  ||
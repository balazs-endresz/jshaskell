/** @license
 * JSParsec - A parser combinator library for JavaScript
 * 
 * http://code.google.com/p/jshaskell/
 * 
 * Copyright (c) 2010 Balazs Endresz (balazs.endresz@gmail.com)
 * Dual licensed under the MIT and GPL licenses.
 * 
 *
 * The initial implementation of some combinators,
 * and the memoization technique is derived from:
 * http://www.bluishcoder.co.nz/2007/10/javascript-parser-combinators.html
 *
 * Most functions should behave like their counterparts in Parsec 3:
 * http://www.haskell.org/haskellwiki/Parsec
 *
 * ------------------------------------------------------------
 * 
 */

/** @license
 * JSHaskell - Some Haskell language features for JavaScript
 * 
 * http://code.google.com/p/jshaskell/
 * 
 * Copyright (c) 2010 Balazs Endresz (balazs.endresz@gmail.com)
 * Dual licensed under the MIT and GPL licenses.
 * 
 * ------------------------------------------------------------
 *
 */;(function(){
;(function(){

// -------------------------------------------------
// Main
// -------------------------------------------------

function namespace(ns, obj){
    return NS[ns] = obj || {};
}

//e.g. module("Text_JSParsec"); importSubmodules("Text_JSParsec", ["Prim", "Char", ..]);
//if not all should be exported with "Text_JSParsec" then use the local attribute,
//and add them to the "Text_JSParsec" module manually: module("Text_JSParsec", {parse:parse, many: many, ...})
function importSubmodules(ns, imp){
    var to  = NS[ns];
    //var imp = [].slice.call(arguments, 1);
    for(var i = 0, l = imp.length; i < l; ++i){
        var obj = NS[ns + "_" + imp[i]];
        for(var name in obj)
            to[name] = obj[name];
    }
    
}



var global = function(){ return this }() || window,
    NS = global["NS"] || (global["NS"] = {}),
    JSParsec = global.JSParsec = {},

    undef,

    _toString    = {}.toString,
    _slice       = [].slice,
    _map         = [].map,
    _filter      = [].filter,
    _indexOf     = [].indexOf,
    _lastIndexOf = [].lastIndexOf,

    isSupported = {
        //for non browser environments
        setTimeout : typeof setTimeout != "undefined",
        
        //to spare an extra function call with type class methods
        funDecomp  : function(){ return this }.toString().indexOf("this") != -1,

        //for using Array.prototype methods on strings
        stringAsArray : !!"a"[0],

        //determines if object properties are enumerated in order
        seqEnum: function(){
            var obj = {9:1, d:1, b:1, 0:1, a:1, c:1},
                arr = [9,  "d", "b",  0,  "a", "c"],
                i = 0;
            for(var key in obj)
                if(key != arr[i++])
                    return false;
            return true;
        }()
    },
    isAllowed = {
        funDecomp  : true,
        stringAsArray : true,

        //e.g. using TypeClass.Type.method instead of getInstance(TypeClass, Type).method
        funNameRef : true,
        
        //TODO: depth
        stackTrace : true
    },
    jsenv = { //TODO: not used yet
        isSupported: isSupported,
        isAllowed: isAllowed
    };

global.global = global;
global.namespace = namespace;
global.importSubmodules = importSubmodules;

//function isType(str, a){ return _toString.call(a) == "[object " + str + "]" }

function isArray(a){ return _toString.call(a) == "[object Array]" }

function isDefined(x){ return x !== undef }

function strictEq(a, b){ return a === b }
function strictNe(a, b){ return a !== b }

function and(a, b){ return a && b }
function or (a, b){ return a || b }

function lt(a, b){ return a < b  }
function le(a, b){ return a <= b }
function gt(a, b){ return a > b  }
function ge(a, b){ return a >= b }

function negate(a){
    return -a;
}

function not(a){
    return (a === true)  ? false :
           (a === false) ? true  : error(not);
}

//TODO: null ?
function typeOf(a){
    var ctr = a.constructor;
    if(ctr)
        return ctr;
    var type = typeof a;
    return  type == "string"   ? String   :
            type == "number"   ? Number   :
            type == "boolean"  ? Boolean  :
            type == "function" ? Function :
            undef;
}

function curry(fn){
    function ret(){
        var args = slice(arguments);
        return args.length >= (fn._length === undef ? fn.length : fn._length) ? fn.apply(null, args) : 
            function(){
                return ret.apply( null, args.concat(slice(arguments)) );
            };
    }
    return ret;
}

var id = function(x){ return x };

//const_ can be called only with one argument at a time:
function const_(x){ return function(_){ return x } }
//but this would work with two arguments as well: const_(x, y)
//var const_ = curry(function(x, _){ return x });


//e.g. eta(error, "Type error") returns a function, which calls error
//with the rest of the arguments, throwing an exception
function eta(f){ 
    var args = slice(arguments, 1)
    return function(){
        return f.apply(null, args)
    }
}

function lazy(f){
    return function(){
        return f().apply(null, arguments);
    }
}

var call = curry(function(a, b){ return a(b) });

function stackTrace(){
    function st(f){
        return !f ? [] : [[f.name || f._name || f, f.arguments]].concat(st(f.caller));
    }
    return st(arguments.callee.caller);
}

function error(a){
    var fnName = (a instanceof Function) ? (a.name || a._name) : null,
        msg = fnName || a || "Unspecified error",
        err = new Error(msg);
    try{
        err.stackTrace = stackTrace();
    }catch(_){}
    err.message = msg;
    throw err;
}


function extend(a, b){
    for(var key in b)
        a[key] = b[key];
    return a;
}

function _namespace(){
    var o, d;
    map(function(v) {
        d = v.split(".");
        o = window[d[0]] = window[d[0]] || {};
        map(function(v2){
            o = o[v2] = o[v2] || {};
        }, d.slice(1));
    }, arguments);
    return o;
}


// -------------------------------------------------
// Basic List/String functions
// -------------------------------------------------

var stringAsArray = !!"a"[0]; //TODO

var slice = stringAsArray ? 
    function(arr, i1){ return _slice.call(arr, i1 || 0) } :
    function(arr, i1){
        var isString = typeof arr == "string";
        if(isString){
            arr = arr.split("");
            if(!i1)
                return arr;
        }
        return _slice.call(arr, i1 || 0);
    }

function indexOf(value, arr) {
    if(arr.indexOf)
        return arr.indexOf(value);
    
    var length = arr.length;   
    if(!length)
        return -1;
    
    for(var from = 0; from < length; from++)  
      if(arr[from] === value)  
        return from;  
   
    return -1;  
}

function lastIndexOf(value, arr) {
    if(arr.lastIndexOf)
        return arr.lastIndexOf(value);

    var length = arr.length;
    if (!length)
        return -1;

    for (var from = length - 1; from > -1; --from)
      if (arr[from] === value)
        return from;

    return -1;
}


function isort(arr){
    if(arr.sort)
        return arr.sort();
    return slice(arr).sort().join("");
}


function map(f, arr){
    var res = [], i = 0, l = arr.length;
    for(; i < l; ++i)
        res[i] = f(arr[i]);
    return res;
}

var imap = _map ?
    function(f, arr){ return _map.call(arr, f) } :
    function(f, arr){    
        var res = [], i = 0, l = arr.length;
        for(; i < l; ++i)
            res[i] = f(arr[i], i, arr);
        return res;
    }

function filter(f, arr) {
    var res = [], i = 0, l = arr.length;
    for(; i < l; ++i)
        if(f(arr[i]))
            res.push(arr[i]);
    return res;
}

var ifilter = _filter ?
    function(f, arr){ return _filter.call(arr, f) } :
    function (f, arr){
        var res = [], i = 0, l = arr.length;
        for(; i < l; ++i)
            if(f(arr[i], i, arr))
                res.push(arr[i]);
        return res;
    }


// -------------------------------------------------
// TODO: these should be in separate modules
// -------------------------------------------------


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

function drop(n, a){
    return a.substring ?
        a.substring(n || 0) :
        _slice.call(a, n || 0);
}

function take(n, a){
    return a.substring ?
        a.substring(0, n) :
        _slice.call(a, 0, n);
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




function compose(fst, snd){
    return function(){
        return fst(snd.apply(null, arguments));
    };
}

//this is the same as (.) in Haskell:
//the inner function receives only the first argument
function compose1(fst, snd){
    return function(a){ 
        var args = slice(arguments, 1);
        args.unshift(snd(a));
        return fst.apply(null, args);
    };
}
//(.) :: (b -> c) -> (a -> b) -> a -> c
//(.) f g x = f (g x)

function flip(fn){
    return function(a, b){ return fn(b, a) };
}

function cons(x, xs){
    if(typeof x == "string" && typeof xs == "string")
        return x + xs;
    
    return [x].concat(xs);
}


function consJoin(x, xs){
    if(typeof x == "string" && typeof xs == "string")
        return x + xs;
    
    return x + xs.join("");
}


function replicate(n, x){
    for (var ret = [], i = 0; i < n; ++i)
        ret[i] = x;
    return ret;
}



//returns True if a list is empty, otherwise False
function null_(a){
    return !a.length;
}


function head(a){
    if(!a.length)
        error(head);
    return a.charAt ? a.charAt(0) : a[0];
}

function append(a, b) {
    var aIsArray = a.constructor == Array,
        bIsArray = b.constructor == Array;
    if(!aIsArray) // first is string
        return a.concat(bIsArray ? b.join("") : b);
    else // first is array
        return a.concat(bIsArray ? b : b.split(""));
}

function concat(arr){
    return foldr(function(a, b){ return a.concat(b) }, [], arr);
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


function elem(x, xs){
    return (xs.indexOf ? xs.indexOf(x) : indexOf(x, xs)) != -1; //TODO
}

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


function digitToInt(c){
    var res = parseInt(c, 16);
    return isNaN(res) ? error(digitToInt) : res;
}


var toInteger = parseInt; //TODO

var fromInteger = id; //TODO

var fromIntegral = id; //TODO

function range(lower, upper){
    return {
        indexOf: function(ch){ return (ch >= lower && ch <= upper) ? true : -1 },
        toString: function(){ return "range(" + lower + ", " + upper + ")" }
    };
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

function readHex(str){
    return parseInt(str.join ? str.join("") : str, 16);
}

function readOct(str){
    return parseInt(str.join ? str.join("") : str, 8);
}

// https://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Objects/String/fromCharCode
// String.fromCharCode() alone cannot get the character at such a high code point  
// The following, on the other hand, can return a 4-byte character as well as the   
//   usual 2-byte ones (i.e., it can return a single character which actually has   
//   a string length of 2 instead of 1!)  
//alert(fixedFromCharCode(0x2F804)); // or 194564 in decimal  
  
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
var chr = String.fromCharCode;

// https://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Global_Objects/String/charCodeAt
function ord(s){
    return s.charCodeAt(0);
}

var round = Math.round;


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




namespace("Haskell_Main", {
    curry       : curry,
    const_      : const_,
    isArray     : isArray,
    isDefined   : isDefined,
    slice       : slice,
    foldl       : foldl,
    foldr       : foldr,
    map         : map,
    imap        : imap,
    filter      : filter,
    ifilter     : ifilter,
    indexOf     : indexOf,
    lastIndexOf : lastIndexOf,
    zip         : zip,
    sort        : sort,
    isort       : isort,

    nub         : nub,
    maybe       : maybe,
    compare     : compare,
    compose     : compose,
    compose1    : compose1,
    call        : call,
    id          : id,
    flip        : flip,
    cons        : cons,
    consJoin    : consJoin,
    replicate   : replicate,
    negate      : negate,
    null_       : null_,
    elem        : elem,

    isSpace     : isSpace,
    isUpper     : isUpper,
    isLower     : isLower,
    isAlpha     : isAlpha,
    isAlphaNum  : isAlphaNum,
    isDigit     : isDigit,
    isHexDigit  : isHexDigit,
    isOctDigit  : isOctDigit,

    digitToInt  : digitToInt,
    range       : range,
    extend      : extend,
    namespace   : namespace,
    toInteger   : toInteger,
    fromInteger : fromInteger,
    fromIntegral: fromIntegral,
    fst         : fst,
    snd         : snd,
    uncurry     : uncurry,
    lookup      : lookup,
    readHex     : readHex,
    readOct     : readOct,
    chr         : chr,
    round       : round,
    typeOf      : typeOf
});/// <reference path="Main.js" />

// -------------------------------------------------
// Type classes
// -------------------------------------------------

//this is used for mapping a type to an object,
//which contains the instance functions
function Map() {
    this.keys = [];
    this.values = [];
}
Map.prototype.put = function (key, val) {
    this.keys.push(key);
    this.values.push(val);
}
Map.prototype.get = function (key) {
    var i = indexOf(key, this.keys);
    if (i == -1) return;
    return this.values[i];
}

//if this is used in a type signature then from then on the types are not checked
var VARARG = {};

//TODO: define aliases for operators
//TODO: handle non-function types (e.g. Bounded)
function typeclass(name, tvar) {

    var tcls = {};

    tcls._context = [];
    //tcls._fnTypes = {};
    tcls._name = name;
    tcls._dict = new Map();

    tcls.context = function () {
        this._context = slice(arguments);
        delete tcls.context;
        return this;
    };

    tcls.types = function (obj) {
        this._fnTypes = obj;
        delete tcls.types;
        return this;
    };
    
    tcls.impl = function (obj) {
        this._default = obj;

        for (var name in tcls._fnTypes) (function (name, type) {

            //constants can be accessed only from the instances
            //see: asTypeOf, getInstance, typeOf
            //but defaults are allowed
            if ((type !== undef) && (type.constructor != Array)) {
                tcls[name] = ((typeof obj == "function") ? obj() : obj)[name];
                return;
            } else 
            tcls[name] = function () {
                var args = arguments;
                var tctr;

                if((type === undef) || (type === VARARG))
                    error("There's no type signature for " + tcls._name + "." + name +
                          ", it can be accessed only by: getInstance(" + 
                          tcls._name + ", SomeType" + ")." + name);
                
                var matched;
                //TODO: optimize
                var matches = ifilter(function(arg, i) {
                    var t = type[i];
                    if(matched || (t == VARARG))
                        return (matched = true);

                    var ctr = typeOf(arg);
                    
                    if (t == tvar) {
                        tctr = tctr || ctr;
                        return tctr == ctr;
                    }
                    else
                        return ctr == t;
                }, args);

                if (!matched && (matches.length != type.length - 1))
                    error("Type error in " + tcls._name + "." + name);
                

                var inst = tcls._dict.get(tctr); //TODO: tcls[tctr.name || tctr._name] || 
                if (!inst)
                    error("No " + tcls._name + " instance for " + (tctr.name || tctr._name));
                return inst[name].apply(null, args);
            };
        } (name, tcls._fnTypes[name]));

        delete tcls.impl;
        return this;
    };

    return tcls;
}

function instance(tcls, tctr, defs) {
    //check if superclasses are defined
    imap(function (cls) {
        if (!cls._dict.get(tctr))
            throw "No " + (cls._name || "") + " instance for " + (tctr.name || tctr._name);
    }, tcls._context);
    //create instance
    var inst = {};
    //create reference to the type constructor
    inst._type = tctr;
    //store the instance on the type class in a map :: Map Type (Instance Type)
    tcls._dict.put(tctr, inst);
    //create definitions by optionally passing the instance,
    //so that methods can refernce each other directly
    defs = (typeof defs == "function") ? defs(inst) : (defs || {});
    //copy instance methods or defaults to the instance object 
    var defaults = tcls._default;
    defaults = (typeof defaults == "function") ? defaults(inst) : defaults;
    for(var name in tcls._fnTypes)
        inst[name] = defs[name] || defaults[name];
    return inst;
}


//Ord.compare(1,2) -> Ordering.LT

function getInstance(tcls, tctr) {
    return tcls._dict.get(tctr);
}
//var OrdNum = getInstance(Ord, Number)
//OrdNum.compare(1,2) -> Ordering.LT

function asTypeOf(tcls, method, value) {
    return tcls._dict.get(typeOf(value))[method];
    //return getInstance(tcls, typeOf(value))[method];
}

//#region comment
/*
//in contrast with Haskell `asTypeOf` takes a type class, and a method name
//instead of a function as a first argument
//it's essential for getting a constant value (not a function) from a type class
//e.g. since Bounded.maxBound is not a function, but it has different values for each type, 
//Bounded.maxBound is undefined, maxBound exists only on each instance
//there are two ways of getting the right value, both are identical:

function getLastIndex1(a){
    var iBounded = getInstance(Bounded, typeOf(a));
    return Enum.fromEnum( iBounded.maxBound )    
}

function getLastIndex2(a){
    return Enum.fromEnum( asTypeOf(Bounded, "maxBound", a) )
}

//the same as the second one:
var getLastIndex3 = compose1(Enum.fromEnum, curry(asTypeOf)(Bounded, "maxBound"))



//if more functions are used from the same type class (inside a loop for example)
//then getting the instance explicitly is more efficient, but not necessary:
function getRangeLength(a){
    var type     = typeOf(a),
        iBounded = getInstance(Bounded, type),
        iEnum    = getInstance(Enum   , type);
        
    return iEnum.fromEnum(iBounded.maxBound) - iEnum.fromEnum(iBounded.minBound)
}

//there's no expicit Enum instance here, so it will be resolved twice:
function getRangeLength_slower(a){
    var type     = typeOf(a),
        iBounded = getInstance(Bounded, type);

    return Enum.fromEnum(iBounded.maxBound) - Enum.fromEnum(iBounded.minBound)
}

*/
//#endregion

namespace("Haskell_Typeclass", {
     typeclass   : typeclass
    ,VARARG      : VARARG
    ,instance    : instance
    ,getInstance : getInstance
    ,asTypeOf    :asTypeOf
})/// <reference path="Main.js" />

// -------------------------------------------------
// Operators
// -------------------------------------------------

function infixl(strength){ return ["l", strength] }
function infixr(strength){ return ["r", strength] }
function infix (strength){ return ["x", strength] }

function getFixity(opstr){
    if(!opstr)
        return;
    if(opstr._String)
        return;
    var op = operators[opstr];
    if(opstr._Op && !op)
        return ["l", 9];

    return op && op.fixity;
}

function getFixityDir(opstr){
    if(!opstr)
        return;
    if(opstr._String)
        return;
    var op = operators[opstr];
    if(opstr._Op && !op)
        return "l";

    return op && (op.fixity[0] || "l" );
}

function getFixityStrn(opstr){
    if(!opstr)
        return;
    if(opstr._String)
        return;
    var op = operators[opstr];
    if(opstr._Op && !op)
        return 9;

    return op && (isDefined(op.fixity[1]) ? op.fixity[1] : 9);
}


var operators = {
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
};



// -------------------------------------------------
// Array expressions
// -------------------------------------------------

function Recurse(){}
var recurse = new Recurse();

// -- see usage in Char

function splice_args(args, i, rec){
    var op;
    if(args[i]._Op){
        delete args[i]._Op;
        op = args[i];
    }else
        op = operators[args[i]].func;
    
    var item = op(args[i-1], args[i+1]);
    args.splice(i-1, 3 , item);
    return resolve(args, rec);
}

//In array-expressions if the square brackets are not intended for groupping subexpressions
//but an actual array is needed the it should be wrapped in `no` (i.e. "not operator")
//just like if there's a chance that a string might be the same one as an operator.
//
//Conversely, functions can be used as operators by wrapping them in `op`.
//
//var p = ex(string, no("<|>"), op(parserPlus), return_, no([])).resolve();
//
//But usually this can be done by simply using the native javascript call operator:
//
//var p = ex(string("<|>"), "<|>", return_([])).resolve();


function no(a){
    if(typeof a == "string"){
        var string = new String(a);
        string._String = true;
        return string;
    }else{
        a._Array = true;
        return a;
    }

}

function op(fn){
    fn._Op = true;
    return fn;
}


//TODO: reject multiple infix operators in the same expression
function resolve(args, rec){
    //recurse on nested array-expressions or callstreams
    args = imap(function(e){
        if(!e)
            return e;
        if(e._Array){
            delete e._Array;
            return e;
        }
        return (e.constructor == Array) ? resolve(e, rec) :
                    (e.CallStream) ? e.resolve(rec) : e;
    }, args);
    
    //inject recursive calls
    if(rec)
        args = map(function(e){return e instanceof Recurse ? rec : e}, args);
    
    //execute functions between operators
    var fna = [], fn, newfna = [], i = 0, l = args.length;
    for(; i < l; ++i){
        var e = args[i], isOp = false;
        
        if(operators[e])
            isOp = true;
        if(e && e._String){
            isOp = false;
            e = e.toString();
        }
        if(e && e._Op)
            isOp = true;

        if(!isOp && i != (l-1))
            fna.push(e);
        else{
            if(i == (l-1))
                fna.push(e);
            if(fna.length> 1){
                //if(!fna[0] || !fna[0].apply)
                //    throw ["Expecting function in array-expression instead of " + fna[0], args, fna];
                var functionInArrayExpr = fna[0];
                fn = functionInArrayExpr.apply(null, fna.slice(1));
            }
            else
                fn = fna[0];
            newfna.push(fn);
            if(i != l-1)
                newfna.push(e);
            fna = [];
        }
    }
    args = newfna;

    //execute operators
    var dir    = imap(getFixityDir , args),
        strn   = imap(getFixityStrn, args),
        max    = ifilter(isDefined, strn).sort().pop(),
        maxfst = indexOf(max, strn),
        maxlst = lastIndexOf(max, strn);
    
    return  dir[maxfst] == "l" ? splice_args(args, maxfst, rec) :
            dir[maxlst] == "r" ? splice_args(args, maxlst, rec) :
            dir[maxfst] == "x" ? splice_args(args, maxfst, rec) :
            args[0];
}

//TODO: remove
//Array.prototype.resolve = function(){ return resolve(this) };

//an interface for array-expressions that handles self recursion, and evalutes lazily
function exl(type){
    return function(){

        function rec(){ return p.apply(null, arguments) }

        var line = arguments, p, resolved;

        function expr(){
            return (resolved ? p : expr.resolve()).apply(null, arguments);
        }

        expr.resolve = function(_rec){
            if(resolved)
                return p;
            p = resolve(line, _rec || rec);
            line = null;
            resolved = true;
            return p;
        };

        expr.CallStream = true;
        expr.constructor = type;
        return expr;
    }
}

function exs(){ return resolve(slice(arguments)) };

namespace("Haskell_Expression", {
    operators : operators,
    infix     : infix,
    infixl    : infixl,
    infixr    : infixr,
    op        : op,
    no        : no,
    //ex        : ex,
    resolve   : resolve,
    recurse   : recurse,
    Recurse   : Recurse,
    exl       : exl,
    exs       : exs
})/// <reference path="Expression.js" />

function Scope(outerScope){
    this.scope = outerScope;
}

//#region comment
/*

The scope chain could be implemented with prototypal inheritance,
which would be easier to use (no scope.scope.x), but above a limit
it crashes Firefox (~90k) and Chrome (~1M) as well:

function newScope(outerScope){
    function Scope(){}
    Scope.prototype = outerScope;
    return new Scope();
}

for(var i = 0, scope; i < 1000000; ++i){
    function S(){};
    S.prototype = scope;
    scope = new S();
}

Also, this takes twice as much time as simply creating a new object.

*/
//#endregion

function createDo(inst){
    return function(){

        function rec(){ return monad.apply(null, arguments) }

        var lines = [], monad, resolved;

        lines.push(resolve(arguments, rec));

        function line(scope){ //TODO: curry -> _length
            if(resolved || (scope instanceof Scope))
                return (resolved ? monad : line.resolve()).apply(null, arguments);
        
            lines.push(resolve(arguments, rec));
            return line;
        }

        line.resolve = function(){
            if(resolved)
                return monad;
            monad = inst.do_.apply(null, lines);
            resolved = true;
            lines = null;
            return monad;
        };

        line.CallStream = true;
        line.constructor = inst._type;
        return line;
    }
}


namespace("Haskell_Do", {
    createDo: createDo
})/// <reference path="Main.js" />

// -------------------------------------------------
// Algebraic Data Types
// -------------------------------------------------

var accessors = {};

function accessor(prop, obj){
    return obj ? obj[prop] : function(obj){ return obj[prop] }
}

function Record(){}
var record = new Record();

function ADT(){}

function adtToString(type){
    return function(){
        var acc = [], rec = this._recordset;
        if(rec && (rec.constructor != Array)){
            for(var name in rec){
                var item = (type ? (rec[name].name || rec[name]) : this[name]);
                if(!type && (item instanceof Function))
                    item = (item.constructor != Function) ?
                                        item.constructor.name :
                                        "Function(" + item.name + ")";
                acc.push(name + (type ? " :: " : " = ") + item );
            }
            var indent = replicate(this._dataConstructor.length + 2, " ").join("");
            acc = "{" + acc.join("\n" + indent + ",") + "\n" + indent +"}";
        }else{
            for(var i = 0; i in this; i++)
                acc.push(type ? (rec[i].name || rec[i]) : this[i]);
            acc = acc.join(" ");
        }
        return "(" + this._dataConstructor + (acc ? " " : "") + acc + ")";
    };
}

ADT.prototype.toString = adtToString();

ADT.prototype.dataConstructorToString = adtToString(true);

//defined later
ADT.prototype.update = eta(error);

function dataError(checkType, valueType, name, typeName, constr) {
    error("Type error: expecting " + checkType.name + 
          " instead of " + valueType.name +
          " in the argument '" + name + "' of " + typeName + "." + constr);
}


function data(type, constr){
    var typeName = type.name;
    if(type._constructors)
        throw "Type constructor has been already defined: '" + typeName + "'";
    type._constructors = constr;

    type.prototype = new ADT();

    for(var i = 0, l = constr.length; i < l; ++i){
        var single = typeof constr[i] != "object",
            name =  single  ? constr[i] : constr[i][0];
        if(name in type)
            throw "The name of the data constructor (" +
                    typeName + "." + name + ") is invalid!";

        type[name] = single ? mkDataCtr(name)() : mkDataCtr(name, slice(constr[i], 1));
        if(!single)
            type[name]._length = constr[i].length - 1; //for currying
    }
    
    function mkDataCtr(constr, fields){
        var recordDef = fields && typeof fields[0] == "object";
        var recordSet = recordDef && fields[0];

        if(recordDef){
            //this is used for simulating the record syntax
            //but it's not guaranteed to work in every javascript environment
            //beacause it assumes that the keys of objects are iterated
            //in the order they were defined,
            //but that is not part of the ECMAScript standard
            var i = 0, fieldName, propToIndex = {}, indexToProp = {};
            for (fieldName in recordSet) {
                var fstchr = fieldName.charAt(0);
                if ((fstchr == "_") || (fstchr == fstchr.toUpperCase()))
                    throw typeName + "." + name + "." + fieldName + 
                          ": record names can't start with underscore or uppercase letters";
                if (fieldName in ({ update: 1 }))
                    throw typeName + "." + name + "." + fieldName + ": record name is invalid";
                
                propToIndex[fieldName] = i;
                indexToProp[i] = fieldName;
                i++;
            }
        }

        function create(_isrecord, rec){
            var isrecord = (_isrecord instanceof Record),
                adt = new type(),
                value, valueType, checkType;

            adt.constructor = type;
            adt._recordset = (recordDef && fields[0]) || fields;
            adt._dataConstructor = constr;
            adt[constr] = true;

            if (isrecord) {
                for (var name in rec) {
                    value = rec[name];

                    if (!recordDef)
                        error("Records are not defined for " + typeName + "." + constr);

                    if (!(name in recordSet))
                        error("The accessor '" + name + "' is not defined for " + typeName + "." + constr);

                    checkType = recordSet[name];

                    if (typeof checkType != "string") {
                        valueType = typeOf(value);
                        if (valueType != checkType)
                            dataError(checkType, valueType, name, typeName, constr);
                    }

                    adt[name] = value;
                    adt[propToIndex[name]] = value;


                }
            } else {
                var args = arguments;
                for (var i = 0, l = args.length; i < l; ++i) {
                    value = args[i];

                    checkType = fields[i];

                    if (typeof checkType != "string") {
                        valueType = typeOf(value);
                        if (valueType != checkType)
                            dataError(checkType, valueType, i, typeName, constr);
                    }

                    adt[i] = value;
                    if(recordDef)
                        adt[indexToProp[i]] = value;
                }
            }
            
            //TODO: deifne as separate function
            adt.update = function(newRecords){
                var obj = {};
                for(var n in recordSet)
                    obj[n] = (n in newRecords) ? newRecords[n] : this[n];
                return create(record, obj);
            };
            
            return adt;
        }
        return create;
    }
}

//#region comment

//currently type variables on the lhs cannot be declared, and they are not checked at all:

//  function Maybe(){}
//  data(Maybe, [["Just", "a"], "Nothing"]);

//  var Just    = Maybe.Just;
//  var Nothing = Maybe.Nothing;

//Just("a") instanceof Maybe
//Just("a").constructor == Maybe
//
//Just("a").Just == true //can be used in place of pattern matching: if(maybeval.Just) ... if(maybeval.Nothing) ...
//Just("a")[0] == "a"    //access arguments by index
//
//Nothing.Nothing == true
//Nothing == Nothing


// using record syntax:
 
/*
function Type(){}

data(Type, [["Constr1", Number, "a"]
            ,"Constr2"
            ,["Constr3", {acc: Number}]
            ,["Constr4", Number]
            ]);

//in Haskell:
data Number = ... -- javascript number type
data Type a = Constr1 Number a
            | Constr2
            | Constr3 {acc :: Number}
            | Constr4 Number
*/

//Type.Constr3(record, {acc:1}).Constr3 == true
//Type.Constr3(record, {acc:1}).acc == 1
//Type.Constr3(record, {acc:1})[0] == 1
//Type.Constr3(1)[0] == 1
//Type.Constr3(1).a == 1
//Type.Constr2.Constr2 == true
//Type.Constr2 == Type.Constr2


//record update (creates a new object):

//function T(){}
//data(T, [["C", {a: String,b: String}]]);
//T.C("2","3").update({a:"4"}).a == "4"

//#endregion

namespace("Haskell_ADT", {
    data      : data,
    ADT       : ADT,
    record    : record,
    accessor  : accessor,
    accessors : accessors
});/// <reference path="ADT.js" />
/// <reference path="Do.js" />
/// <reference path="Typeclass.js" />

// -------------------------------------------------
// Basic data types
// -------------------------------------------------

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





namespace("Haskell_DataTypes", {
     Unit      : Unit
    ,Tuple     : Tuple
    ,Maybe     : Maybe
    ,Ordering  : Ordering
    ,Either    : Either
    ,Eq        : Eq
    ,Ord       : Ord
    ,Functor   : Functor
    ,Monad     : Monad
    ,Scope     : Scope
    ,Show      : Show
})
//TODO:
//infix  4  ==, /=, <, <=, >=, >
//infixr 3  &&
//infixr 2  ||
}());;(function(){
/// <reference path="Haskell/Main.js" />
/// <reference path="Haskell/ADT.js" />
/// <reference path="Haskell/Typeclass.js" />
/// <reference path="Haskell/Expression.js" />
/// <reference path="Haskell/Do.js" />
/// <reference path="Haskell/DataTypes.js" />

namespace("Haskell")
importSubmodules("Haskell",
    ["Main"
    ,"ADT"
    ,"Typeclass"
    ,"Expression"
    ,"Do"
    ,"DataTypes"
    ])


}());;(function(){
;var curry = NS['Haskell'].curry, const_ = NS['Haskell'].const_, isArray = NS['Haskell'].isArray, isDefined = NS['Haskell'].isDefined, slice = NS['Haskell'].slice, foldl = NS['Haskell'].foldl, foldr = NS['Haskell'].foldr, map = NS['Haskell'].map, imap = NS['Haskell'].imap, filter = NS['Haskell'].filter, ifilter = NS['Haskell'].ifilter, indexOf = NS['Haskell'].indexOf, lastIndexOf = NS['Haskell'].lastIndexOf, zip = NS['Haskell'].zip, sort = NS['Haskell'].sort, isort = NS['Haskell'].isort, nub = NS['Haskell'].nub, maybe = NS['Haskell'].maybe, compare = NS['Haskell'].compare, compose = NS['Haskell'].compose, compose1 = NS['Haskell'].compose1, call = NS['Haskell'].call, id = NS['Haskell'].id, flip = NS['Haskell'].flip, cons = NS['Haskell'].cons, consJoin = NS['Haskell'].consJoin, replicate = NS['Haskell'].replicate, negate = NS['Haskell'].negate, null_ = NS['Haskell'].null_, elem = NS['Haskell'].elem, isSpace = NS['Haskell'].isSpace, isUpper = NS['Haskell'].isUpper, isLower = NS['Haskell'].isLower, isAlpha = NS['Haskell'].isAlpha, isAlphaNum = NS['Haskell'].isAlphaNum, isDigit = NS['Haskell'].isDigit, isHexDigit = NS['Haskell'].isHexDigit, isOctDigit = NS['Haskell'].isOctDigit, digitToInt = NS['Haskell'].digitToInt, range = NS['Haskell'].range, extend = NS['Haskell'].extend, namespace = NS['Haskell'].namespace, toInteger = NS['Haskell'].toInteger, fromInteger = NS['Haskell'].fromInteger, fromIntegral = NS['Haskell'].fromIntegral, fst = NS['Haskell'].fst, snd = NS['Haskell'].snd, uncurry = NS['Haskell'].uncurry, lookup = NS['Haskell'].lookup, readHex = NS['Haskell'].readHex, readOct = NS['Haskell'].readOct, chr = NS['Haskell'].chr, round = NS['Haskell'].round, typeOf = NS['Haskell'].typeOf, data = NS['Haskell'].data, ADT = NS['Haskell'].ADT, record = NS['Haskell'].record, accessor = NS['Haskell'].accessor, accessors = NS['Haskell'].accessors, typeclass = NS['Haskell'].typeclass, VARARG = NS['Haskell'].VARARG, instance = NS['Haskell'].instance, getInstance = NS['Haskell'].getInstance, asTypeOf = NS['Haskell'].asTypeOf, operators = NS['Haskell'].operators, infix = NS['Haskell'].infix, infixl = NS['Haskell'].infixl, infixr = NS['Haskell'].infixr, op = NS['Haskell'].op, no = NS['Haskell'].no, resolve = NS['Haskell'].resolve, recurse = NS['Haskell'].recurse, Recurse = NS['Haskell'].Recurse, exl = NS['Haskell'].exl, exs = NS['Haskell'].exs, createDo = NS['Haskell'].createDo, Unit = NS['Haskell'].Unit, Tuple = NS['Haskell'].Tuple, Maybe = NS['Haskell'].Maybe, Ordering = NS['Haskell'].Ordering, Either = NS['Haskell'].Either, Eq = NS['Haskell'].Eq, Ord = NS['Haskell'].Ord, Functor = NS['Haskell'].Functor, Monad = NS['Haskell'].Monad, Scope = NS['Haskell'].Scope, Show = NS['Haskell'].Show;
/// <reference path="../../../../jshaskell/src/Haskell.js" local />

// -------------------------------------------------
// ParseState
// -------------------------------------------------

var undef;

function ParseState(input, index) {
    this.input  = input;
    this.index  = index || 0;
    this.length = input.length - this.index;
    this.cache  = {};
    return this;
}

ParseState.prototype = {

    memoize: false,

    scrollTo: function(index) {
        this.index  = index;
        this.length = this.input.length - index;
        return this;
    },

    scroll: function(index) {
        this.index  += index;
        this.length -= index;
        return this;
    },
    
    dropped: 0, //TODO: cut input periodically if there's no try_

    at: function(index){
        return this.input.charAt(this.index + index);
    },

    substring: function(start, end){
        return this.input.substring(
            start + this.index,
            (end || this.length) + this.index);
    },

    substr: function(start, length){
        return this.input.substring(
            start + this.index,
            length || this.length);
    },

    toString: function(){
        var substr = this.substring(0);
        return 'PS at ' + this.index + ' ' + 
            (substr.length ? '"' + substr + '"' : "Empty"); 
    },

    getCached: function(pid) {
        if(!this.memoize)
            return;

        var p = this.cache[pid];
        if(!p)
            return;

        var result = p[this.index];

        if(!result)
            return;

        //result.remaining === this
        this.index  = result.index;
        this.length = result.length;

        return result;
    },

    putCached: function(pid, index, cached) {
        if(!this.memoize)
            return false;
        
        //cached.remaining === this
        cached.index  = this.index;
        cached.length = this.length;


        var p = this.cache[pid];
        if(!p)
            p = this.cache[pid] = {};

        p[index] = cached;
    }
    
    ,sourceLine: function(pos){
        var m = this.input.substring(0, pos).match(/(\r\n)|\r|\n/g);
        return m ? m.length : 0;
    }

    /*

    //returns a new state object
    ,from: function(index) {
        var r = new ParseState(this.input, this.index + index);
        r.cache  = this.cache;
        r.length = this.length - index;
        return r;
    }

    ,skipWhitespace: function(){
        var m = this.substring(0).match(/^\s+/);
        return m ? this.scroll(m[0].length) : this;
    }

    */
};

function ps(str) {
    return new ParseState(str);
}




// -------------------------------------------------
// Result
// -------------------------------------------------


// remaining: is the remaining string(ParseState) to be parsed
// matched:   is the portion of the string that was successfully matched by the parser
// ast:       is the AST returned by the parse, which doesn't need to be successful
//                this is the value that Functor, Applicative, and Monad functions operate on
// success:   might be true or false
// expecting: contains the value that the parser expected but haven't matched completely or at all
//                It's either a single string, or an object with a property 'string' and 'at'.
//                If it's just a string, then the index can be determined from ParseState.index,
//                else the latter form should be used (this might be changed later!).
//                It might be an array of these values, which represents a choice.


function make_result(ast, success, expecting){
    return  {ast: ast
            ,success: success === undef ? true : success
            ,expecting: expecting
            };
}

var _EmptyOk = make_result(undef);


function _fail(expecting){
    return make_result(undef, false, expecting);
}


function unexpected(name){
    return function(scope, state, k){
        return k(make_result(null, false, {unexpected: name}));
    };
}

//accepts an identifier string, see usage with notFollowedBy
function unexpectedIdent(name){
    return function(scope, state, k){
        return k(make_result(null, false, {unexpected: scope[name]}));
    };
}


function parserFail(msg){
    return function(scope, state, k){
        return k(make_result(undef, false, msg));
    };
};

var fail = parserFail;


function parserZero(scope, state, k){
    return k(make_result(undef, false));
}

var mzero = parserZero;
var empty = mzero;



// -------------------------------------------------
// Parser
// -------------------------------------------------


// Helper function to convert string literals and CallStreams to token parsers
function toParser(p){
    return (typeof p == "string") ? string(p) : 
        isArray(p) ? resolve(p) : p;
}


function trampoline(x){
    while(x && x.func)
        x = x.func.apply(null, x.args || []);
}


function trampolineAsync(x, count){ //TODO: use while
    count = count || 0 ;
    count++;
    
    if(!(x && x.func)){
        count = 0;
        return;
    }

    x = x.func.apply(null, x.args || []);
    
    if(count % 500 == 0 )
        setTimeout(function(){ trampolineAsync(x, count) }, 1);
    else
        trampolineAsync(x, count);
}

function run(p, strOrState, complete, error, async){
    var input = strOrState instanceof ParseState ? strOrState : ps(strOrState);
    (async ? trampolineAsync : trampoline) ({func:p, args:[new Scope(), input, function(result){
        result.state = input;
        delete result.index;
        delete result.length;
        if(!result.success){
            result.error = processError(result.expecting, result.state);
            error && error(result.error);
        }else{
            delete result.error;
            delete result.expecting;
        }
        complete(result);
    }]});
}

function processError(e, s, i, unexp){
    var index = i === undefined ? s.index : i;

    if(typeof e == "string"){
        var lines = s.input.split("\n"),
            linecount = lines.length,
            restlc = s.input.substr(index).split("\n").length,
            line = linecount - restlc + 1,
            lindex = index - lines.splice(0,line-1).join("\n").length,
            unexpMsg = unexp || s.input.substr(index, e.length).substr(0, 6);
        return 'Unexpected "' + (unexpMsg.length ? unexpMsg : "end of input") +  
                (unexp ? "" : ('", expecting "' + e)) + 
                '" at line ' + line + ' char ' + lindex;
    }

    if(isArray(e)){
        var err = map(function(er){ return typeof er == "object" ? er.expecting : er }, e);
        return processError(err.join('" or "'), s);
    }else if(typeof e == "object")
        return processError(e.expecting, s, e.at, e.unexpected);
}

var parser_id = 0;

function Parser(){}

function parserBind(p, f){ 
    return function(scope, state, k){
        return {func:p, args:[scope, state, function(result){
            if(result.success){
                return {func:f(result.ast), args:[scope, state, k]}
            }else{
                return k(result);
            }
        }]};
    };
}


var do2 = function(p1, p2){
    function fn(scope, state, k){
        return { func: p1, args: [scope, state, function(result){
            return result.success ? p2(scope, state, k) : k(result); //TODO: p2
        }]};
    }
    fn.constructor = Parser;
    return fn;
};

//TODO: foldl
var do_ = function(p1, p2, p3 /* ... */){
    var parsers = map(toParser, arguments);
    function fn(outerScope, state, k){
        var scope = new Scope(outerScope),
            i = 1,
            l = parsers.length,
            result = parsers[0];

        for(; i < l; ++i)
            result = do2(result, parsers[i]);

        return result(scope, state, k);
    }
    fn.constructor = Parser;
    return fn;
};


function bind(name, p){ 
    if(name == "scope")
        throw "Can't use 'scope' as an identifier!";
    return function(scope, state, k){
        return { func: p, args: [scope, state, function(result){
            if(result.success)
                scope[name] = result.ast;
            result = extend({}, result);
            
            return k(result);
        }]};
    };
}


function ret(name, more){
    var args;
    if(more) 
        args = slice(arguments);

    return function(scope, state, k){

        return { func: function(){
            var ast, type = typeof name;
            //if(args){
            //  ast =  resolve(resolveBindings(args, scope));
            //}else 
            if(type == "string"){
                if(!(name in scope))
                    throw 'Not in scope: "' + name + '"';
                ast = scope[name];      
            }else
                ast = name(scope);

            return k(make_result(ast));

        }};
    };
}

function resolveBindings(arr, scope){
    return isArray(arr) ?
        map(function(e){ return (e in scope) ? scope[e] : resolveBindings(e) }, arr)
        : arr;
}

function withBound(fn){
    var args = slice(arguments, 1);
    return function(scope){
        return fn.apply(null, map(function(e){ return scope[e] }, args));
    };
}

var returnCall = compose(ret, withBound);

function getPosition(scope, state, k){
    return k(make_result(state.index));
}

var getParserState = getPosition; //TODO?

function setPosition(id){
    var type = typeof id;
    return function(scope, state, k){
        state.scrollTo(type == "string" ? scope[id] : id);
        return k(_EmptyOk);
    };
}

var setParserState = setPosition; //TODO?

//in contrast with Haskell here's no closure in the do_ notation,
//it's simulated with `bind` and `ret`,
//this function does what `pure` and `return` do in Haskell
function parserReturn(value){
    return function(scope, state, k){
        return k(make_result(value));
    };
}

var return_ = parserReturn;
var pure = return_;


function ap(a, b){
    return do_(bind("a", a), bind("b", b), ret(function(scope){ return scope.a(scope.b) }));
}

//liftM f m1 = do { x1 <- m1; return (f x1) }
function liftM(f, m1){
    return do_(bind("x1", m1), returnCall(f, "x1"));
}
var parsecMap = liftM;
var fmap   = parsecMap;
var liftA  = fmap;

//liftM2 f m1 m2 = do { x1 <- m1; x2 <- m2; return (f x1 x2) }
function liftM2(f, m1, m2){
    return do_(bind("x1", m1), bind("x2", m2), returnCall(f, "x1", "x2"));
}
var liftA2 = liftM2;

//liftM3 f m1 m2 m3 = do { x1 <- m1; x2 <- m2; x3 <- m3; return (f x1 x2 x3) }
function liftM3(f, m1, m2, m3){
    return do_(bind("x1", m1), bind("x2", m2), bind("x3", m3),
               returnCall(f, "x1", "x2", "x3"));
}
var liftA3 = liftM3;



//var skip_fst = function(p1, p2){ return liftA2(const_(id), p1, p2) };
//function skip_fst(p1, p2){ return do_(p1, p2) }
var skip_fst = do2;

//var skip_snd = function(p1, p2){ return liftA2(const_, p1, p2) };
function skip_snd(p1, p2){ return do_(bind("a", p1), p2, ret("a")) }



function parserPlus(p1, p2){
    function fn(scope, state, k){
        return {func: p1, args:[scope, state, function(result){
            var errors =  [];

            function handleError(result){
                var err = result.expecting;
                if(err){
                    if(isArray(err))
                        errors = errors.concat(err);
                    else
                        errors.push(err);
                }
                if(!result.success)
                    result.expecting = errors;
                else
                    delete result.expecting;
            }
            
            handleError(result);
            if(result.ast !== undef)
                return {func:k, args: [result]};
            else
                return {func: p2, args: [scope, state, function(result){
                    handleError(result);
                    return k(result);
                }]};
            
        }]};
    }
    fn.constructor = Parser;
    return fn;
}

// 'parserPlus' is a parser combinator that provides a choice between other parsers.
// It takes any number of parsers as arguments and returns a parser that will try
// each of the given parsers in order. The first one that matches some string 
// results in a successfull parse. It fails if all parsers fail.
function parserPlusN(p1, p2, p3 /* ... */){
    var parsers = map(toParser, arguments);
    return function(scope, state, k){
        var i = 1,
            l = parsers.length,
            result = parsers[0];
        
        for(; i < l; ++i)
            result = parserPlus(result, parsers[i]);

        return result(scope, state, k);
    };
}

var mplus = parserPlus;



//accepts multiple parsers and returns a new parser that
//evaluates them in order and
//succeeds if all the parsers succeeded
//fails when a parser fails but returns the array of previous ASTs in the result
function tokens(parsers){
    return function(scope, state, k){
        var i = 0,
            ast = [],
            length = parsers.length;
        
        function next(parser){
            return function(scope, state, k){
                return {func:parser, args:[scope, state, function(result){
                    i++;
                    if(!result.success)
                        return k(result);
                    if(result.ast !== undef)
                        ast.push(result.ast);
                    return i < length ? next(parsers[i])(scope, state, k) : k(result);
                }]};
            };
        }

        return {func:next(parsers[i]), args:[scope, state, function(_result){
            var result = extend({}, _result);
            result.ast = ast;
            if(result.success)
                delete result.expecting;
            return k(result);
        }]};
    };
}

function _many(onePlusMatch){
    return function(parser){
        return function(scope, state, k){
            var matchedOne = false,
                ast = [];
            
            function next(parser){
                return function(scope, state, k){
                    return {func:parser, args:[scope, state, function(result){
                        if(!result.success)
                            return k(result);
                            
                        matchedOne = true;
                        if(result.ast !== undef)
                            ast.push(result.ast);
                                
                        return next(parser)(scope, state, k);
                    }]};
                };
            }
    
            return {func:next(parser), args:[scope, state, function(_result){
                var result = extend({}, _result);
                result.success = !onePlusMatch || (matchedOne && onePlusMatch);
                result.ast = ast;
                if(result.success)
                    delete result.expecting;
                else
                    result.ast = undef;
                return k(result);
            }]};
        };
    };
}

var many = _many(false);

var many1 = _many(true);

//tokenPrim :: (c -> ParseState -> startIndex -> Result) -> (c -> Parser)
function tokenPrim(fn){
    return function(c){
        var pid = parser_id++;
        var combinator = function(scope, state, k){
            var startIndex = state.index;
            var result = state.getCached(pid);
            if(result !== undef)
                return k(result);
                
            result = fn(c, state, startIndex);
                        
            state.putCached(pid, startIndex, result);
            return k(result);
        };
        combinator.constructor = Parser;
        return combinator;
    };
}

//tokenPrimP1 :: (arg2 -> parser1Result -> ParseState -> startIndex -> newResult)
//              -> (Parser -> arg2 -> Parser)
function tokenPrimP1(fn){
    return function(p1, arg2){
        var pid = parser_id++;
        var combinator = function(scope, state, k){
            var startIndex = state.index;
            var result = state.getCached(pid);
            if(result !== undef)
                return k(result);
                
            return {func:p1, args:[scope, state, function(result){
                
                    result = fn(arg2, result, state, startIndex);
                    
                    state.putCached(pid, startIndex, result);
                    return k(result);
                }]};
            
        };
        combinator.constructor = Parser;
        return combinator;
    };
}


var try_ = tokenPrimP1(function(_, result, state, startIndex){
    result = extend({}, result);
    if(result.success)
        return result;
    state.scrollTo(startIndex);
    result.ast = undef;
    return result;
});


var skipMany = function(p){
    return tokenPrimP1(function(_, result, state, startIndex){
        result = extend({}, result);
        result.ast = undef;
        return result;
    })(many(p), null);
};

//string :: Char -> Parser
var char_ = tokenPrim(function(c, state, startIndex){
    if(state.length > 0 && state.at(0) == c){
        state.scroll(1);
        return make_result(c);
    }
    return _fail(c);
});


//string :: (Char -> Bool) -> Parser
var satisfy = tokenPrim(function(cond, state){
    var fstchar = state.at(0);
    if(state.length > 0 && cond(fstchar)){
        state.scroll(1);
        return make_result(fstchar);
    }
    return _fail(fstchar);
});



//string :: String -> Parser
var string = function(s){ //TODO
    return tokenPrimP1(function(_, result, state, startIndex){
        result = extend({}, result);
        result.ast = result.ast.join("");
        if(!result.success)
            result.expecting = {at:startIndex, expecting: s};
        else delete result.expecting;
        if(!result.ast.length) //TODO
            result.ast = undef;
        return result;
    })(tokens(map(char_, s)), null);
};


//tokenPrimP1 :: (a -> parser1Result -> ParseState -> startIndex -> newResult)
//              -> (Parser -> a -> Parser)
//label :: Parser -> String -> Parser
var label = tokenPrimP1(function(str, result, state, startIndex){
    if(!result.success){
        result = extend({}, result);
        result.expecting = {at: startIndex, expecting: str};
    }
    return result;  
});


//accepts a regexp or a string
//in case of a string it either matches the whole string or nothing

//match :: StringOrRegex -> Parser
var match = tokenPrim(function(sr, state){
        var result;
        if(typeof sr == "string"){
            if(state.substring(0, sr.length) == sr){
                state.scroll(sr.length);
                result = make_result(sr);
            }else
                result = _fail(sr);
                        
        }else if(sr.exec){
            var rx = new RegExp("^" + sr.source);
            var substr = state.substring(0);
            var match = rx.exec(substr);
            match = match && match[0];
            var length = match && match.length;
            var matched = substr.substr(0, length);
            if(length){
                state.scroll(length);
                result = make_result(matched);
            }else
                result = _fail(sr.source);
        }
        return result;
});



extend(operators, {
    "<-" : {
        func:   bind,
        fixity: infixr(-1) //this is a special operator, don't use negative fixity anywhere else!
        //,type:    [String, Parser, Parser]
    },
    ">>=": {
        func:   parserBind,
        fixity: infixl(1)
        //,type:    [Parser, Function, Parser]
    },
    "=<<": {
        func:   flip(parserBind),
        fixity: infixr(1)
        //,type:    [Parser, Parser, Parser]
    },
    ">>" : {
        func:   skip_fst,
        fixity: infixl(1)
        //,type:    [Parser, Parser, Parser]
    },
    "*>" : { //liftA2 (const id)
        func:   skip_fst,
        fixity: infixl(4)
        //,type:    [Parser, Parser, Parser]
    },
    "<*" : { //liftA2 const
        func:   skip_snd,
        fixity: infixl(4)
        //,type:    [Parser, Parser, Parser]
    },
    "<$>": {
        func:   fmap,
        fixity: infixl(4)
        //,type:    [Function, Parser, Parser]
    },
    "<*>": {
        func:   ap,
        fixity: infixl(4)
        //,type:    [Parser, Parser, Parser]
    },
    "<**>": { //liftA2 (flip ($))
        func:   curry(liftA2)(flip(call)),
        fixity: infixl(4)
        //,type:    [Parser, Parser, Parser]
    },
        //the (<$) combinator uses the value on the left 
        //if the parser on the right succeeds. x <$ p = pure x <* p
        //from Control.Applicative: (<$>) . const :: Functor f => a -> f b -> f a
    "<$" : {
        func:   function(val, parser){ return skip_snd(pure(value), parser) },
        fixity: infixl(4)
        //,type:    ["*", Parser, Parser]
    },
    "<|>": {
        func:   parserPlus,
        fixity: infixr(1)
        //,type:    [Parser, Parser, Parser]
    },
    "<?>": {
        func:   label,
        fixity: infix(0)
        //,type:    [Parser, String, Parser]
    }   
});



//a specialized version of `lazy`
function withScope(f){
    return function(scope, state, k){
        return f(scope)(scope, state, k);
    }
}

//#region comment
/*
usage: 

//assignExpr :: ExpressionParser st
//assignExpr = do
//  p <- getPosition
//  lhs <- parseTernaryExpr
//  let assign = do
//        op <- assignOp
//        lhs <- asLValue p lhs
//        rhs <- assignExpr
//        return (AssignExpr p op lhs rhs)
//  assign <|> (return lhs)

var assignExpr = do$
  ("p"   ,"<-", getPosition)
  ("lhs" ,"<-", parseTernaryExpr)
  (do$("op"  ,"<-", assignOp)
      ("lhs" ,"<-", withScope, function(scope){
        //bring p to the current scope, so that returnCall can be used:
        scope.p = scope.scope.p;
        return asLValue(scope.scope.p, scope.scope.lhs); //here asLValue retruns a parser
      })
      ("rhs" ,"<-", withScope, function(){ return assignExpr })
      (returnCall, Expression.AssignExpr, "p", "op", "lhs", "rhs")
  ,"<|>", ret, "lhs")

  withScope first is used to directly access variables from the enclosing scope,
  and second for just referencing the current parser recursively.

*/
//#endregion


instance(Monad, Parser, function(inst){return{
    ">>="   : parserBind,
    ">>"    : do2,
    do_     : do_,
    return_ : parserReturn,
    fail    : parserFail,
    run     : run
    /* //the default implementation used (which is slightly slower)
    ,do$  : function (){

        function rec(scope, state, k){ return p(scope, state, k) }

        var lines = [], p, resolved;

        lines.push(resolve(arguments, rec));

        function line(scope, state, k){
            if(resolved || (scope instanceof Scope))
                return (resolved ? p : line.resolve())(scope, state, k);
        
            lines.push(resolve(arguments, rec));
            return line;
        }

        line.resolve = function(){
            if(resolved)
                return p;
            p = do_.apply(null, lines);
            resolved = true;
            lines = null;
            return p;
        };

        line.CallStream = true;
        line.constructor = Parser;
        return line;
    }
    */
}})
var ParserMonad = getInstance(Monad, Parser);
var do$ = ParserMonad.do$;
//var do_ = ParserMonad.do_
var ex = exl(Parser);



//from Control.Monad
//
//-- | Evaluate each action in the sequence from left to right,
//-- and collect the results.
//sequence       :: Monad m => [m a] -> m [a] 
//{-# INLINE sequence #-}
//sequence ms = foldr k (return []) ms
//            where
//              k m m' = do { x <- m; xs <- m'; return (x:xs) }
function sequence(ms){
    //TODO!!!!
    var inst = getInstance(Monad, typeOf(ms[0]));
    var bindVar = NS.Text_Parsec.bind;
    var ret = NS.Text_Parsec.ret;
    var withBound = NS.Text_Parsec.withBound;

    function k(m1, m2){
        return inst.do_(
            bindVar("x", m1),
            bindVar("xs", m2),
            ret(withBound(cons, "x", "xs"))
        );
    }

    return foldr(k, inst.return_([]), ms);
}

namespace("Text_Parsec_Prim", {
    sequence        : sequence,

    run             : run,
    Parser          : Parser,
    ParseState      : ParseState,
    ps              : ps, 
    toParser        : toParser,
    unexpected      : unexpected,
    parsecMap       : parsecMap,
    fmap            : fmap,
    liftM           : liftM,
    liftM2          : liftM2,
    liftM3          : liftM3,
    liftA           : liftA,
    liftA2          : liftA2,
    liftA3          : liftA3,
    ap              : ap,
    parserBind      : parserBind,
    parserReturn    : parserReturn,
    return_         : return_,
    pure            : pure,
    parserFail      : parserFail,
    fail            : fail,
    parserZero      : parserZero,
    mzero           : mzero,
    empty           : empty,
    parserPlus      : parserPlus,
    parserPlusN     : parserPlusN,
    mplus           : mplus,
    do_             : do_,
    do$             : do$,
    do2             : do2,
    bind            : bind,
    ret             : ret,
    withBound       : withBound,
    returnCall      : returnCall,
    getPosition     : getPosition,
    setPosition     : setPosition,
    getParserState  : getParserState,
    setParserState  : setParserState,
    tokens          : tokens,
    many            : many,
    many1           : many1,
    string          : string,
    char_           : char_,
    satisfy         : satisfy,
    label           : label,
    try_            : try_,
    skipMany        : skipMany,
    match           : match,
    withScope       : withScope,
    ex              : ex
});
/// <reference path="Prim.js" />

// -------------------------------------------------
// Combinator
// -------------------------------------------------


//-- Commonly used generic combinators

//module Text.Parsec.Combinator
//    ( choice
//    , count
//    , between
//    , option, optionMaybe, optional
//    , skipMany1
//    , many1
//    , sepBy, sepBy1
//    , endBy, endBy1
//    , sepEndBy, sepEndBy1
//    , chainl, chainl1
//    , chainr, chainr1
//    , eof, notFollowedBy
//    -- tricky combinators
//    , manyTill, lookAhead, anyToken
//    ) where
//
//import Control.Monad
//import Text.Parsec.Prim
//


//-- | @choice ps@ tries to apply the parsers in the list @ps@ in order,
//-- until one of them succeeds. Returns the value of the succeeding
//-- parser.
//
//choice :: (Stream s m t) => [ParsecT s u m a] -> ParsecT s u m a
//choice ps           = foldr (<|>) mzero ps
//

function choice(ps){
    return foldr(parserPlus, mzero, ps);
}


//-- | @option x p@ tries to apply parser @p@. If @p@ fails without
//-- consuming input, it returns the value @x@, otherwise the value
//-- returned by @p@.
//--
//-- >  priority  = option 0 (do{ d <- digit
//-- >                          ; return (digitToInt d) 
//-- >                          })
//
//option :: (Stream s m t) => a -> ParsecT s u m a -> ParsecT s u m a
//option x p          = p <|> return x
//

function option(x, p){
    return parserPlus(p, return_(x));
}


//-- | @optionMaybe p@ tries to apply parser @p@.  If @p@ fails without
//-- consuming input, it return 'Nothing', otherwise it returns
//-- 'Just' the value returned by @p@.
//
//optionMaybe :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (Maybe a)
//optionMaybe p       = option Nothing (liftM Just p)
//

function optionMaybe(p){
    return option(Maybe.Nothing, liftM(Maybe.Just, p));
}


//-- | @optional p@ tries to apply parser @p@.  It will parse @p@ or nothing.
//-- It only fails if @p@ fails after consuming input. It discards the result
//-- of @p@.
//
//optional :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m ()
//optional p          = do{ p; return ()} <|> return ()
//

function optional(p){
    return parserPlus(do_(p, return_(null)), return_(null));
}


//-- | @between open close p@ parses @open@, followed by @p@ and @close@.
//-- Returns the value returned by @p@.
//--
//-- >  braces  = between (symbol "{") (symbol "}")
//
//between :: (Stream s m t) => ParsecT s u m open -> ParsecT s u m close
//            -> ParsecT s u m a -> ParsecT s u m a
//between open close p
//                    = do{ open; x <- p; close; return x }
//

function between(open, close, p){
    return do_(open, bind("x", p), close, ret("x"));
};


//-- | @skipMany1 p@ applies the parser @p@ /one/ or more times, skipping
//-- its result. 
//
//skipMany1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m ()
//skipMany1 p         = do{ p; skipMany p }
//{-
//skipMany p          = scan
//                    where
//                      scan  = do{ p; scan } <|> return ()
//-}
//

function skipMany1(p){
    return do_(p, skipMany(p));
}

//-- | @many p@ applies the parser @p@ /one/ or more times. Returns a
//-- list of the returned values of @p@.
//--
//-- >  word  = many1 letter
//
//many1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m [a]
//many1 p             = do{ x <- p; xs <- many p; return (x:xs) }
//{-
//many p              = scan id
//                    where
//                      scan f    = do{ x <- p
//                                    ; scan (\tail -> f (x:tail))
//                                    }
//                                <|> return (f [])
//-}
//
//

// -- defined in Prim


//-- | @sepBy p sep@ parses /zero/ or more occurrences of @p@, separated
//-- by @sep@. Returns a list of values returned by @p@.
//--
//-- >  commaSep p  = p `sepBy` (symbol ",")
//
//sepBy :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
//sepBy p sep         = sepBy1 p sep <|> return []
//

function sepBy(p, sep){
    return parserPlus(sepBy1(p, sep), return_([]));
}


//-- | @sepBy1 p sep@ parses /one/ or more occurrences of @p@, separated
//-- by @sep@. Returns a list of values returned by @p@. 
//
//sepBy1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
//sepBy1 p sep        = do{ x <- p
//                        ; xs <- many (sep >> p)
//                        ; return (x:xs)
//                        }
//
//

function sepBy1(p, sep){
    return do_(
        bind("x", p),
        bind("xs", many( do_(sep, p) ) ),
        returnCall(cons, "x", "xs")
    );
}


//-- | @sepEndBy1 p sep@ parses /one/ or more occurrences of @p@,
//-- separated and optionally ended by @sep@. Returns a list of values
//-- returned by @p@. 
//
//sepEndBy1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
//sepEndBy1 p sep     = do{ x <- p
//                        ; do{ sep
//                            ; xs <- sepEndBy p sep
//                            ; return (x:xs)
//                            }
//                          <|> return [x]
//                        }
//

function sepEndBy1(p, sep){
    return do_(
        bind("x", p),
        parserPlus(
            do_(
                sep,
                //bind("xs", sepEndBy(p, sep)),
                //thanks to eager evaluation this doesn't terminate without eta-expansion
                bind("xs", function(scope, state, k){ return sepEndBy(p, sep)(scope, state, k) }),
                ret(function(scope){ return cons(scope.scope.x, scope.xs) })
            ),
            ret(function(scope){ return [scope.x] })
        )
    );
}

//-- | @sepEndBy p sep@ parses /zero/ or more occurrences of @p@,
//-- separated and optionally ended by @sep@, ie. haskell style
//-- statements. Returns a list of values returned by @p@.
//--
//-- >  haskellStatements  = haskellStatement `sepEndBy` semi
//
//sepEndBy :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
//sepEndBy p sep      = sepEndBy1 p sep <|> return []
//
//

function sepEndBy(p, sep){
    return parserPlus(sepEndBy1(p, sep), return_([]));
}

//-- | @endBy1 p sep@ parses /one/ or more occurrences of @p@, seperated
//-- and ended by @sep@. Returns a list of values returned by @p@. 
//
//endBy1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
//endBy1 p sep        = many1 (do{ x <- p; sep; return x })
//

function endBy1(p, sep){
    return many1(do_( bind("x", p),  sep, ret("x") ));
}


//-- | @endBy p sep@ parses /zero/ or more occurrences of @p@, seperated
//-- and ended by @sep@. Returns a list of values returned by @p@.
//--
//-- >   cStatements  = cStatement `endBy` semi
//
//endBy :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
//endBy p sep         = many (do{ x <- p; sep; return x })
//

function endBy(p, sep){
    return many(do_( bind("x", p),  sep, ret("x") ));
}


//-- | @count n p@ parses @n@ occurrences of @p@. If @n@ is smaller or
//-- equal to zero, the parser equals to @return []@. Returns a list of
//-- @n@ values returned by @p@. 
//
//count :: (Stream s m t) => Int -> ParsecT s u m a -> ParsecT s u m [a]
//count n p           | n <= 0    = return []
//                    | otherwise = sequence (replicate n p)
//

function count(n, p){
    return (n <= 0) ? return_([]) : sequence(replicate(n, p));
}

//-- | @chainr p op x@ parser /zero/ or more occurrences of @p@,
//-- separated by @op@ Returns a value obtained by a /right/ associative
//-- application of all functions returned by @op@ to the values returned
//-- by @p@. If there are no occurrences of @p@, the value @x@ is
//-- returned.
//
//chainr :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> a -> ParsecT s u m a
//chainr p op x       = chainr1 p op <|> return x
//

function chainr(p, op, x){
    return parserPlus(chainr1(p, op), return_(x));
}


//-- | @chainl p op x@ parser /zero/ or more occurrences of @p@,
//-- separated by @op@. Returns a value obtained by a /left/ associative
//-- application of all functions returned by @op@ to the values returned
//-- by @p@. If there are zero occurrences of @p@, the value @x@ is
//-- returned.
//
//chainl :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> a -> ParsecT s u m a
//chainl p op x       = chainl1 p op <|> return x
//

function chainl(p, op, x){
    return parserPlus(chainl1(p, op), return_(x));
}


//-- | @chainl1 p op x@ parser /one/ or more occurrences of @p@,
//-- separated by @op@ Returns a value obtained by a /left/ associative
//-- application of all functions returned by @op@ to the values returned
//-- by @p@. . This parser can for example be used to eliminate left
//-- recursion which typically occurs in expression grammars.
//--
//-- >  expr    = term   `chainl1` addop
//-- >  term    = factor `chainl1` mulop
//-- >  factor  = parens expr <|> integer
//-- >
//-- >  mulop   =   do{ symbol "*"; return (*)   }
//-- >          <|> do{ symbol "/"; return (div) }
//-- >
//-- >  addop   =   do{ symbol "+"; return (+) }
//-- >          <|> do{ symbol "-"; return (-) }
//
//chainl1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a
//chainl1 p op        = do{ x <- p; rest x }
//                    where
//                      rest x    = do{ f <- op
//                                    ; y <- p
//                                    ; rest (f x y)
//                                    }
//                                <|> return x
//

function chainl1(p, op){
    
    function rest(x){ 
        var a = do_(
                    bind("f", op),
                    bind("y", p),
                    function(scope, state, k){
                        return rest(scope.f(x, scope.y))(scope, state, k);
                    }
                );
        return parserPlus(a, return_(x));
    }

    return parserBind(p, rest);
}


//-- | @chainr1 p op x@ parser /one/ or more occurrences of |p|,
//-- separated by @op@ Returns a value obtained by a /right/ associative
//-- application of all functions returned by @op@ to the values returned
//-- by @p@.
//
//chainr1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a
//chainr1 p op        = scan
//                    where
//                      scan      = do{ x <- p; rest x }
//
//                      rest x    = do{ f <- op
//                                    ; y <- scan
//                                    ; return (f x y)
//                                    }
//                                <|> return x
//

function chainr1(p, op){
    
    function rest(x){ 
        var a = do_(
                    bind("f", op),
                    bind("y", scan),
                    ret(function(scope){
                        return scope.f(x, scope.y);
                    })
                );
        return parserPlus(a, return_(x));
    }
    
    var scan = parserBind(p, rest);
    
    return scan;
}




//-----------------------------------------------------------
//-- Tricky combinators
//-----------------------------------------------------------


//-- | The parser @anyToken@ accepts any kind of token. It is for example
//-- used to implement 'eof'. Returns the accepted token. 
//
//anyToken :: (Stream s m t, Show t) => ParsecT s u m t
//anyToken            = tokenPrim show (\pos _tok _toks -> pos) Just
//

function anyToken(scope, state, k){
    var at = state.at(0);
    if(at.length){
        state.scroll(1);
        return k(make_result(at));
    }
    
    return k(_fail("anyToken"));
}


//-- | This parser only succeeds at the end of the input. This is not a
//-- primitive parser but it is defined using 'notFollowedBy'.
//--
//-- >  eof  = notFollowedBy anyToken <?> "end of input"
//
//eof :: (Stream s m t, Show t) => ParsecT s u m ()
//eof                 = notFollowedBy anyToken <?> "end of input"
//

// this works too:
// var eof = [notFollowedBy, anyToken ,"<?>", "end of input"].resolve();
function eof(scope, state, k){
    return k(make_result(undef, !state.length, state.length ? "end of input" : undef));
}

//-- | @notFollowedBy p@ only succeeds when parser @p@ fails. This parser
//-- does not consume any input. This parser can be used to implement the
//-- \'longest match\' rule. For example, when recognizing keywords (for
//-- example @let@), we want to make sure that a keyword is not followed
//-- by a legal identifier character, in which case the keyword is
//-- actually an identifier (for example @lets@). We can program this
//-- behaviour as follows:
//--
//-- >  keywordLet  = try (do{ string "let"
//-- >                       ; notFollowedBy alphaNum
//-- >                       })
//
//notFollowedBy :: (Stream s m t, Show a) => ParsecT s u m a -> ParsecT s u m ()
//notFollowedBy p     = try (do{ c <- try p; unexpected (show c) }
//                           <|> return ()
//                          )
//

/*
function notFollowedBy(p){
    return try_(
        parserPlus(
            do_(
                bind("c", try_(p)),
                unexpectedIdent("c")
            ),
            return_(null)
        )
    );
}
*/

//since `show c` is not necessary, so we can simplify it:
function notFollowedBy(p){
    return try_(parserPlus(
            parserBind(try_(p), unexpected),
            return_(null)
    ));
}


//-- | @manyTill p end@ applies parser @p@ /zero/ or more times until
//-- parser @end@ succeeds. Returns the list of values returned by @p@.
//-- This parser can be used to scan comments:
//--
//-- >  simpleComment   = do{ string "<!--"
//-- >                      ; manyTill anyChar (try (string "-->"))
//-- >                      }
//--
//--    Note the overlapping parsers @anyChar@ and @string \"<!--\"@, and
//--    therefore the use of the 'try' combinator.
//
//manyTill :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
//manyTill p end      = scan
//                    where
//                      scan  = do{ end; return [] }
//                            <|>
//                              do{ x <- p; xs <- scan; return (x:xs) }
//

function manyTill(p, end){

    function _scan(scope, state, k){ return scan(scope, state, k) }

    var scan = parserPlus(
        do_( end, return_([]) ),
        do_( bind("x", p), bind("xs", _scan), returnCall(cons, "x", "xs") )
    );

    return scan;
}


//-- | @lookAhead p@ parses @p@ without consuming any input.
//
//lookAhead :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m a
//lookAhead p         = do{ state <- getParserState
//                        ; x <- p
//                        ; setParserState state
//                        ; return x
//                        }

function lookAhead(p){
    return do_(
        bind("state", getParserState),
        bind("x", p),
        setParserState("state"),
        ret("x")
    );
}


namespace("Text_Parsec_Combinator", {
    choice        : choice
  , count         : count
  , between       : between
  , option        : option
  , optionMaybe   : optionMaybe
  , optional      : optional
  , skipMany1     : skipMany1
//, many1         : many1
  , sepBy         : sepBy
  , sepBy1        : sepBy1
  , endBy         : endBy
  , endBy1        : endBy1
  , sepEndBy      : sepEndBy
  , sepEndBy1     : sepEndBy1
  , chainl        : chainl
  , chainl1       : chainl1
  , chainr        : chainr
  , chainr1       : chainr1
  , eof           : eof
  , notFollowedBy : notFollowedBy
  , manyTill      : manyTill
  , lookAhead     : lookAhead
  , anyToken      : anyToken
});/// <reference path="Prim.js" />
/// <reference path="Combinator.js" />

// -------------------------------------------------
// Expr
// -------------------------------------------------

//-----------------------------------------------------------------------------
//-- |
//-- Module      :  Text.Parsec.Expr
//-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//-- License     :  BSD-style (see the LICENSE file)
//-- 
//-- Maintainer  :  derek.a.elkins@gmail.com
//-- Stability   :  provisional
//-- Portability :  non-portable
//-- 
//-- A helper module to parse \"expressions\".
//-- Builds a parser given a table of operators and associativities.
//-- 
//-----------------------------------------------------------------------------
//
//module Text.Parsec.Expr
//    ( Assoc(..), Operator(..), OperatorTable
//    , buildExpressionParser
//    ) where
//
//import Text.Parsec.Prim
//import Text.Parsec.Combinator
//
//-----------------------------------------------------------
//-- Assoc and OperatorTable
//-----------------------------------------------------------
//
//-- |  This data type specifies the associativity of operators: left, right
//-- or none.
//
//data Assoc                = AssocNone
//                          | AssocLeft
//                          | AssocRight

function Assoc(){}
data(Assoc, ["AssocNone", "AssocLeft", "AssocRight"]);

//-- | This data type specifies operators that work on values of type @a@.
//-- An operator is either binary infix or unary prefix or postfix. A
//-- binary operator has also an associated associativity.
//
//data Operator s u m a   = Infix (ParsecT s u m (a -> a -> a)) Assoc
//                        | Prefix (ParsecT s u m (a -> a))
//                        | Postfix (ParsecT s u m (a -> a))
function Operator(){}
data(Operator, [
    ["Infix", Parser, Assoc],
    ["Prefix", Parser],
    ["Postfix", Parser]
]);


//-- | An @OperatorTable s u m a@ is a list of @Operator s u m a@
//-- lists. The list is ordered in descending
//-- precedence. All operators in one list have the same precedence (but
//-- may have a different associativity).
//
//type OperatorTable s u m a = [[Operator s u m a]]


//-----------------------------------------------------------
//-- Convert an OperatorTable and basic term parser into
//-- a full fledged expression parser
//-----------------------------------------------------------
//
//-- | @buildExpressionParser table term@ builds an expression parser for
//-- terms @term@ with operators from @table@, taking the associativity
//-- and precedence specified in @table@ into account. Prefix and postfix
//-- operators of the same precedence can only occur once (i.e. @--2@ is
//-- not allowed if @-@ is prefix negate). Prefix and postfix operators
//-- of the same precedence associate to the left (i.e. if @++@ is
//-- postfix increment, than @-2++@ equals @-1@, not @-3@).
//--
//-- The @buildExpressionParser@ takes care of all the complexity
//-- involved in building expression parser. Here is an example of an
//-- expression parser that handles prefix signs, postfix increment and
//-- basic arithmetic.
//--
//-- >  expr    = buildExpressionParser table term
//-- >          <?> "expression"
//-- >
//-- >  term    =  parens expr 
//-- >          <|> natural
//-- >          <?> "simple expression"
//-- >
//-- >  table   = [ [prefix "-" negate, prefix "+" id ]
//-- >            , [postfix "++" (+1)]
//-- >            , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
//-- >            , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
//-- >            ]
//-- >          
//-- >  binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
//-- >  prefix  name fun       = Prefix (do{ reservedOp name; return fun })
//-- >  postfix name fun       = Postfix (do{ reservedOp name; return fun })
//
//buildExpressionParser :: (Stream s m t)
//                      => OperatorTable s u m a
//                      -> ParsecT s u m a
//                      -> ParsecT s u m a
//buildExpressionParser operators simpleExpr = ...
function buildExpressionParser(operators, simpleExpr){
    
    function hook(fn, ident){
        return function(scope, state, k){
            return fn(scope[ident])(scope, state, k);
        };
    }
    
    function splitOp(oper, tuple){
        
        var op = oper[0];
        var rassoc  = tuple[0],
            lassoc  = tuple[1],
            nassoc  = tuple[2],
            prefix  = tuple[3],
            postfix = tuple[4];
        
//      splitOp (Infix op assoc) (rassoc,lassoc,nassoc,prefix,postfix)
//        = case assoc of
//            AssocNone  -> (rassoc,lassoc,op:nassoc,prefix,postfix)
//            AssocLeft  -> (rassoc,op:lassoc,nassoc,prefix,postfix)
//            AssocRight -> (op:rassoc,lassoc,nassoc,prefix,postfix)
        if(oper.Infix){
            var assoc = oper[1];
            if(assoc.AssocNone)
                return [rassoc, lassoc, cons(op, nassoc), prefix, postfix];
            if(assoc.AssocLeft)
                return [rassoc, cons(op, lassoc), nassoc, prefix, postfix];
            if(assoc.AssocRight)
                return [cons(op, rassoc), lassoc, nassoc, prefix, postfix];
        }

//      splitOp (Prefix op) (rassoc,lassoc,nassoc,prefix,postfix)
//        = (rassoc,lassoc,nassoc,op:prefix,postfix)
        if(oper.Prefix)
            return [rassoc, lassoc, nassoc, cons(op, prefix), postfix];
        
//      splitOp (Postfix op) (rassoc,lassoc,nassoc,prefix,postfix)
//        = (rassoc,lassoc,nassoc,prefix,op:postfix)
        if(oper.Postfix)
            return [rassoc, lassoc, nassoc, prefix, cons(op, postfix)];
    }
    
    
//              ambigious assoc op = try $
//                                  do{ op; fail ("ambiguous use of a " ++ assoc
//                                                 ++ " associative operator")
//                                    }
    function ambigious(assoc, op){
        return try_(do_(op, fail("ambiguous use of a " + assoc + " associative operator")));
    }


    function makeParser(term, ops){
        
        var tuple = foldr(splitOp, [[],[],[],[],[]], ops);
        
        var rassoc  = tuple[0],
            lassoc  = tuple[1],
            nassoc  = tuple[2],
            prefix  = tuple[3],
            postfix = tuple[4];
            
        var rassocOp   = choice(rassoc),
            lassocOp   = choice(lassoc),
            nassocOp   = choice(nassoc),
            prefixOp   = label(choice(prefix) , ""),
            postfixOp  = label(choice(postfix), "");
            
        var ambigiousRight = ambigious("right", rassocOp),
            ambigiousLeft  = ambigious("left" , lassocOp),
            ambigiousNon   = ambigious("non"  , nassocOp);
            
        var postfixP  = parserPlus(postfixOp, return_(id)),
            prefixP   = parserPlus(prefixOp , return_(id));
            
//              termP      = do{ pre  <- prefixP
//                             ; x    <- term
//                             ; post <- postfixP
//                             ; return (post (pre x))
//                             }
        var termP = do$
            ("pre"  ,"<-", prefixP)
            ("x"    ,"<-", term)
            ("post" ,"<-", postfixP)
            (ret, function(scope){ return scope.post(scope.pre(scope.x)) })
        
        
//              rassocP x  = do{ f <- rassocOp
//                             ; y  <- do{ z <- termP; rassocP1 z }
//                             ; return (f x y)
//                             }
//                           <|> ambigiousLeft
//                           <|> ambigiousNon
//                           -- <|> return x
        function rassocP(x){
            return ex(do$("f"  ,"<-", rassocOp)
                         ("y"  ,"<-", do$("z"  ,"<-", termP)
                                         (hook, rassocP1, "z")
                         )
                         (ret, function(scope){ return scope.f(x, scope.y) })
                    ,"<|>", ambigiousLeft
                    ,"<|>", ambigiousNon
                    //,"<|>", return_, x
                    );
        }
        
//              rassocP1 x = rassocP x  <|> return x
        function rassocP1(x){
            return parserPlus(rassocP(x), return_(x));
        }
        
//              lassocP x  = do{ f <- lassocOp
//                             ; y <- termP
//                             ; lassocP1 (f x y)
//                             }
//                           <|> ambigiousRight
//                           <|> ambigiousNon
//                           -- <|> return x
        function lassocP(x){
            return ex(do$("f"  ,"<-", lassocOp)
                         ("y"  ,"<-", termP)
                         (function(scope, state, k){
                             return lassocP1(scope.f(x, scope.y))(scope, state, k);
                         })
                         ,"<|>", ambigiousRight
                         ,"<|>", ambigiousNon
                         //,"<|>", return_, x
                    );
        }
        
//              lassocP1 x = lassocP x <|> return x
        function lassocP1(x){
            return parserPlus(lassocP(x), return_(x));
        }

//              nassocP x  = do{ f <- nassocOp
//                             ; y <- termP
//                             ;    ambigiousRight
//                              <|> ambigiousLeft
//                              <|> ambigiousNon
//                              <|> return (f x y)
//                             }
//                           -- <|> return x
        function nassocP(x){
            return do$
                ("f"  ,"<-", nassocOp)
                ("y"  ,"<-", termP)
                (       ambigiousRight
                ,"<|>", ambigiousLeft
                ,"<|>", ambigiousNon
                ,"<|>", ret, function(scope){ return scope.f(x, scope.y) }
                )
        }
        
//           in  do{ x <- termP
//                 ; rassocP x <|> lassocP  x <|> nassocP x <|> return x
//                   <?> "operator"
//                 }
        return do$("x" ,"<-", termP)
                  (hook, rassocP, "x"
                  ,"<|>", hook, lassocP, "x"
                  ,"<|>", hook, nassocP, "x"
                  ,"<|>", ret, "x"
                  ,"<?>", "operator").resolve();
    }
        
//  buildExpressionParser operators simpleExpr
//       = foldl (makeParser) simpleExpr operators
    return foldl(makeParser, simpleExpr, operators);
}


namespace("Text_Parsec_Expr", {
    Assoc: Assoc,
    Operator: Operator,
    buildExpressionParser: buildExpressionParser
});/// <reference path="Prim.js" />

// -------------------------------------------------
// Char
// -------------------------------------------------


//-- Commonly used character parsers.

//module Text.Parsec.Char where
//
//import Data.Char
//import Text.Parsec.Pos
//import Text.Parsec.Prim

// | @oneOf cs@ succeeds if the current character is in the supplied
// list of characters @cs@. Returns the parsed character. See also
// 'satisfy'.
// 
// >   vowel  = oneOf "aeiou"

//oneOf :: (Stream s m Char) => [Char] -> ParsecT s u m Char
//oneOf cs            = satisfy (\c -> elem c cs)

var oneOf = function(cs){
	return label(satisfy(function(c){ return elem(c, cs) }), "oneOf(" + cs + ")");
};

// | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
// character /not/ in the supplied list of characters @cs@. Returns the
// parsed character.
//
// >  consonant = noneOf "aeiou"

//noneOf :: (Stream s m Char) => [Char] -> ParsecT s u m Char
//noneOf cs           = satisfy (\c -> not (elem c cs))

var noneOf = function(cs){
	return label(satisfy(function(c){ return !elem(c, cs) }), "noneOf(" + cs + ")");
};


// | Parses a white space character (any character which satisfies 'isSpace')
// Returns the parsed character. 

//space :: (Stream s m Char) => ParsecT s u m Char
//space               = satisfy isSpace       <?> "space"

var space = ex(satisfy, isSpace ,"<?>", "space").resolve();


// | Skips /zero/ or more white space characters. See also 'skipMany'.

//spaces :: (Stream s m Char) => ParsecT s u m ()
//spaces              = skipMany space        <?> "white space"

var spaces = ex(skipMany, space ,"<?>", "white space").resolve();


// | Parses a newline character (\'\\n\'). Returns a newline character. 

//newline :: (Stream s m Char) => ParsecT s u m Char
//newline             = char '\n'             <?> "new-line"

var newline = ex(char_, '\n' ,"<?>", "new-line").resolve();

// | Parses a tab character (\'\\t\'). Returns a tab character. 

//tab :: (Stream s m Char) => ParsecT s u m Char
//tab                 = char '\t'             <?> "tab"

var tab = ex(char_, '\t' ,"<?>", "tab").resolve();

// | Parses an upper case letter (a character between \'A\' and \'Z\').
// Returns the parsed character. 

//upper :: (Stream s m Char) => ParsecT s u m Char
//upper               = satisfy isUpper       <?> "uppercase letter"

var upper = ex(satisfy, isUpper ,"<?>", "uppercase letter").resolve();


// | Parses a lower case character (a character between \'a\' and \'z\').
// Returns the parsed character. 

//lower :: (Stream s m Char) => ParsecT s u m Char
//lower               = satisfy isLower       <?> "lowercase letter"

var lower = ex(satisfy, isLower ,"<?>", "lowercase letter").resolve();


// | Parses a letter or digit (a character between \'0\' and \'9\').
// Returns the parsed character. 

//alphaNum :: (Stream s m Char => ParsecT s u m Char)
//alphaNum            = satisfy isAlphaNum    <?> "letter or digit"

var alphaNum = ex(satisfy, isAlphaNum ,"<?>", "letter or digit").resolve();


// | Parses a letter (an upper case or lower case character). Returns the
// parsed character. 

//letter :: (Stream s m Char) => ParsecT s u m Char
//letter              = satisfy isAlpha       <?> "letter"

var letter = ex(satisfy, isAlpha ,"<?>", "letter").resolve();

// | Parses a digit. Returns the parsed character. 

//digit :: (Stream s m Char) => ParsecT s u m Char
//digit               = satisfy isDigit       <?> "digit"

var digit = ex(satisfy, isDigit ,"<?>", "digit").resolve();


// | Parses a hexadecimal digit (a digit or a letter between \'a\' and
// \'f\' or \'A\' and \'F\'). Returns the parsed character. 

//hexDigit :: (Stream s m Char) => ParsecT s u m Char
//hexDigit            = satisfy isHexDigit    <?> "hexadecimal digit"

var hexDigit = ex(satisfy, isHexDigit ,"<?>", "hexadecimal digit").resolve();


// | Parses an octal digit (a character between \'0\' and \'7\'). Returns
// the parsed character. 

//octDigit :: (Stream s m Char) => ParsecT s u m Char
//octDigit            = satisfy isOctDigit    <?> "octal digit"

var octDigit = ex(satisfy, isOctDigit ,"<?>", "octal digit").resolve();


// | This parser succeeds for any character. Returns the parsed character. 

//anyChar :: (Stream s m Char) => ParsecT s u m Char
//anyChar             = satisfy (const True)

var anyChar = ex(satisfy, const_(true)).resolve();


// | @char c@ parses a single character @c@. Returns the parsed
// character (i.e. @c@).
//
// >  semiColon  = char ';'

//char c              = satisfy (==c)  <?> show [c]

//var char_ = function(c){
//	return ex(satisfy, function(ch){ return ch == c } ,"<?>", c).resolve();
//}

// -- a specialized version is defined in Prim



// | @string s@ parses a sequence of characters given by @s@. Returns
// the parsed string (i.e. @s@).
//
// >  divOrMod    =   string "div" 
// >              <|> string "mod"

//string :: (Stream s m Char) => String -> ParsecT s u m String
//string s            = tokens show updatePosString s

// -- defined in Prim

namespace("Text_Parsec_Char", {
    oneOf    : oneOf,
    noneOf   : noneOf,
    space    : space,
    spaces   : spaces,
    newline  : newline,
    tab      : tab,
    upper    : upper,
    lower    : lower,
    alphaNum : alphaNum,
    letter   : letter,
    digit    : digit,
    hexDigit : hexDigit,
    octDigit : octDigit,
    anyChar  : anyChar
});/// <reference path="Prim.js" />
/// <reference path="Char.js" />
/// <reference path="Combinator.js" />

// -------------------------------------------------
// Token
// -------------------------------------------------

 
//-- A helper module to parse lexical elements (tokens). See 'makeTokenParser'
//-- for a description of how to use it.

//module Text.Parsec.Token
//  ( LanguageDef
//  , GenLanguageDef (..)
//  , TokenParser
//  , GenTokenParser (..)
//  , makeTokenParser
//  ) where
//
//import Data.Char ( isAlpha, toLower, toUpper, isSpace, digitToInt )
//import Data.List ( nub, sort )
//import Control.Monad.Identity
//import Text.Parsec.Prim
//import Text.Parsec.Char
//import Text.Parsec.Combinator
//
//-----------------------------------------------------------
//-- Language Definition
//-----------------------------------------------------------
//
//type LanguageDef st = GenLanguageDef String st Identity
//
//-- | The @GenLanguageDef@ type is a record that contains all parameterizable
//-- features of the 'Text.Parsec.Token' module. The module 'Text.Parsec.Language'
//-- contains some default definitions.
//


function GenLanguageDef(){}

data(GenLanguageDef, [["LanguageDef", {

//data GenLanguageDef s u m
//  = LanguageDef { 
//  
//  -- | Describes the start of a block comment. Use the empty string if the
//  -- language doesn't support block comments. For example \"\/*\". 
//
//  commentStart   :: String,
commentStart: String,
//
//  -- | Describes the end of a block comment. Use the empty string if the
//  -- language doesn't support block comments. For example \"*\/\". 
//
//  commentEnd     :: String,
commentEnd: String,
//
//  -- | Describes the start of a line comment. Use the empty string if the
//  -- language doesn't support line comments. For example \"\/\/\". 
//
//  commentLine    :: String,
commentLine: String,
//
//  -- | Set to 'True' if the language supports nested block comments. 
//
//  nestedComments :: Bool,
nestedComments: Boolean,
//
//  -- | This parser should accept any start characters of identifiers. For
//  -- example @letter \<|> char \"_\"@. 
//
//  identStart     :: ParsecT s u m Char,
identStart: Parser,
//
//  -- | This parser should accept any legal tail characters of identifiers.
//  -- For example @alphaNum \<|> char \"_\"@. 
//
//  identLetter    :: ParsecT s u m Char,
identLetter: Parser,
//
//  -- | This parser should accept any start characters of operators. For
//  -- example @oneOf \":!#$%&*+.\/\<=>?\@\\\\^|-~\"@ 
//
//  opStart        :: ParsecT s u m Char,
opStart: Parser,
//
//  -- | This parser should accept any legal tail characters of operators.
//  -- Note that this parser should even be defined if the language doesn't
//  -- support user-defined operators, or otherwise the 'reservedOp'
//  -- parser won't work correctly. 
//
//  opLetter       :: ParsecT s u m Char,
opLetter: Parser,
//
//  -- | The list of reserved identifiers. 
//
//  reservedNames  :: [String],
reservedNames: Array,
//
//  -- | The list of reserved operators. 
//
//  reservedOpNames:: [String],
reservedOpNames: Array,
//
//  -- | Set to 'True' if the language is case sensitive. 
//
//  caseSensitive  :: Bool
caseSensitive: Boolean
//
//  }

}]]);



//-----------------------------------------------------------
//-- A first class module: TokenParser
//-----------------------------------------------------------
//
//type TokenParser st = GenTokenParser String st Identity
//
//-- | The type of the record that holds lexical parsers that work on
//-- @s@ streams with state @u@ over a monad @m@.
//

function GenTokenParser(){}

data(GenTokenParser, [["TokenParser", {

//data GenTokenParser s u m
//  = TokenParser {
//
//      -- | This lexeme parser parses a legal identifier. Returns the identifier
//      -- string. This parser will fail on identifiers that are reserved
//      -- words. Legal identifier (start) characters and reserved words are
//      -- defined in the 'LanguageDef' that is passed to
//      -- 'makeTokenParser'. An @identifier@ is treated as
//      -- a single token using 'try'.
//
//      identifier       :: ParsecT s u m String,
identifier: Parser,
//      
//      -- | The lexeme parser @reserved name@ parses @symbol 
//      -- name@, but it also checks that the @name@ is not a prefix of a
//      -- valid identifier. A @reserved@ word is treated as a single token
//      -- using 'try'. 
//
//      reserved         :: String -> ParsecT s u m (),
reserved: Function,
//
//      -- | This lexeme parser parses a legal operator. Returns the name of the
//      -- operator. This parser will fail on any operators that are reserved
//      -- operators. Legal operator (start) characters and reserved operators
//      -- are defined in the 'LanguageDef' that is passed to
//      -- 'makeTokenParser'. An @operator@ is treated as a
//      -- single token using 'try'. 
//
//      operator         :: ParsecT s u m String,
operator: Parser,
//
//      -- |The lexeme parser @reservedOp name@ parses @symbol
//      -- name@, but it also checks that the @name@ is not a prefix of a
//      -- valid operator. A @reservedOp@ is treated as a single token using
//      -- 'try'. 
//
//      reservedOp       :: String -> ParsecT s u m (),
reservedOp: Function,
//
//
//      -- | This lexeme parser parses a single literal character. Returns the
//      -- literal character value. This parsers deals correctly with escape
//      -- sequences. The literal character is parsed according to the grammar
//      -- rules defined in the Haskell report (which matches most programming
//      -- languages quite closely). 
//
//      charLiteral      :: ParsecT s u m Char,
charLiteral: Parser,
//
//      -- | This lexeme parser parses a literal string. Returns the literal
//      -- string value. This parsers deals correctly with escape sequences and
//      -- gaps. The literal string is parsed according to the grammar rules
//      -- defined in the Haskell report (which matches most programming
//      -- languages quite closely). 
//
//      stringLiteral    :: ParsecT s u m String,
stringLiteral: Parser,
//
//      -- | This lexeme parser parses a natural number (a positive whole
//      -- number). Returns the value of the number. The number can be
//      -- specified in 'decimal', 'hexadecimal' or
//      -- 'octal'. The number is parsed according to the grammar
//      -- rules in the Haskell report. 
//
//      natural          :: ParsecT s u m Integer,
natural: Parser,
//
//      -- | This lexeme parser parses an integer (a whole number). This parser
//      -- is like 'natural' except that it can be prefixed with
//      -- sign (i.e. \'-\' or \'+\'). Returns the value of the number. The
//      -- number can be specified in 'decimal', 'hexadecimal'
//      -- or 'octal'. The number is parsed according
//      -- to the grammar rules in the Haskell report. 
//      
//      integer          :: ParsecT s u m Integer,
integer: Parser,
//
//      -- | This lexeme parser parses a floating point value. Returns the value
//      -- of the number. The number is parsed according to the grammar rules
//      -- defined in the Haskell report. 
//
//      float            :: ParsecT s u m Double,
float_: Parser,
//
//      -- | This lexeme parser parses either 'natural' or a 'float'.
//      -- Returns the value of the number. This parsers deals with
//      -- any overlap in the grammar rules for naturals and floats. The number
//      -- is parsed according to the grammar rules defined in the Haskell report. 
//
//      naturalOrFloat   :: ParsecT s u m (Either Integer Double),
naturalOrFloat: Parser,
//
//      -- | Parses a positive whole number in the decimal system. Returns the
//      -- value of the number. 
//
//      decimal          :: ParsecT s u m Integer,
decimal: Parser,
//
//      -- | Parses a positive whole number in the hexadecimal system. The number
//      -- should be prefixed with \"0x\" or \"0X\". Returns the value of the
//      -- number. 
//
//      hexadecimal      :: ParsecT s u m Integer,
hexadecimal: Parser,
//
//      -- | Parses a positive whole number in the octal system. The number
//      -- should be prefixed with \"0o\" or \"0O\". Returns the value of the
//      -- number. 
//
//      octal            :: ParsecT s u m Integer,
octal: Parser,
//
//      -- | Lexeme parser @symbol s@ parses 'string' @s@ and skips
//      -- trailing white space. 
//
//      symbol           :: String -> ParsecT s u m String,
symbol: Function,
//
//      -- | @lexeme p@ first applies parser @p@ and than the 'whiteSpace'
//      -- parser, returning the value of @p@. Every lexical
//      -- token (lexeme) is defined using @lexeme@, this way every parse
//      -- starts at a point without white space. Parsers that use @lexeme@ are
//      -- called /lexeme/ parsers in this document.
//      -- 
//      -- The only point where the 'whiteSpace' parser should be
//      -- called explicitly is the start of the main parser in order to skip
//      -- any leading white space.
//      --
//      -- >    mainParser  = do{ whiteSpace
//      -- >                     ; ds <- many (lexeme digit)
//      -- >                     ; eof
//      -- >                     ; return (sum ds)
//      -- >                     }
//
//      lexeme           :: forall a. ParsecT s u m a -> ParsecT s u m a,
lexeme: Function,
//
//      -- | Parses any white space. White space consists of /zero/ or more
//      -- occurrences of a 'space', a line comment or a block (multi
//      -- line) comment. Block comments may be nested. How comments are
//      -- started and ended is defined in the 'LanguageDef'
//      -- that is passed to 'makeTokenParser'. 
//
//      whiteSpace       :: ParsecT s u m (),
whiteSpace: Parser,
//
//      -- | Lexeme parser @parens p@ parses @p@ enclosed in parenthesis,
//      -- returning the value of @p@.
//
//      parens           :: forall a. ParsecT s u m a -> ParsecT s u m a,
parens: Function,
//
//      -- | Lexeme parser @braces p@ parses @p@ enclosed in braces (\'{\' and
//      -- \'}\'), returning the value of @p@. 
//
//      braces           :: forall a. ParsecT s u m a -> ParsecT s u m a,
braces: Function,
//
//      -- | Lexeme parser @angles p@ parses @p@ enclosed in angle brackets (\'\<\'
//      -- and \'>\'), returning the value of @p@. 
//
//      angles           :: forall a. ParsecT s u m a -> ParsecT s u m a,
angles: Function,
//
//      -- | Lexeme parser @brackets p@ parses @p@ enclosed in brackets (\'[\'
//      -- and \']\'), returning the value of @p@. 
//
//      brackets         :: forall a. ParsecT s u m a -> ParsecT s u m a,
brackets: Function,
//
//      -- | DEPRECATED: Use 'brackets'.
//
//      squares          :: forall a. ParsecT s u m a -> ParsecT s u m a,
squares: Function,
//
//      -- | Lexeme parser |semi| parses the character \';\' and skips any
//      -- trailing white space. Returns the string \";\". 
//
//      semi             :: ParsecT s u m String,
semi: Parser,
//
//      -- | Lexeme parser @comma@ parses the character \',\' and skips any
//      -- trailing white space. Returns the string \",\". 
//
//      comma            :: ParsecT s u m String,
comma: Parser,
//
//      -- | Lexeme parser @colon@ parses the character \':\' and skips any
//      -- trailing white space. Returns the string \":\". 
//
//      colon            :: ParsecT s u m String,
colon: Parser,
//
//      -- | Lexeme parser @dot@ parses the character \'.\' and skips any
//      -- trailing white space. Returns the string \".\". 
//
//      dot              :: ParsecT s u m String,
dot: Parser,
//
//      -- | Lexeme parser @semiSep p@ parses /zero/ or more occurrences of @p@
//      -- separated by 'semi'. Returns a list of values returned by
//      -- @p@.
//
//      semiSep          :: forall a . ParsecT s u m a -> ParsecT s u m [a],
semiSep: Function,
//
//      -- | Lexeme parser @semiSep1 p@ parses /one/ or more occurrences of @p@
//      -- separated by 'semi'. Returns a list of values returned by @p@. 
//
//      semiSep1         :: forall a . ParsecT s u m a -> ParsecT s u m [a],
semiSep1: Function,
//
//      -- | Lexeme parser @commaSep p@ parses /zero/ or more occurrences of
//      -- @p@ separated by 'comma'. Returns a list of values returned
//      -- by @p@. 
//
//      commaSep        :: forall a . ParsecT s u m a -> ParsecT s u m [a]
commaSep: Function,
//
//      -- | Lexeme parser @commaSep1 p@ parses /one/ or more occurrences of
//      -- @p@ separated by 'comma'. Returns a list of values returned
//      -- by @p@. 
//
//      commaSep1        :: forall a . ParsecT s u m a -> ParsecT s u m [a]
commaSep1: Function
//  }
}]]);

//
//-----------------------------------------------------------
//-- Given a LanguageDef, create a token parser.
//-----------------------------------------------------------
//
//-- | The expression @makeTokenParser language@ creates a 'GenTokenParser'
//-- record that contains lexical parsers that are
//-- defined using the definitions in the @language@ record.
//--
//-- The use of this function is quite stylized - one imports the
//-- appropiate language definition and selects the lexical parsers that
//-- are needed from the resulting 'GenTokenParser'.
//--
//-- >  module Main where
//-- >
//-- >  import Text.Parsec
//-- >  import qualified Text.Parsec.Token as P
//-- >  import Text.Parsec.Language (haskellDef)
//-- >
//-- >  -- The parser
//-- >  ...
//-- >
//-- >  expr  =   parens expr
//-- >        <|> identifier
//-- >        <|> ...
//-- >       
//-- >
//-- >  -- The lexer
//-- >  lexer       = P.makeTokenParser haskellDef    
//-- >      
//-- >  parens      = P.parens lexer
//-- >  braces      = P.braces lexer
//-- >  identifier  = P.identifier lexer
//-- >  reserved    = P.reserved lexer
//-- >  ...
//
//makeTokenParser :: (Stream s m Char)
//              => GenLanguageDef s u m -> GenTokenParser s u m
//makeTokenParser languageDef
//  = TokenParser{ identifier = identifier
//               , reserved = reserved
//               , operator = operator
//               , reservedOp = reservedOp
//
//               , charLiteral = charLiteral
//               , stringLiteral = stringLiteral
//               , natural = natural
//               , integer = integer
//               , float = float
//               , naturalOrFloat = naturalOrFloat
//               , decimal = decimal
//               , hexadecimal = hexadecimal
//               , octal = octal
//
//               , symbol = symbol
//               , lexeme = lexeme
//               , whiteSpace = whiteSpace
//
//               , parens = parens
//               , braces = braces
//               , angles = angles
//               , brackets = brackets
//               , squares = brackets
//               , semi = semi
//               , comma = comma
//               , colon = colon
//               , dot = dot
//               , semiSep = semiSep
//               , semiSep1 = semiSep1
//               , commaSep = commaSep
//               , commaSep1 = commaSep1
//               }


function makeTokenParser(languageDef){
    if(!languageDef.LanguageDef)
        throw "Type error: unexpected '" + languageDef.constructor.name + "', expecting 'GenLanguageDef.LanguageDef'";


//  -----------------------------------------------------------
//  -- White space & symbols
//  -----------------------------------------------------------

//  symbol name
//      = lexeme (string name)

function symbol(name){
    return lexeme(string(name));
}


//
//  lexeme p
//      = do{ x <- p; whiteSpace; return x  }

function lexeme(p){
    return do_(bind("x", p), whiteSpace, ret("x") );
}


//
//
//  simpleSpace =
//      skipMany1 (satisfy isSpace)

var simpleSpace =
        skipMany1(satisfy(isSpace));


//
//  oneLineComment =
//      do{ try (string (commentLine languageDef))
//        ; skipMany (satisfy (/= '\n'))
//        ; return ()
//        }

var oneLineComment =
       do$( try_(string(languageDef.commentLine)) )
          ( skipMany, satisfy(function(c){ return c != '\n' }) )
          ( return_, null)


//
//  inCommentSingle
//      =   do{ try (string (commentEnd languageDef)); return () }
//      <|> do{ skipMany1 (noneOf startEnd)         ; inCommentSingle }
//      <|> do{ oneOf startEnd                      ; inCommentSingle }
//      <?> "end of comment"
//      where
//        startEnd   = nub (commentEnd languageDef ++ commentStart languageDef)

var startEnd = nub( slice( languageDef.commentEnd + languageDef.commentStart ) );

function _inCommentSingle(scope, state, k){ return inCommentSingle(scope, state, k) }

var inCommentSingle
            = ex( do_( try_ (string ( languageDef.commentEnd )) , return_(null) )
        ,"<|>", do_( skipMany1(noneOf (startEnd))          , _inCommentSingle )
        ,"<|>", do_( oneOf(startEnd)                       , _inCommentSingle )
        ,"<?>", "end of comment")



//  inCommentMulti
//      =   do{ try (string (commentEnd languageDef)) ; return () }
//      <|> do{ multiLineComment                     ; inCommentMulti }
//      <|> do{ skipMany1 (noneOf startEnd)          ; inCommentMulti }
//      <|> do{ oneOf startEnd                       ; inCommentMulti }
//      <?> "end of comment"
//      where
//        startEnd   = nub (commentEnd languageDef ++ commentStart languageDef)

function _inCommentMulti(scope, state, k){ return inCommentMulti(scope, state, k) }

var inCommentMulti
            = ex( do_( try_ (string ( languageDef.commentEnd )) , return_(null) )
        ,"<|>", do_( _multiLineComment                     , _inCommentMulti )
        ,"<|>", do_( skipMany1(noneOf (startEnd))          , _inCommentMulti )
        ,"<|>", do_( oneOf(startEnd)                       , _inCommentMulti )
        ,"<?>", "end of comment")



//  inComment
//      | nestedComments languageDef  = inCommentMulti
//      | otherwise                = inCommentSingle

var inComment = languageDef.nestedComments ? inCommentMulti : inCommentSingle;


//  multiLineComment =
//      do { try (string (commentStart languageDef))
//         ; inComment
//         }

function _multiLineComment(scope, state, k){ return multiLineComment(scope, state, k) }

var multiLineComment =
        do_( try_ (string (languageDef.commentStart))
           , inComment);


//  whiteSpace
//      | noLine && noMulti  = skipMany (simpleSpace <?> "")
//      | noLine             = skipMany (simpleSpace <|> multiLineComment <?> "")
//      | noMulti            = skipMany (simpleSpace <|> oneLineComment <?> "")
//      | otherwise          = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
//      where
//        noLine  = null (commentLine languageDef)
//        noMulti = null (commentStart languageDef)

var noLine   = null_(languageDef.commentLine);
var noMulti  = null_(languageDef.commentStart);

var whiteSpace = resolve(
    (noLine && noMulti) ? [skipMany, [simpleSpace ,"<?>", ""]] :
    noLine              ? [skipMany, [simpleSpace ,"<|>", multiLineComment ,"<?>", ""]] :
    noMulti             ? [skipMany, [simpleSpace ,"<|>", oneLineComment ,"<?>", ""]] :
                          [skipMany, [simpleSpace ,"<|>", oneLineComment ,"<|>", multiLineComment ,"<?>", ""]]
    );




//  -----------------------------------------------------------
//  -- Bracketing
//  -----------------------------------------------------------
//  parens p        = between (symbol "(") (symbol ")") p
//  braces p        = between (symbol "{") (symbol "}") p
//  angles p        = between (symbol "<") (symbol ">") p
//  brackets p      = between (symbol "[") (symbol "]") p
//
//  semi            = symbol ";"
//  comma           = symbol ","
//  dot             = symbol "."
//  colon           = symbol ":"
//
//  commaSep p      = sepBy p comma
//  semiSep p       = sepBy p semi
//
//  commaSep1 p     = sepBy1 p comma
//  semiSep1 p      = sepBy1 p semi



function parens(p){
    return between(symbol("("), symbol(")"), p);
}
function braces(p){
    return between(symbol("{"), symbol("}"), p);
}
function angles(p){
    return between(symbol("<"), symbol(">"), p);
}
function brackets(p){
    return between(symbol("["), symbol("]"), p);
}

var semi  = symbol(";");
var comma = symbol(",");
var dot   = symbol(".");
var colon = symbol(":");

function commaSep(p){
    return sepBy(p, comma);
}
function semiSep(p){
    return sepBy(p, semi);
}

function commaSep1(p){
    return sepBy1(p, comma);
}
function semiSep1(p){
    return sepBy1(p, semi);
}



//  -----------------------------------------------------------
//  -- Chars & Strings
//  -----------------------------------------------------------


var ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                       "FS","GS","RS","US","SP"];

var ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                       "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                       "CAN","SUB","ESC","DEL"];


var ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                       '\EM','\FS','\GS','\RS','\US','\SP'];

var ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                       '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                       '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL'];


//  -- escape code tables
//  escMap          = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
//  asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

var escMap          = zip(slice("abfnrtv\\\"\'"), slice("\a\b\f\n\r\t\v\\\"\'"));
var asciiMap        = zip(ascii3codes.concat(ascii2codes), ascii3.concat(ascii2));

//
//  charEsc         = choice (map parseEsc escMap)
//                  where
//                    parseEsc (c,code)     = do{ char c; return code }

var charEsc         = choice(map(parseEsc, escMap));
                    
function parseEsc(tuple){
    return do_( char_(tuple[0]), return_(tuple[1]) );
}


//  charAscii       = choice (map parseAscii asciiMap)
//                  where
//                    parseAscii (asc,code) = try (do{ string asc; return code })

var charAscii       = choice(map(parseAscii, asciiMap));

function parseAscii(tuple){
    return try_(do_( string(tuple[0]), return_(tuple[1]) ));
}


//  stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

var stringLetter    = satisfy(function(c){
                            return (c != '"') && (c != '\\') && (c > '\026');
                      });


//  escapeEmpty     = char '&'

var escapeEmpty     = char_('&');


//  escapeGap       = do{ many1 space
//                      ; char '\\' <?> "end of string gap"
//                      }

var escapeGap       = do$( many1, space )
                         ( char_('\\') ,"<?>", "end of string gap")
                        

//  charNum         = do{ code <- decimal
//                                <|> do{ char 'o'; number 8 octDigit }
//                                <|> do{ char 'x'; number 16 hexDigit }
//                      ; return (toEnum (fromInteger code))
//                      }

var charNum         = do$( "code" ,"<-", _decimal
                                      ,"<|>", do_( char_('o'), number(8, octDigit) )
                                      ,"<|>", do_( char_('x'), number(16, hexDigit) )
                         )
                         ( ret(function(scope){ return toEnum(fromInteger(scope.code)) }) )


//  charControl     = do{ char '^'
//                      ; code <- upper
//                      ; return (toEnum (fromEnum code - fromEnum 'A'))
//                      }

var charControl     = do$( char_('^') )
                         ( "code" ,"<-", upper )
                         ( ret(function(scope){ return toEnum(fromEnum(scope.code) - fromEnum('A'))  }) )
 

//  -- escape codes
//  escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl
//                  <?> "escape code"

var escapeCode      = ex(charEsc ,"<|>", charNum ,"<|>", charAscii ,"<|>", charControl
                    ,"<?>", "escape code")


//  charEscape      = do{ char '\\'; escapeCode }

var charEscape      = do_(char_('\\'), escapeCode);



//  charLetter      = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

var charLetter      = satisfy(function(c){
                            return (c != '\'') && (c != '\\') && (c > '\026');
                      });


//
//  characterChar   = charLetter <|> charEscape
//                  <?> "literal character"

var characterChar   = ex(charLetter ,"<|>", charEscape
                    ,"<?>", "literal character")


//  charLiteral     = lexeme (between (char '\'')
//                                    (char '\'' <?> "end of character")
//                                    characterChar )
//                  <?> "character"

var charLiteral     = ex(lexeme, [between, char_('\''), 
                                    [char_('\'') ,"<?>", "end of character"],
                                    characterChar]
                    ,"<?>", "character");


//
//  stringEscape    = do{ char '\\'
//                      ;     do{ escapeGap  ; return Nothing }
//                        <|> do{ escapeEmpty; return Nothing }
//                        <|> do{ esc <- escapeCode; return (Just esc) }
//                      }

var stringEscape    = do$( char_('\\') )
                         (        do_( escapeGap,   return_(Maybe.Nothing) )
                          ,"<|>", do_( escapeEmpty, return_(Maybe.Nothing) )
                          ,"<|>", do$( "esc" ,"<-", escapeCode)
                                     ( returnCall, Maybe.Just, "esc" )
                         )


//  stringChar      =   do{ c <- stringLetter; return (Just c) }
//                  <|> stringEscape
//                  <?> "string character"

var stringChar      = ex(do$( "c" ,"<-", stringLetter )
                            ( returnCall, Maybe.Just, "c" )
                      ,"<|>", stringEscape
                      ,"<?>", "string character");


//  stringLiteral   = lexeme (
//                    do{ str <- between (char '"')
//                                       (char '"' <?> "end of string")
//                                       (many stringChar)
//                      ; return (foldr (maybe id (:)) "" str)
//                      }
//                    <?> "literal string")

var stringLiteral   = ex(lexeme,
                          [do$( "str", "<-", between, char_('"'),
                                                      [char_('"') ,"<?>", "end of string"],
                                                      [many, stringChar]
                               )
                               (ret, function(scope){ return foldr(curry(maybe)(id, curry(cons)), "", scope.str) })
                          ,"<?>", "literal string"]
                      );



//  -----------------------------------------------------------
//  -- Numbers
//  -----------------------------------------------------------


//  number base baseDigit
//      = do{ digits <- many1 baseDigit
//          ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
//          ; seq n (return n)
//          }

function number(base, baseDigit){ 
    return do$( "digits" ,"<-", many1, baseDigit )
              ( ret, function(scope){
                        return foldl(function(x, d){
                                  return base * x + toInteger(digitToInt(d));
                              }, 0, scope.digits);
              })
}


//  decimal         = number 10 digit
//  hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }
//  octal           = do{ oneOf "oO"; number 8 octDigit  }

function _decimal(scope, state, k){ return decimal(scope, state, k) }

var decimal         = number(10, digit);
var hexadecimal     = do$( oneOf, "xX" ) ( number, 16, hexDigit )
var octal           = do$( oneOf, "oO" ) ( number, 8, octDigit  )



//
//  fraction        = do{ char '.'
//                      ; digits <- many1 digit <?> "fraction"
//                      ; return (foldr op 0.0 digits)
//                      }
//                    <?> "fraction"
//                  where
//                    op d f    = (f + fromIntegral (digitToInt d))/10.0

function _op(d, f){
    return (f + fromIntegral(digitToInt(d))) / 10.0;
}

var fraction        = ex(do$( char_('.'))
                            ( "digits" ,"<-", many1, digit ,"<?>", "fraction")
                            ( ret, function(scope){ return foldr(_op, 0.0, scope.digits) })
                        ,"<?>", "fraction")



//
//  sign            =   (char '-' >> return negate)
//                  <|> (char '+' >> return id)
//                  <|> return id

var sign            = ex([char_('-') ,">>", return_, negate]
                            ,"<|>", [char_('+') ,">>", return_, id]
                            ,"<|>", return_, id);


//
//  exponent'       = do{ oneOf "eE"
//                      ; f <- sign
//                      ; e <- decimal <?> "exponent"
//                      ; return (power (f e))
//                      }
//                    <?> "exponent"
//                  where
//                     power e  | e < 0      = 1.0/power(-e)
//                              | otherwise  = fromInteger (10^e)

function power(e){
    return (e < 0) ?  1.0 / power(-e) :  fromInteger(Math.pow(10,e));
}

var exponent_       = ex(do$( oneOf, "eE" )
                            ( "f" ,"<-", sign )
                            ( "e" ,"<-", decimal ,"<?>", "exponent" )
                            ( returnCall, power, "f", "e")
                        ,"<?>", "exponent");



//  fractExponent n = do{ fract <- fraction
//                      ; expo  <- option 1.0 exponent'
//                      ; return ((fromInteger n + fract)*expo)
//                      }
//                  <|>
//                    do{ expo <- exponent'
//                      ; return ((fromInteger n)*expo)
//                      }

function fractExponent(n){
    return ex(
         do$( "fract" ,"<-", fraction )
            ( "expo"  ,"<-", option, 1.0, exponent_ )
            ( ret, function(scope){ return fromInteger(n + scope.fract) * scope.expo })
        ,"<|>",
         do$( "expo", "<-", exponent_ )
            ( ret, function(scope){ return fromInteger(n) * scope.expo })
    );
}

//  -- floats
//  floating        = do{ n <- decimal
//                      ; fractExponent n
//                      }

var floating        = do$( "n" ,"<-", decimal)
                         ( function(scope, state, k){ return fractExponent(scope.n)(scope, state, k) })


//  fractFloat n    = do{ f <- fractExponent n
//                      ; return (Right f)
//                      }

function fractFloat(n){
    return do$( "f" ,"<-", fractExponent, n)
              ( returnCall, Either.Right, "f")
}


//
//  decimalFloat    = do{ n <- decimal
//                      ; option (Left n)
//                               (fractFloat n)
//                      }

var decimalFloat    = do$( "n" ,"<-", decimal )
                         ( function(scope, state, k){ 
                               return option(Either.Left(scope.n), fractFloat(scope.n))(scope, state, k);
                         })


//  zeroNumFloat    =  do{ n <- hexadecimal <|> octal
//                       ; return (Left n)
//                       }
//                  <|> decimalFloat
//                  <|> fractFloat 0
//                  <|> return (Left 0)


var zeroNumFloat    = ex(do$( "n" ,"<-", hexadecimal ,"<|>", octal )
                            ( returnCall, Either.Left, "n" )
                       ,"<|>", decimalFloat
                       ,"<|>", fractFloat(0)
                       ,"<|>", return_, Either.Left(0)
                       );




//  natFloat        = do{ char '0'
//                      ; zeroNumFloat
//                      }
//                    <|> decimalFloat

var natFloat        = ex(do_( char_('0'),
                            zeroNumFloat
                            )
                      ,"<|>", decimalFloat);



//  zeroNumber      = do{ char '0'
//                      ; hexadecimal <|> octal <|> decimal <|> return 0
//                      }
//                    <?> ""

var zeroNumber      = ex(do$( char_, '0')
                            ( hexadecimal ,"<|>", octal ,"<|>", decimal ,"<|>", return_, 0 )
                      ,"<?>", "");


//  nat             = zeroNumber <|> decimal

var nat             = parserPlus(zeroNumber, decimal);

//  -- integers and naturals
//  int             = do{ f <- lexeme sign
//                      ; n <- nat
//                      ; return (f n)
//                      }

var int_            = do$( "f" ,"<-", lexeme, sign )
                         ( "n" ,"<-", nat )
                         ( ret, function(scope){ return scope.f(scope.n) })



//  naturalOrFloat  = lexeme (natFloat) <?> "number"
//
//  float           = lexeme floating   <?> "float"
//  integer         = lexeme int        <?> "integer"
//  natural         = lexeme nat        <?> "natural"

var naturalOrFloat  = ex(lexeme, natFloat   ,"<?>", "number" ).resolve();

var float_          = ex(lexeme, floating   ,"<?>", "float"  ).resolve();
var integer         = ex(lexeme, int_       ,"<?>", "integer").resolve();
var natural         = ex(lexeme, nat        ,"<?>", "natural").resolve();




//  -----------------------------------------------------------
//  -- Operators & reserved ops
//  -----------------------------------------------------------


//  reservedOp name =
//      lexeme $ try $
//      do{ string name
//        ; notFollowedBy (opLetter languageDef) <?> ("end of " ++ show name)
//        }

function reservedOp(name){
    return ex(lexeme ,"$", try_,
                do$( string(name) ) 
                   ( notFollowedBy, languageDef.opLetter ,"<?>", "end of " + name )
            );
}


//  oper =
//      do{ c <- (opStart languageDef)
//        ; cs <- many (opLetter languageDef)
//        ; return (c:cs)
//        }
//      <?> "operator"

var oper =
        ex(do$( "c"  ,"<-", languageDef.opStart )
              ( "cs" ,"<-", many, languageDef.opLetter )
              ( returnCall, consJoin, "c", "cs" )
         ,"<?>", "operator");


//  isReservedOp name =
//      isReserved (sort (reservedOpNames languageDef)) name

function isReservedOp(name){
        return isReserved( isort( languageDef.reservedOpNames ), name);
}


//  operator =
//      lexeme $ try $
//      do{ name <- oper
//        ; if (isReservedOp name)
//           then unexpected ("reserved operator " ++ show name)
//           else return name
//        }

var operator =
        ex(lexeme ,"$", try_,
           do$( "name" ,"<-", oper )
              ( function(scope, state, k){
                    return (isReservedOp(scope.name) ? 
                        unexpected("reserved operator " + scope.name) :
                        return_(scope.name) )
                    (scope, state, k);
              })
        );





//  -----------------------------------------------------------
//  -- Identifiers & Reserved words
//  -----------------------------------------------------------


//  caseString name
//      | caseSensitive languageDef  = string name
//      | otherwise               = do{ walk name; return name }
//      where
//        walk []     = return ()
//        walk (c:cs) = do{ caseChar c <?> msg; walk cs }
//
//        caseChar c  | isAlpha c  = char (toLower c) <|> char (toUpper c)
//                    | otherwise  = char c
//
//        msg         = show name

function caseString(name){

    function walk(cs){
        return (!cs.length) ? return_(null) :
                       do_( label(caseChar(cs[0]), "" + name),
                            walk(slice(cs, 1)) );
    }

    function caseChar(c){
        return isAlpha(c) ? parserPlus( char_(c.toLowerCase()),
                                        char_(c.toUpperCase())) : 
                            char_(c);
    }

    return languageDef.caseSensitive ? string(name) : do_( walk(name), return_(name) );

}

//  reserved name =
//      lexeme $ try $
//      do{ caseString name
//        ; notFollowedBy (identLetter languageDef) <?> ("end of " ++ show name)
//        }

function reserved(name){
    return ex(lexeme ,"$", try_,
              do$( caseString(name) )
                 ( notFollowedBy, languageDef.identLetter ,"<?>", "end of " + name )
            );
}


//  ident
//      = do{ c <- identStart languageDef
//          ; cs <- many (identLetter languageDef)
//          ; return (c:cs)
//          }
//      <?> "identifier"

var ident
        = ex(do$( "c"  ,"<-", languageDef.identStart )
                ( "cs" ,"<-", many, languageDef.identLetter )
                ( returnCall, consJoin, "c", "cs" )
           ,"<?>", "identifier");


//  isReserved names name
//      = scan names
//      where
//        scan []       = False
//        scan (r:rs)   = case (compare r name) of
//                          LT  -> scan rs
//                          EQ  -> True
//                          GT  -> False

function isReserved(names, name){
    function scan(rs){
        if(!rs.length) 
            return false;

        var ord = compare(rs[0], name);

        return  ord.LT ? scan(slice(rs, 1)) :
                ord.EQ ? true : 
                ord.GT ? false : null;
    }

    return scan(names);
}


//  isReservedName name
//      = isReserved theReservedNames caseName
//      where
//        caseName      | caseSensitive languageDef  = name
//                      | otherwise               = map toLower name

function isReservedName(name){
    var caseName = languageDef.caseSensitive ? name : name.toLowerCase();

    return isReserved(theReservedNames, caseName);
}


//  identifier =
//      lexeme $ try $
//      do{ name <- ident
//        ; if (isReservedName name)
//           then unexpected ("reserved word " ++ show name)
//           else return name
//        }

var identifier =
        ex(lexeme ,"$", try_,
           do$( "name" ,"<-", ident )
              ( function(scope, state, k){
                    return ( isReservedName(scope.name) ? 
                                unexpected("reserved word " + scope.name) : 
                                return_(scope.name)
                            )(scope, state, k);
              })
        );


//  theReservedNames
//      | caseSensitive languageDef  = sortedNames
//      | otherwise               = map (map toLower) sortedNames
//      where
//        sortedNames   = sort (reservedNames languageDef)

var sortedNames = isort(languageDef.reservedNames);
var theReservedNames = languageDef.caseSensitive ? 
                            sortedNames : 
                            map( function(str){ return str.toLowerCase() }, sortedNames );



    return GenTokenParser.TokenParser(record, {
        identifier : identifier,
        reserved   : reserved,
        operator   : operator,
        reservedOp : reservedOp,
        
        charLiteral    : charLiteral,
        stringLiteral  : stringLiteral,
        natural        : natural,
        integer        : integer,
        float_         : float_,
        naturalOrFloat : naturalOrFloat,
        decimal        : decimal,
        hexadecimal    : hexadecimal,
        octal          : octal,
        
        symbol     : symbol,
        lexeme     : lexeme,
        whiteSpace : whiteSpace,
        
        parens     : parens,
        braces     : braces,
        angles     : angles,
        brackets   : brackets,
        squares    : brackets,
        semi       : semi,
        comma      : comma,
        colon      : colon,
        dot        : dot,
        semiSep    : semiSep,
        semiSep1   : semiSep1,
        commaSep   : commaSep,
        commaSep1  : commaSep1
    });
}


namespace("Text_Parsec_Token", {
    GenLanguageDef  : GenLanguageDef,
    GenTokenParser  : GenTokenParser,
    makeTokenParser : makeTokenParser
});/// <reference path="Prim.js" />
/// <reference path="Char.js" />
/// <reference path="Combinator.js" />
/// <reference path="Token.js" />

// -------------------------------------------------
// Language
// -------------------------------------------------


//-- A helper module that defines some language definitions that can be used
//-- to instantiate a token parser (see "Text.Parsec.Token").

//module Text.Parsec.Language
//    ( haskellDef, haskell
//    , mondrianDef, mondrian
//    , emptyDef
//    , haskellStyle
//    , javaStyle
//    , LanguageDef
//    , GenLanguageDef
//    ) where
//
//import Text.Parsec
//import Text.Parsec.Token


//-----------------------------------------------------------
//-- minimal language definition
//--------------------------------------------------------
//
//-- TODO: This seems wrong
//-- < This is the most minimal token definition. It is recommended to use
//-- this definition as the basis for other definitions. @emptyDef@ has
//-- no reserved names or operators, is case sensitive and doesn't accept
//-- comments, identifiers or operators.
//
//emptyDef   :: LanguageDef st
//emptyDef    = LanguageDef
//               { commentStart   = ""
//               , commentEnd     = ""
//               , commentLine    = ""
//               , nestedComments = True
//               , identStart     = letter <|> char '_'
//               , identLetter    = alphaNum <|> oneOf "_'"
//               , opStart        = opLetter emptyDef
//               , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
//               , reservedOpNames= []
//               , reservedNames  = []
//               , caseSensitive  = True
//               }

var emptyDefOpLetter = oneOf(":!#$%&*+./<=>?@\\^|-~");

var emptyDef = GenLanguageDef.LanguageDef(record,
               { commentStart   : ""
               , commentEnd     : ""
               , commentLine    : ""
               , nestedComments : true
               , identStart     : exs(letter   ,"<|>", char_('_'))
               , identLetter    : exs(alphaNum ,"<|>", oneOf("_'"))
               , opStart        : emptyDefOpLetter
               , opLetter       : emptyDefOpLetter
               , reservedOpNames: []
               , reservedNames  : []
               , caseSensitive  : true
               });


//-----------------------------------------------------------
//-- Styles: haskellStyle, javaStyle
//-----------------------------------------------------------
//
//-- | This is a minimal token definition for Haskell style languages. It
//-- defines the style of comments, valid identifiers and case
//-- sensitivity. It does not define any reserved words or operators.
//
//haskellStyle :: LanguageDef st
//haskellStyle = emptyDef
//                { commentStart   = "{-"
//                , commentEnd     = "-}"
//                , commentLine    = "--"
//                , nestedComments = True
//                , identStart     = letter
//                , identLetter    = alphaNum <|> oneOf "_'"
//                , opStart        = opLetter haskellStyle
//                , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
//                , reservedOpNames= []
//                , reservedNames  = []
//                , caseSensitive  = True
//                }

var haskellStyle = GenLanguageDef.LanguageDef(record,
               { commentStart   : "{-"
               , commentEnd     : "-}"
               , commentLine    : "--"
               , nestedComments : true
               , identStart     : letter
               , identLetter    : ex(alphaNum   ,"<|>", oneOf, "_'").resolve()
               , opStart        : emptyDefOpLetter
               , opLetter       : emptyDefOpLetter
               , reservedOpNames: []
               , reservedNames  : []
               , caseSensitive  : true
               });


//-- | This is a minimal token definition for Java style languages. It
//-- defines the style of comments, valid identifiers and case
//-- sensitivity. It does not define any reserved words or operators.
//
//javaStyle  :: LanguageDef st
//javaStyle   = emptyDef
//      { commentStart   = "/*"
//      , commentEnd     = "*/"
//      , commentLine    = "//"
//      , nestedComments = True
//      , identStart     = letter
//      , identLetter    = alphaNum <|> oneOf "_'"
//      , reservedNames  = []
//      , reservedOpNames= []
//      , caseSensitive  = False
//      }

var javaStyle = GenLanguageDef.LanguageDef(record,
               { commentStart    : "/*"
               , commentEnd      : "*/"
               , commentLine     : "//"
               , nestedComments  : true
               , identStart      : letter
               , identLetter     : exs(alphaNum   ,"<|>", oneOf, "_'")
               , opStart         : emptyDefOpLetter
               , opLetter        : emptyDefOpLetter
               , reservedOpNames : []
               , reservedNames   : []
               , caseSensitive   : false
               });

//-----------------------------------------------------------
//-- Haskell
//-----------------------------------------------------------


//-- | The language definition for the language Haskell98.
//
//haskell98Def :: LanguageDef st
//haskell98Def = haskellStyle
//                { reservedOpNames= ["::","..","=","\\","|","<-","->","@","~","=>"]
//                , reservedNames  = ["let","in","case","of","if","then","else",
//                                    "data","type",
//                                    "class","default","deriving","do","import",
//                                    "infix","infixl","infixr","instance","module",
//                                    "newtype","where",
//                                    "primitive"
//                                    -- "as","qualified","hiding"
//                                   ]
//                }

var haskell98Def = haskellStyle.update(
                { reservedOpNames : ["::","..","=","\\","|","<-","->","@","~","=>"]
                , reservedNames   : ["let","in","case","of","if","then","else",
                                     "data","type",
                                     "class","default","deriving","do","import",
                                     "infix","infixl","infixr","instance","module",
                                     "newtype","where",
                                     "primitive"
                                   // ,"as","qualified","hiding"
                                    ]
                });



//-- | The language definition for the Haskell language.
//
//haskellDef  :: LanguageDef st
//haskellDef   = haskell98Def
//          { identLetter    = identLetter haskell98Def <|> char '#'
//          , reservedNames  = reservedNames haskell98Def ++
//                     ["foreign","import","export","primitive"
//                     ,"_ccall_","_casm_"
//                     ,"forall"
//                     ]
//                }

var haskellDef = haskell98Def.update(
            { identLetter   : exs(haskell98Def.identLetter ,"<|>", char_('#'))
            , reservedNames : haskell98Def.reservedNames.concat(
                                  ["foreign","import","export","primitive"
                                  ,"_ccall_","_casm_"
                                  ,"forall"
                                  ])
            });

//-- | A lexer for the haskell language.
//
//haskell :: TokenParser st
//haskell      = makeTokenParser haskellDef

//var haskell = makeTokenParser(haskellDef);


//-----------------------------------------------------------
//-- Mondrian
//-----------------------------------------------------------

//-- | The language definition for the language Mondrian.
//
//mondrianDef :: LanguageDef st
//mondrianDef = javaStyle
//      { reservedNames = [ "case", "class", "default", "extends"
//                , "import", "in", "let", "new", "of", "package"
//                ]
//                , caseSensitive  = True
//      }


var mondrianDef = javaStyle.update(
        { reservedNames : [ "case", "class", "default", "extends"
                          , "import", "in", "let", "new", "of", "package"
                          ]
        , caseSensitive : true
        });

//-- | A lexer for the mondrian language.
//
//mondrian :: TokenParser st
//mondrian    = makeTokenParser mondrianDef

//var mondrian = makeTokenParser(mondrianDef);



namespace("Text_Parsec_Language", {
    emptyDef    : emptyDef,
    haskellStyle: haskellStyle,
    javaStyle   : javaStyle,
    haskellDef  : haskellDef,
    mondrianDef : mondrianDef,
    getHaskell  : function(){ return makeTokenParser(haskellDef)  },
    getMondrian : function(){ return makeTokenParser(mondrianDef) }
});
}());;(function(){
/// <reference path="Parsec/Prim.js" />
/// <reference path="Parsec/Char.js" />
/// <reference path="Parsec/Combinator.js" />
/// <reference path="Parsec/Token.js" />
/// <reference path="Parsec/Language.js" />
/// <reference path="Parsec/Expr.js" />

namespace("Text_Parsec")
importSubmodules("Text_Parsec",
    ["Prim"
    ,"Char"
    ,"Combinator"
    ,"Token"
    ,"Language"
    ,"Expr"
    ])



}());
}());
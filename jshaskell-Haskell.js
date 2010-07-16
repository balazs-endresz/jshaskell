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


}());
}());
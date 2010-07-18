
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

function elem(x, xs){
    return (xs.indexOf ? xs.indexOf(x) : indexOf(x, xs)) != -1; //TODO
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


function readHex(str){
    return parseInt(str.join ? str.join("") : str, 16);
}

function readOct(str){
    return parseInt(str.join ? str.join("") : str, 8);
}

function digitToInt(c){
    var res = parseInt(c, 16);
    return isNaN(res) ? error(digitToInt) : res;
}

var round = Math.round;

var toInteger = parseInt; //TODO

var fromInteger = id; //TODO

var fromIntegral = id; //TODO

function range(lower, upper){
    return {
        indexOf: function(ch){ return (ch >= lower && ch <= upper) ? true : -1 },
        toString: function(){ return "range(" + lower + ", " + upper + ")" }
    };
}


// * the last return value indicates that the thunk is fully evaluated (i.e. it's not a function), 
//   and the result is passed to the continuation/callback specified in the program
// * the second parameter is for making it asynchronous (unless it's Infinity)
// * if the thunks are not CPS-based then only the synchronous method returns the result,
//   which must not be a function
// * the unrolled loops are not _entirely_ pointless: e.g. a complex parser (like the one in WebBits)
//   can build up hundreds of thousands of thunks even for a smaller document

function evalThunks(thunk, hundredTimes) {

    if (!hundredTimes || hundredTimes == Infinity) {
        try {
            while ((thunk = thunk()()()()()()()()()()()()()()()()()()()()()()()()()
                                 ()()()()()()()()()()()()()()()()()()()()()()()()()
                                 ()()()()()()()()()()()()()()()()()()()()()()()()()
                                 ()()()()()()()()()()()()()()()()()()()()()()()()()
            ));
        } catch (_) { }
        return thunk;
    }

    function next() {
        try {
            var i = hundredTimes;
            do {
                thunk = thunk()()()()()()()()()()()()()()()()()()()()()()()()()
                             ()()()()()()()()()()()()()()()()()()()()()()()()()
                             ()()()()()()()()()()()()()()()()()()()()()()()()()
                             ()()()()()()()()()()()()()()()()()()()()()()()()();
            } while (--i);
            setTimeout(next, 1);
        } catch (_) { }
    }
    next();
}





namespace("Haskell_Main", {
    curry       : curry,
    const_      : const_,
    isArray     : isArray,
    isDefined   : isDefined,
    slice       : slice,
    map         : map,
    imap        : imap,
    filter      : filter,
    ifilter     : ifilter,
    indexOf     : indexOf,
    lastIndexOf : lastIndexOf,
    isort       : isort,

    compose     : compose,
    compose1    : compose1,
    call        : call,
    id          : id,
    flip        : flip,
    cons        : cons,
    consJoin    : consJoin,
    negate      : negate,
    null_       : null_,
    elem        : elem,

    digitToInt  : digitToInt,
    range       : range,
    extend      : extend,
    namespace   : namespace,
    toInteger   : toInteger,
    fromInteger : fromInteger,
    fromIntegral: fromIntegral,

    readHex     : readHex,
    readOct     : readOct,
    chr         : chr,
    round       : round,
    typeOf      : typeOf,
    strictEq    : strictEq,
    strictNe    : strictNe,
    lt          : lt,
    le          : le,
    gt          : gt,
    ge          : ge,
    not         : not,
    negate      : negate,
    evalThunks  : evalThunks
});


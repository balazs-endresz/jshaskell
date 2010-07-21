
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
    },
    ignoreAsserts = true; //TODO

global.global = global;
global.namespace = namespace;
global.importSubmodules = importSubmodules;

//function isType(str, a){ return _toString.call(a) == "[object " + str + "]" }

function isArray(a){ return _toString.call(a) == "[object Array]" }

function isDefined(x){ return x !== undef }

function strictEq(a, b){ return a === b }
function strictNe(a, b){ return a !== b }

function unsafeAdd(a, b){ return a + b }
function unsafeSub(a, b){ return a - b }
function unsafeMul(a, b){ return a * b }
function unsafeDiv(a, b){ return a / b }



function lt(a, b){ return a < b  }
function le(a, b){ return a <= b }
function gt(a, b){ return a > b  }
function ge(a, b){ return a >= b }

function negate(a){
    return -a;
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

//e.g. eta(error, "Type error") returns a function, which calls error
//with the rest of the arguments, throwing an exception
function eta(f){ 
    var args = slice(arguments, 1)
    return function(){
        return f.apply(null, args)
    }
}


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


// -------------------------------------------------
// Basic List/String functions,
// use ONLY the ones in Prelude or Data_List !!!
// -------------------------------------------------

var stringAsArray = !!"a"[0]; //TODO

//TODO!!! use only in low level code, 
//replace occurances with tail/toArray in jsparsec
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
    var isStr = arr.charAt,
        r = toArray(arr);
    r.sort();
    return isStr ? r.join("") : r;
}

var imap = _map ?
    function(f, arr){ return _map.call(arr, f) } :
    function(f, arr){    
        var res = [], i = 0, l = arr.length;
        for(; i < l; ++i)
            res[i] = f(arr[i], i, arr);
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


function toArray(a){
    if(!a.charAt)
        return _slice.call(a, 0);
    for(var r = [], i = 0, l = a.length; i < l; ++i)
        r.push(a.charAt(i));
    return r;
}


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

var Bool = Boolean;

namespace("Haskell_Main", {
    Bool        : Bool,
    isArray     : isArray,
    isDefined   : isDefined,
    slice       : slice,
    imap        : imap,
    ifilter     : ifilter,
    indexOf     : indexOf,
    lastIndexOf : lastIndexOf,
    isort       : isort,
    range       : range,
    extend      : extend,
    namespace   : namespace,
    typeOf      : typeOf,
    strictEq    : strictEq,
    strictNe    : strictNe,
    unsafeAdd   : unsafeAdd,
    unsafeSub   : unsafeSub,
    unsafeMul   : unsafeMul,
    unsafeDiv   : unsafeDiv,
    lt          : lt,
    le          : le,
    gt          : gt,
    ge          : ge,
    negate      : negate,
    evalThunks  : evalThunks,
    toArray     : toArray,
    curry       : curry,
    error       : error

});


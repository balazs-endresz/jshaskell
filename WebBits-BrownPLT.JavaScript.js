/** @license
 * JavaScript analysis tools based on JSParsec
 * 
 * http://code.google.com/p/jshaskell/
 * 
 * Copyright (c) 2010 Balazs Endresz (balazs.endresz@gmail.com)
 * Dual licensed under the MIT and GPL licenses.
 *
 * This is a port of the WebBits library (BrownPLT.JavaScript):
 * http://hackage.haskell.org/package/WebBits-2.0
 * Copyright (c) 2007-2009 Arjun Guha, Claudiu Saftoiu, and Spiridon Eliopoulos
 * 
 * ------------------------------------------------------------
 *
 */

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

/// <reference path="Main.js" />

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
        args = imap(function(e){return e instanceof Recurse ? rec : e}, args);
    
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
    createDo: createDo,
    Scope   : Scope
})/// <reference path="Main.js" />

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

namespace("Haskell_TypeClass", {
     typeclass   : typeclass
    ,VARARG      : VARARG
    ,instance    : instance
    ,getInstance : getInstance
    ,asTypeOf    :asTypeOf
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

namespace("Haskell_DataType", {
    data      : data,
    ADT       : ADT,
    record    : record,
    accessor  : accessor,
    accessors : accessors
});
}());;(function(){
/// <reference path="Haskell/Main.js" />
/// <reference path="Haskell/DataType.js" />
/// <reference path="Haskell/TypeClass.js" />
/// <reference path="Haskell/Expression.js" />
/// <reference path="Haskell/Do.js" />

namespace("Haskell")

importSubmodules("Haskell",
    ["Main"
    ,"DataType"
    ,"TypeClass"
    ,"Expression"
    ,"Do"
    ])

}());;(function(){
;var curry = NS['Haskell'].curry, const_ = NS['Haskell'].const_, isArray = NS['Haskell'].isArray, isDefined = NS['Haskell'].isDefined, slice = NS['Haskell'].slice, map = NS['Haskell'].map, imap = NS['Haskell'].imap, filter = NS['Haskell'].filter, ifilter = NS['Haskell'].ifilter, indexOf = NS['Haskell'].indexOf, lastIndexOf = NS['Haskell'].lastIndexOf, isort = NS['Haskell'].isort, compose = NS['Haskell'].compose, compose1 = NS['Haskell'].compose1, call = NS['Haskell'].call, id = NS['Haskell'].id, flip = NS['Haskell'].flip, cons = NS['Haskell'].cons, consJoin = NS['Haskell'].consJoin, negate = NS['Haskell'].negate, null_ = NS['Haskell'].null_, elem = NS['Haskell'].elem, digitToInt = NS['Haskell'].digitToInt, range = NS['Haskell'].range, extend = NS['Haskell'].extend, namespace = NS['Haskell'].namespace, toInteger = NS['Haskell'].toInteger, fromInteger = NS['Haskell'].fromInteger, fromIntegral = NS['Haskell'].fromIntegral, readHex = NS['Haskell'].readHex, readOct = NS['Haskell'].readOct, chr = NS['Haskell'].chr, round = NS['Haskell'].round, typeOf = NS['Haskell'].typeOf, strictEq = NS['Haskell'].strictEq, strictNe = NS['Haskell'].strictNe, lt = NS['Haskell'].lt, le = NS['Haskell'].le, gt = NS['Haskell'].gt, ge = NS['Haskell'].ge, not = NS['Haskell'].not, evalThunks = NS['Haskell'].evalThunks, data = NS['Haskell'].data, ADT = NS['Haskell'].ADT, record = NS['Haskell'].record, accessor = NS['Haskell'].accessor, accessors = NS['Haskell'].accessors, typeclass = NS['Haskell'].typeclass, VARARG = NS['Haskell'].VARARG, instance = NS['Haskell'].instance, getInstance = NS['Haskell'].getInstance, asTypeOf = NS['Haskell'].asTypeOf, operators = NS['Haskell'].operators, infix = NS['Haskell'].infix, infixl = NS['Haskell'].infixl, infixr = NS['Haskell'].infixr, op = NS['Haskell'].op, no = NS['Haskell'].no, resolve = NS['Haskell'].resolve, recurse = NS['Haskell'].recurse, Recurse = NS['Haskell'].Recurse, exl = NS['Haskell'].exl, exs = NS['Haskell'].exs, createDo = NS['Haskell'].createDo, Scope = NS['Haskell'].Scope;﻿/// <reference path="../../jshaskell/src/Haskell.js" local />


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
}());;(function(){
;var curry = NS['Haskell'].curry, const_ = NS['Haskell'].const_, isArray = NS['Haskell'].isArray, isDefined = NS['Haskell'].isDefined, slice = NS['Haskell'].slice, map = NS['Haskell'].map, imap = NS['Haskell'].imap, filter = NS['Haskell'].filter, ifilter = NS['Haskell'].ifilter, indexOf = NS['Haskell'].indexOf, lastIndexOf = NS['Haskell'].lastIndexOf, isort = NS['Haskell'].isort, compose = NS['Haskell'].compose, compose1 = NS['Haskell'].compose1, call = NS['Haskell'].call, id = NS['Haskell'].id, flip = NS['Haskell'].flip, cons = NS['Haskell'].cons, consJoin = NS['Haskell'].consJoin, negate = NS['Haskell'].negate, null_ = NS['Haskell'].null_, elem = NS['Haskell'].elem, digitToInt = NS['Haskell'].digitToInt, range = NS['Haskell'].range, extend = NS['Haskell'].extend, namespace = NS['Haskell'].namespace, toInteger = NS['Haskell'].toInteger, fromInteger = NS['Haskell'].fromInteger, fromIntegral = NS['Haskell'].fromIntegral, readHex = NS['Haskell'].readHex, readOct = NS['Haskell'].readOct, chr = NS['Haskell'].chr, round = NS['Haskell'].round, typeOf = NS['Haskell'].typeOf, strictEq = NS['Haskell'].strictEq, strictNe = NS['Haskell'].strictNe, lt = NS['Haskell'].lt, le = NS['Haskell'].le, gt = NS['Haskell'].gt, ge = NS['Haskell'].ge, not = NS['Haskell'].not, evalThunks = NS['Haskell'].evalThunks, data = NS['Haskell'].data, ADT = NS['Haskell'].ADT, record = NS['Haskell'].record, accessor = NS['Haskell'].accessor, accessors = NS['Haskell'].accessors, typeclass = NS['Haskell'].typeclass, VARARG = NS['Haskell'].VARARG, instance = NS['Haskell'].instance, getInstance = NS['Haskell'].getInstance, asTypeOf = NS['Haskell'].asTypeOf, operators = NS['Haskell'].operators, infix = NS['Haskell'].infix, infixl = NS['Haskell'].infixl, infixr = NS['Haskell'].infixr, op = NS['Haskell'].op, no = NS['Haskell'].no, resolve = NS['Haskell'].resolve, recurse = NS['Haskell'].recurse, Recurse = NS['Haskell'].Recurse, exl = NS['Haskell'].exl, exs = NS['Haskell'].exs, createDo = NS['Haskell'].createDo, Scope = NS['Haskell'].Scope;;var Unit = NS['Prelude'].Unit, Tuple = NS['Prelude'].Tuple, Maybe = NS['Prelude'].Maybe, Ordering = NS['Prelude'].Ordering, Either = NS['Prelude'].Either, Eq = NS['Prelude'].Eq, Ord = NS['Prelude'].Ord, Functor = NS['Prelude'].Functor, Monad = NS['Prelude'].Monad, Show = NS['Prelude'].Show, foldl = NS['Prelude'].foldl, foldr = NS['Prelude'].foldr, zip = NS['Prelude'].zip, replicate = NS['Prelude'].replicate, sort = NS['Prelude'].sort, nub = NS['Prelude'].nub, maybe = NS['Prelude'].maybe, lookup = NS['Prelude'].lookup, span = NS['Prelude'].span, elemIndex = NS['Prelude'].elemIndex, uncons = NS['Prelude'].uncons, compare = NS['Prelude'].compare, fst = NS['Prelude'].fst, snd = NS['Prelude'].snd, uncurry = NS['Prelude'].uncurry, until = NS['Prelude'].until, fix = NS['Prelude'].fix, fix_ = NS['Prelude'].fix_, isSpace = NS['Prelude'].isSpace, isUpper = NS['Prelude'].isUpper, isLower = NS['Prelude'].isLower, isAlpha = NS['Prelude'].isAlpha, isAlphaNum = NS['Prelude'].isAlphaNum, isDigit = NS['Prelude'].isDigit, isHexDigit = NS['Prelude'].isHexDigit, isOctDigit = NS['Prelude'].isOctDigit;/// <reference path="../../../../jshaskell/src/Haskell.js" local />
/// <reference path="../../../../base/src/Prelude.js" local />

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

        this.index  = result.index;
        this.length = result.length;

        return result;
    },

    putCached: function(pid, index, cached) {
        if(!this.memoize)
            return false;
        
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


// ast:       is the AST returned by the parse, which doesn't need to be successful
//                this is the value that Functor, Applicative, and Monad functions operate on
// success:   might be true or false
// expecting: contains the value that the parser expected but haven't matched completely or at all
//                It's either a single string, or an object with a property 'string' and 'at'.
//                If it's just a string, then the index can be determined from ParseState.index,
//                else the latter form should be used (this might be changed later!).
//                It might be an array of these values, which represents a choice.

function unexpected(name){
    return function(scope, state, k){
        return k({ast: null, success: false, expecting: {unexpected: name}});
    };
}

//accepts an identifier string, see usage with notFollowedBy
function unexpectedIdent(name){
    return function(scope, state, k){
        return k({ast: null, success: false, expecting: {unexpected: scope[name]}});
    };
}


function parserFail(msg){
    return function(scope, state, k){
        return k({success: false, expecting: msg});
    };
};

var fail = parserFail;


function parserZero(scope, state, k){
    return k({success: false});
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


function run(p, strOrState, complete, error, async){
    var input = strOrState instanceof ParseState ? strOrState : ps(strOrState);
    evalThunks(function(){ return p(new Scope(), input, function(result){
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
    })}, async);
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
        return function(){ return p(scope, state, function(result){
            if(result.success){
                return function(){ return f(result.ast)(scope, state, k) }
            }else{
                return k(result);
            }
        })};
    };
}


var do2 = function(p1, p2){
    function fn(scope, state, k){
        return function(){ return p1(scope, state, function(result){
            return result.success ? p2(scope, state, k) : k(result); //TODO: p2
        })};
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
        return function(){ return p(scope, state, function(result){
            if(result.success)
                scope[name] = result.ast;
            return k(result);
        })};
    };
}


function ret(name, more){
    var args;
    if(more) 
        args = slice(arguments);

    return function(scope, state, k){

        return function(){ return function(){
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

            return k({ast: ast, success: true});

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
    return k({ast: state.index, success: true});
}

var getParserState = getPosition; //TODO?

function setPosition(id){
    var type = typeof id;
    return function(scope, state, k){
        state.scrollTo(type == "string" ? scope[id] : id);
        return k({success: true});
    };
}

var setParserState = setPosition; //TODO?

//in contrast with Haskell here's no closure in the do_ notation,
//it's simulated with `bind` and `ret`,
//this function does what `pure` and `return` do in Haskell
function parserReturn(value){
    return function(scope, state, k){
        return k({ast: value, success: true});
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
        return function(){ return p1(scope, state, function(result){
            var errors =  [];

            function handleError(result){
                var err = result.expecting;
                if(err){
                    if(err.constructor == Array)
                        errors = errors.concat(err);
                    else
                        errors.push(err);
                }
                result.expecting = result.success ? undef : errors;
            }
            
            handleError(result);
            if(result.ast !== undef)
                return function(){ return k(result) };
            else
                return function(){ return p2(scope, state, function(result){
                    handleError(result);
                    return k(result);
                })};
            
        })};
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
                return function(){ return parser(scope, state, function(result){
                    i++;
                    if(!result.success)
                        return k(result);
                    if(result.ast !== undef)
                        ast.push(result.ast);
                    return i < length ? next(parsers[i])(scope, state, k) : k(result);
                })};
            };
        }

        return function(){ return next(parsers[i])(scope, state, function(result){
            var success = result.success;
            return k({ast: ast, success: success, expecting: success ? undef : result.expecting });
        })};
    };
}

function _many(onePlusMatch){
    return function(parser){
        return function(scope, state, k){
            var matchedOne = false,
                ast = [];
            
            function next(parser){
                return function(scope, state, k){
                    return function(){ return parser(scope, state, function(result){
                        if(!result.success)
                            return k(result);
                            
                        matchedOne = true;
                        if(result.ast !== undef)
                            ast.push(result.ast);
                                
                        return next(parser)(scope, state, k);
                    })};
                };
            }
    
            return function(){ return next(parser)(scope, state, function(result){
                var success = !onePlusMatch || (matchedOne && onePlusMatch);
                return k({ast: success ? ast : undef
                         ,success: success
                         ,expecting: success ? undef : result.expecting
                         });
            })};
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
                
            return function(){ return p1(scope, state, function(result){
                    
                    result = fn(arg2, result, state, startIndex);
                    
                    state.putCached(pid, startIndex, result);
                    return k(result);
                })};
            
        };
        combinator.constructor = Parser;
        return combinator;
    };
}


var try_ = tokenPrimP1(function(_, result, state, startIndex){
    if(result.success)
        return result;
    state.scrollTo(startIndex);
    return {ast: undef, success: false, expecting: result.expecting };
});


var skipMany = function(p){
    return tokenPrimP1(function(_, result, state, startIndex){
        return {ast: undef, success: result.success, expecting: result.expecting };
    })(many(p), null);
};

//string :: Char -> Parser
var char_ = tokenPrim(function(c, state, startIndex){
    if(state.length > 0 && state.at(0) == c){
        state.scroll(1);
        return {ast: c, success: true};
    }
    return {success: false, expecting: c};
});


//string :: (Char -> Bool) -> Parser
var satisfy = tokenPrim(function(cond, state){
    var fstchar = state.at(0);
    if(state.length > 0 && cond(fstchar)){
        state.scroll(1);
        return {ast: fstchar, success: true};
    }
    return {success: false, expecting: fstchar};
});



//string :: String -> Parser
var string = function(s){ //TODO
    return tokenPrimP1(function(_, result, state, startIndex){
        var ast = result.ast.join("");
        return {ast: ast.length ? ast : undef //TODO
               ,success: result.success
               ,expecting: result.success ? undef : {at:startIndex, expecting: s}
               };
    })(tokens(map(char_, s)), null);
};


//tokenPrimP1 :: (a -> parser1Result -> ParseState -> startIndex -> newResult)
//              -> (Parser -> a -> Parser)
//label :: Parser -> String -> Parser
var label = tokenPrimP1(function(str, result, state, startIndex){
    return result.success ? result : 
        {ast: result.ast, success: false, expecting: {at: startIndex, expecting: str}};
});


//accepts a regexp or a string
//in case of a string it either matches the whole string or nothing

//match :: StringOrRegex -> Parser
var match = tokenPrim(function(sr, state){
        if(typeof sr == "string"){
            if(state.substring(0, sr.length) == sr){
                state.scroll(sr.length);
                return {ast: sr, success: true};
            }else
                return {success: false, expecting: sr};
                        
        }else if(sr.exec){
            var rx = new RegExp("^" + sr.source);
            var substr = state.substring(0);
            var match = rx.exec(substr);
            match = match && match[0];
            var length = match && match.length;
            var matched = substr.substr(0, length);
            if(length){
                state.scroll(length);
                return {ast: matched, success: true};
            }else
                return {success: false, expecting: sr.source};
        }
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

    //the default implementation can be used too, which is slightly slower
    //because `arguments` and `apply` is used instead of directly calling each function
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
    //var inst = getInstance(Monad, typeOf(ms[0]));

    function k(m1, m2){
        return do_(
            bind("x", m1),
            bind("xs", m2),
            ret(withBound(cons, "x", "xs"))
        );
    }

    return foldr(k, return_([]), ms);
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



}());;(function(){
;var data = NS['Haskell_DataType'].data, ADT = NS['Haskell_DataType'].ADT, record = NS['Haskell_DataType'].record, accessor = NS['Haskell_DataType'].accessor, accessors = NS['Haskell_DataType'].accessors;;var Unit = NS['Prelude'].Unit, Tuple = NS['Prelude'].Tuple, Maybe = NS['Prelude'].Maybe, Ordering = NS['Prelude'].Ordering, Either = NS['Prelude'].Either, Eq = NS['Prelude'].Eq, Ord = NS['Prelude'].Ord, Functor = NS['Prelude'].Functor, Monad = NS['Prelude'].Monad, Show = NS['Prelude'].Show, foldl = NS['Prelude'].foldl, foldr = NS['Prelude'].foldr, zip = NS['Prelude'].zip, replicate = NS['Prelude'].replicate, sort = NS['Prelude'].sort, nub = NS['Prelude'].nub, maybe = NS['Prelude'].maybe, lookup = NS['Prelude'].lookup, span = NS['Prelude'].span, elemIndex = NS['Prelude'].elemIndex, uncons = NS['Prelude'].uncons, compare = NS['Prelude'].compare, fst = NS['Prelude'].fst, snd = NS['Prelude'].snd, uncurry = NS['Prelude'].uncurry, until = NS['Prelude'].until, fix = NS['Prelude'].fix, fix_ = NS['Prelude'].fix_, isSpace = NS['Prelude'].isSpace, isUpper = NS['Prelude'].isUpper, isLower = NS['Prelude'].isLower, isAlpha = NS['Prelude'].isAlpha, isAlphaNum = NS['Prelude'].isAlphaNum, isDigit = NS['Prelude'].isDigit, isHexDigit = NS['Prelude'].isHexDigit, isOctDigit = NS['Prelude'].isOctDigit;;var sequence = NS['Text_Parsec'].sequence, run = NS['Text_Parsec'].run, Parser = NS['Text_Parsec'].Parser, ParseState = NS['Text_Parsec'].ParseState, ps = NS['Text_Parsec'].ps, toParser = NS['Text_Parsec'].toParser, unexpected = NS['Text_Parsec'].unexpected, parsecMap = NS['Text_Parsec'].parsecMap, fmap = NS['Text_Parsec'].fmap, liftM = NS['Text_Parsec'].liftM, liftM2 = NS['Text_Parsec'].liftM2, liftM3 = NS['Text_Parsec'].liftM3, liftA = NS['Text_Parsec'].liftA, liftA2 = NS['Text_Parsec'].liftA2, liftA3 = NS['Text_Parsec'].liftA3, ap = NS['Text_Parsec'].ap, parserBind = NS['Text_Parsec'].parserBind, parserReturn = NS['Text_Parsec'].parserReturn, return_ = NS['Text_Parsec'].return_, pure = NS['Text_Parsec'].pure, parserFail = NS['Text_Parsec'].parserFail, fail = NS['Text_Parsec'].fail, parserZero = NS['Text_Parsec'].parserZero, mzero = NS['Text_Parsec'].mzero, empty = NS['Text_Parsec'].empty, parserPlus = NS['Text_Parsec'].parserPlus, parserPlusN = NS['Text_Parsec'].parserPlusN, mplus = NS['Text_Parsec'].mplus, do_ = NS['Text_Parsec'].do_, do$ = NS['Text_Parsec'].do$, do2 = NS['Text_Parsec'].do2, bind = NS['Text_Parsec'].bind, ret = NS['Text_Parsec'].ret, withBound = NS['Text_Parsec'].withBound, returnCall = NS['Text_Parsec'].returnCall, getPosition = NS['Text_Parsec'].getPosition, setPosition = NS['Text_Parsec'].setPosition, getParserState = NS['Text_Parsec'].getParserState, setParserState = NS['Text_Parsec'].setParserState, tokens = NS['Text_Parsec'].tokens, many = NS['Text_Parsec'].many, many1 = NS['Text_Parsec'].many1, string = NS['Text_Parsec'].string, char_ = NS['Text_Parsec'].char_, satisfy = NS['Text_Parsec'].satisfy, label = NS['Text_Parsec'].label, try_ = NS['Text_Parsec'].try_, skipMany = NS['Text_Parsec'].skipMany, match = NS['Text_Parsec'].match, withScope = NS['Text_Parsec'].withScope, ex = NS['Text_Parsec'].ex, oneOf = NS['Text_Parsec'].oneOf, noneOf = NS['Text_Parsec'].noneOf, space = NS['Text_Parsec'].space, spaces = NS['Text_Parsec'].spaces, newline = NS['Text_Parsec'].newline, tab = NS['Text_Parsec'].tab, upper = NS['Text_Parsec'].upper, lower = NS['Text_Parsec'].lower, alphaNum = NS['Text_Parsec'].alphaNum, letter = NS['Text_Parsec'].letter, digit = NS['Text_Parsec'].digit, hexDigit = NS['Text_Parsec'].hexDigit, octDigit = NS['Text_Parsec'].octDigit, anyChar = NS['Text_Parsec'].anyChar, choice = NS['Text_Parsec'].choice, count = NS['Text_Parsec'].count, between = NS['Text_Parsec'].between, option = NS['Text_Parsec'].option, optionMaybe = NS['Text_Parsec'].optionMaybe, optional = NS['Text_Parsec'].optional, skipMany1 = NS['Text_Parsec'].skipMany1, sepBy = NS['Text_Parsec'].sepBy, sepBy1 = NS['Text_Parsec'].sepBy1, endBy = NS['Text_Parsec'].endBy, endBy1 = NS['Text_Parsec'].endBy1, sepEndBy = NS['Text_Parsec'].sepEndBy, sepEndBy1 = NS['Text_Parsec'].sepEndBy1, chainl = NS['Text_Parsec'].chainl, chainl1 = NS['Text_Parsec'].chainl1, chainr = NS['Text_Parsec'].chainr, chainr1 = NS['Text_Parsec'].chainr1, eof = NS['Text_Parsec'].eof, notFollowedBy = NS['Text_Parsec'].notFollowedBy, manyTill = NS['Text_Parsec'].manyTill, lookAhead = NS['Text_Parsec'].lookAhead, anyToken = NS['Text_Parsec'].anyToken, GenLanguageDef = NS['Text_Parsec'].GenLanguageDef, GenTokenParser = NS['Text_Parsec'].GenTokenParser, makeTokenParser = NS['Text_Parsec'].makeTokenParser, emptyDef = NS['Text_Parsec'].emptyDef, haskellStyle = NS['Text_Parsec'].haskellStyle, javaStyle = NS['Text_Parsec'].javaStyle, haskellDef = NS['Text_Parsec'].haskellDef, mondrianDef = NS['Text_Parsec'].mondrianDef, getHaskell = NS['Text_Parsec'].getHaskell, getMondrian = NS['Text_Parsec'].getMondrian, Assoc = NS['Text_Parsec'].Assoc, Operator = NS['Text_Parsec'].Operator, buildExpressionParser = NS['Text_Parsec'].buildExpressionParser;;var curry = NS['Haskell'].curry, const_ = NS['Haskell'].const_, isArray = NS['Haskell'].isArray, isDefined = NS['Haskell'].isDefined, slice = NS['Haskell'].slice, map = NS['Haskell'].map, imap = NS['Haskell'].imap, filter = NS['Haskell'].filter, ifilter = NS['Haskell'].ifilter, indexOf = NS['Haskell'].indexOf, lastIndexOf = NS['Haskell'].lastIndexOf, isort = NS['Haskell'].isort, compose = NS['Haskell'].compose, compose1 = NS['Haskell'].compose1, call = NS['Haskell'].call, id = NS['Haskell'].id, flip = NS['Haskell'].flip, cons = NS['Haskell'].cons, consJoin = NS['Haskell'].consJoin, negate = NS['Haskell'].negate, null_ = NS['Haskell'].null_, elem = NS['Haskell'].elem, digitToInt = NS['Haskell'].digitToInt, range = NS['Haskell'].range, extend = NS['Haskell'].extend, namespace = NS['Haskell'].namespace, toInteger = NS['Haskell'].toInteger, fromInteger = NS['Haskell'].fromInteger, fromIntegral = NS['Haskell'].fromIntegral, readHex = NS['Haskell'].readHex, readOct = NS['Haskell'].readOct, chr = NS['Haskell'].chr, round = NS['Haskell'].round, typeOf = NS['Haskell'].typeOf, strictEq = NS['Haskell'].strictEq, strictNe = NS['Haskell'].strictNe, lt = NS['Haskell'].lt, le = NS['Haskell'].le, gt = NS['Haskell'].gt, ge = NS['Haskell'].ge, not = NS['Haskell'].not, evalThunks = NS['Haskell'].evalThunks, data = NS['Haskell'].data, ADT = NS['Haskell'].ADT, record = NS['Haskell'].record, accessor = NS['Haskell'].accessor, accessors = NS['Haskell'].accessors, typeclass = NS['Haskell'].typeclass, VARARG = NS['Haskell'].VARARG, instance = NS['Haskell'].instance, getInstance = NS['Haskell'].getInstance, asTypeOf = NS['Haskell'].asTypeOf, operators = NS['Haskell'].operators, infix = NS['Haskell'].infix, infixl = NS['Haskell'].infixl, infixr = NS['Haskell'].infixr, op = NS['Haskell'].op, no = NS['Haskell'].no, resolve = NS['Haskell'].resolve, recurse = NS['Haskell'].recurse, Recurse = NS['Haskell'].Recurse, exl = NS['Haskell'].exl, exs = NS['Haskell'].exs, createDo = NS['Haskell'].createDo, Scope = NS['Haskell'].Scope;
/// <reference path="../../../../jshaskell/src/Haskell/DataType.js" local />
/// <reference path="../../../../base/src/Prelude.js" local />


//-- |JavaScript's syntax.
//module BrownPLT.JavaScript.Syntax(Expression(..),CaseClause(..),Statement(..),
//         InfixOp(..),CatchClause(..),VarDecl(..),JavaScript(..),
//         AssignOp(..),Id(..),PrefixOp(..),Prop(..),
//         ForInit(..),ForInInit(..),unId
//  , UnaryAssignOp (..)
//  , LValue (..)
//  ) where
//
//import Text.ParserCombinators.Parsec(SourcePos) -- used by data JavaScript
//import Data.Generics(Data,Typeable)
//
//data JavaScript a
//  -- |A script in <script> ... </script> tags.  This may seem a little silly,
//  -- but the Flapjax analogue has an inline variant and attribute-inline 
//  -- variant.
//  = Script a [Statement a] 
//  deriving (Show,Data,Typeable,Eq,Ord)

function Statement(){}

function JavaScript(){}
data(JavaScript, [["Script", "a", Array]]);

//data Id a = Id a String deriving (Show,Eq,Ord,Data,Typeable)

function Id(){}
data(Id, [["Id", "a", String]]);


//unId :: Id a -> String
//unId (Id _ s) = s

function unId(idVal){
    if(!(idVal instanceof Id))
        throw "Type error: expecting type of 'Id' instead of " + idVal.constructor;
    return idVal.Id ? idVal[1] : null;
}


//-- http://developer.mozilla.org/en/docs/
//--   Core_JavaScript_1.5_Reference:Operators:Operator_Precedence
//data InfixOp = OpLT | OpLEq | OpGT | OpGEq  | OpIn  | OpInstanceof | OpEq | OpNEq
//             | OpStrictEq | OpStrictNEq | OpLAnd | OpLOr 
//             | OpMul | OpDiv | OpMod  | OpSub | OpLShift | OpSpRShift
//             | OpZfRShift | OpBAnd | OpBXor | OpBOr | OpAdd
//    deriving (Show,Data,Typeable,Eq,Ord,Enum)

function InfixOp(){}
data(InfixOp, [
    "OpLT", "OpLEq", "OpGT", "OpGEq", "OpIn", "OpInstanceof", "OpEq", "OpNEq",
    "OpStrictEq", "OpStrictNEq", "OpLAnd", "OpLOr",
    "OpMul", "OpDiv", "OpMod", "OpSub", "OpLShift", "OpSpRShift",
    "OpZfRShift", "OpBAnd", "OpBXor", "OpBOr", "OpAdd"
]);


//data AssignOp = OpAssign | OpAssignAdd | OpAssignSub | OpAssignMul | OpAssignDiv
//  | OpAssignMod | OpAssignLShift | OpAssignSpRShift | OpAssignZfRShift
//  | OpAssignBAnd | OpAssignBXor | OpAssignBOr
//  deriving (Show,Data,Typeable,Eq,Ord)

function AssignOp(){}
data(AssignOp, [
    "OpAssign", "OpAssignAdd", "OpAssignSub", "OpAssignMul", "OpAssignDiv",
    "OpAssignMod", "OpAssignLShift", "OpAssignSpRShift", "OpAssignZfRShift",
    "OpAssignBAnd", "OpAssignBXor", "OpAssignBOr"
]);


//data UnaryAssignOp
//  = PrefixInc | PrefixDec | PostfixInc | PostfixDec
//  deriving (Show, Data, Typeable, Eq, Ord)

function UnaryAssignOp(){}
data(UnaryAssignOp, [
    "PrefixInc", "PrefixDec", "PostfixInc", "PostfixDec"
]);

//data PrefixOp = PrefixLNot | PrefixBNot | PrefixPlus
//  | PrefixMinus | PrefixTypeof | PrefixVoid | PrefixDelete
//  deriving (Show,Data,Typeable,Eq,Ord)

function PrefixOp(){}
data(PrefixOp, [
    "PrefixLNot", "PrefixBNot", "PrefixPlus",
    "PrefixMinus", "PrefixTypeof", "PrefixVoid", "PrefixDelete"
]);


//data Prop a 
//  = PropId a (Id a) | PropString a String | PropNum a Integer
//  deriving (Show,Data,Typeable,Eq,Ord)

function Prop(){}
data(Prop, [
    ["PropId", "a", Id] , ["PropString", "a", String] , ["PropNum", "a", Number]
]);


//data LValue a
//  = LVar a String
//  | LDot a (Expression a) String
//  | LBracket a (Expression a) (Expression a)
//  deriving (Show, Eq, Ord, Data, Typeable)

function LValue(){}
function Expression(){}

data(LValue, [
    ["LVar", "a", String],
    ["LDot", "a", Expression, String],
    ["LBracket", "a", Expression, Expression]
]);


//data Expression a
//  = StringLit a String
//  | RegexpLit a String Bool {- global? -} Bool {- case-insensitive? -}
//  | NumLit a Double
//  | IntLit a Int
//  | BoolLit a Bool
//  | NullLit a
//  | ArrayLit a [Expression a]
//  | ObjectLit a [(Prop a, Expression a)]
//  | ThisRef a
//  | VarRef a (Id a)
//  | DotRef a (Expression a) (Id a)
//  | BracketRef a (Expression a) {- container -} (Expression a) {- key -}
//  | NewExpr a (Expression a) {- constructor -} [Expression a]
//  | PrefixExpr a PrefixOp (Expression a)
//  | UnaryAssignExpr a UnaryAssignOp (LValue a)
//  | InfixExpr a InfixOp (Expression a) (Expression a)
//  | CondExpr a (Expression a) (Expression a) (Expression a)
//  | AssignExpr a AssignOp (LValue a) (Expression a)
//  | ParenExpr a (Expression a)
//  | ListExpr a [Expression a]
//  | CallExpr a (Expression a) [Expression a]
//  --funcexprs are optionally named
//  | FuncExpr a (Maybe (Id a)) [(Id a)] (Statement a)
//  deriving (Show,Data,Typeable,Eq,Ord)

data(Expression, [
    ["StringLit", "a", String]
   ,["RegexpLit", "a", String,
                       Boolean, //global?
                       Boolean] // case-insensitive?
   ,["NumLit"   , "a", Number]
   ,["IntLit"   , "a", Number]
   ,["BoolLit"  , "a", Boolean]
   ,["NullLit"  , "a"]
   ,["ArrayLit" , "a", Array]
   ,["ObjectLit", "a", Array]
   ,["ThisRef"  , "a"]
   ,["VarRef"   , "a", Id]
   ,["DotRef"   , "a", Expression, Id]
   ,["BracketRef"   , "a", Expression, // container
                           Expression] // key
   ,["NewExpr"      , "a", Expression, // constructor
                           Array]
   ,["PrefixExpr"   , "a", PrefixOp, Expression]
   ,["UnaryAssignExpr", "a", UnaryAssignOp, LValue]
   ,["InfixExpr"    , "a", InfixOp, Expression, Expression]
   ,["CondExpr"     , "a", Expression, Expression, Expression]
   ,["AssignExpr"   , "a", AssignOp, LValue, Expression]
   ,["ParenExpr", "a", Expression]
   ,["ListExpr" , "a", Array]
   ,["CallExpr" , "a", Expression, Array]
   ,["FuncExpr" , "a", Maybe, Array, Statement] //funcexprs are optionally named
]);


//data CaseClause a 
//  = CaseClause a (Expression a) [Statement a]
//  | CaseDefault a [Statement a]
//  deriving (Show,Data,Typeable,Eq,Ord

function CaseClause(){}
data(CaseClause, [
    ["CaseClause", "a", Expression, Array],
    ["CaseDefault", "a", Array]                  
]);


//data CatchClause a 
//  = CatchClause a (Id a) (Statement a) 
//  deriving (Show,Data,Typeable,Eq,Ord)

function CatchClause(){}
data(CatchClause, [
    ["CatchClause", "a", Id, Statement]
]);


//data VarDecl a 
//  = VarDecl a (Id a) (Maybe (Expression a)) 
//  deriving (Show,Data,Typeable,Eq,Ord)

function VarDecl(){}
data(VarDecl, [
    ["VarDecl", "a", Id, Maybe] 
]);


//data ForInit a
//  = NoInit
//  | VarInit [VarDecl a]
//  | ExprInit (Expression a)
//  deriving (Show,Data,Typeable,Eq,Ord)

function ForInit(){}
data(ForInit, [
    "NoInit",
    ["VarInit", Array],
    ["ExprInit", Expression]
]);


//data ForInInit a
// = ForInVar (Id a)
// | ForInNoVar (Id a)
// deriving (Show,Data,Typeable,Eq,Ord)

function ForInInit(){}
data(ForInInit, [
    ["ForInVar", Id],
    ["ForInNoVar", Id]
]);


//data Statement a
//  = BlockStmt a [Statement a]
//  | EmptyStmt a
//  | ExprStmt a (Expression a)
//  | IfStmt a (Expression a) (Statement a) (Statement a)
//  | IfSingleStmt a (Expression a) (Statement a)
//  | SwitchStmt a (Expression a) [CaseClause a]
//  | WhileStmt a (Expression a) (Statement a)
//  | DoWhileStmt a (Statement a) (Expression a)
//  | BreakStmt a (Maybe (Id a))
//  | ContinueStmt a (Maybe (Id a))
//  | LabelledStmt a (Id a) (Statement a)
//  | ForInStmt a (ForInInit a) (Expression a) (Statement a)
//  | ForStmt a (ForInit a)        
//              (Maybe (Expression a)) -- test
//              (Maybe (Expression a)) -- increment
//              (Statement a)          -- body
//  | TryStmt a (Statement a) {-body-} [CatchClause a] {-catches-}
//      (Maybe (Statement a)) {-finally-}
//  | ThrowStmt a (Expression a)
//  | ReturnStmt a (Maybe (Expression a))
//  | WithStmt a (Expression a) (Statement a)
//  | VarDeclStmt a [VarDecl a]
//  | FunctionStmt a (Id a) {-name-} [(Id a)] {-args-} (Statement a) {-body-}
//  deriving (Show,Data,Typeable,Eq,Ord)

data(Statement, [
     ["BlockStmt"   , "a", Array]
    ,["EmptyStmt"   , "a"]
    ,["ExprStmt"    , "a", Expression]
    ,["IfStmt"      , "a", Expression, Statement, Statement]
    ,["IfSingleStmt", "a", Expression, Statement]
    ,["SwitchStmt"  , "a", Expression, Array]
    ,["WhileStmt"   , "a", Expression, Statement]
    ,["DoWhileStmt" , "a", Statement, Expression]
    ,["BreakStmt"   , "a", Maybe]
    ,["ContinueStmt", "a", Maybe]
    ,["LabelledStmt", "a", Id, Statement]
    ,["ForInStmt"   , "a", ForInInit, Expression, Statement]
    ,["ForStmt"     , "a", ForInit, 
                           Maybe,     //test
                           Maybe,     //increment
                           Statement] //body
    ,["TryStmt"     , "a", Statement, //body
                           Array,     //catches
                           Maybe]     //finally
    ,["ThrowStmt"   , "a", Expression]
    ,["ReturnStmt"  , "a", Maybe]
    ,["WithStmt"    , "a", Expression, Statement]
    ,["VarDeclStmt" , "a", Array]
    ,["FunctionStmt", "a", Id,        //name
                           Array,     //args
                           Statement] //body
]);


namespace("BrownPLT_JavaScript_Syntax", {
     Expression     : Expression
    ,CaseClause     : CaseClause
    ,Statement      : Statement
    ,InfixOp        : InfixOp
    ,CatchClause    : CatchClause
    ,VarDecl        : VarDecl
    ,JavaScript     : JavaScript
    ,AssignOp       : AssignOp
    ,Id             : Id
    ,PrefixOp       : PrefixOp
    ,Prop           : Prop
    ,ForInit        : ForInit
    ,ForInInit      : ForInInit
    ,unId           : unId
    ,UnaryAssignOp  : UnaryAssignOp
    ,LValue         : LValue
})/// <reference path="../../../../jshaskell/src/Haskell/DataType.js" local />
/// <reference path="../../../../jsparsec/src/Text/Parsec.js" local />


//{- This isn't a lexer in the sense that it provides a JavaScript token-stream.
// - This module provides character-parsers for various JavaScript tokens.
// -}
//module BrownPLT.JavaScript.Lexer(lexeme,identifier,reserved,operator,reservedOp,charLiteral,
//                        stringLiteral,natural,integer,float,naturalOrFloat,
//                        decimal,hexadecimal,octal,symbol,whiteSpace,parens,
//                        braces,brackets,squares,semi,comma,colon,dot,
//                        identifierStart) where
//
//import Prelude hiding (lex)
//import Text.ParserCombinators.Parsec
//import qualified Text.ParserCombinators.Parsec.Token as T

//identifierStart = (letter <|> oneOf "$_")

var identifierStart = parserPlus(letter, oneOf("$_"))

//javascriptDef =
//  T.LanguageDef "/*"
//                "*/"
//                "//"
//                False -- no nested comments
//                identifierStart
//                (alphaNum <|> oneOf "$_") -- identifier rest
//                (oneOf "{}<>()~.,?:|&^=!+-*/%!") -- operator start
//                (oneOf "=<>|&+") -- operator rest
//                ["break", "case", "catch", "const", "continue", "debugger", 
//                 "default", "delete", "do", "else", "enum", "false", "finally",
//                 "for", "function", "if", "instanceof", "in", "let", "new", 
//                 "null", "return", "switch", "this", "throw", "true", "try", 
//                 "typeof", "var", "void", "while", "with"]
//                ["|=", "^=", "&=", "<<=", ">>=", ">>>=", "+=", "-=", "*=", "/=", 
//                 "%=", "=", ";", ",", "?", ":", "||", "&&", "|", "^", "&", 
//                 "===", "==", "=", "!==", "!=", "<<", "<=", "<", ">>>", ">>", 
//                 ">=", ">", "++", "--", "+", "-", "*", "/", "%", "!", "~", ".", 
//                 "[", "]", "{", "}", "(", ")","</","instanceof"]
//                 True -- case-sensitive


var javascriptDef = GenLanguageDef.LanguageDef(record,
               { commentStart    : "/*"
               , commentEnd      : "*/"
               , commentLine     : "//"
               , nestedComments  : false
               , identStart      : identifierStart
               , identLetter     : parserPlus(alphaNum, oneOf("$_"))
               , opStart         : oneOf("{}<>()~.,?:|&^=!+-*/%!")
               , opLetter        : oneOf("=<>|&+")
               , reservedOpNames : [
                 "|=", "^=", "&=", "<<=", ">>=", ">>>=", "+=", "-=", "*=", "/=", 
                 "%=", "=", ";", ",", "?", ":", "||", "&&", "|", "^", "&", 
                 "===", "==", "=", "!==", "!=", "<<", "<=", "<", ">>>", ">>", 
                 ">=", ">", "++", "--", "+", "-", "*", "/", "%", "!", "~", ".", 
                 "[", "]", "{", "}", "(", ")"
                 ,"instanceof", "in" //TODO: are these needed here?
                 ]
               , reservedNames   : [
                 "break", "case", "catch", "const", "continue", "debugger", 
                 "default", "delete", "do", "else", "enum", "false", "finally",
                 "for", "function", "if", "instanceof", "in", "let", "new", 
                 "null", "return", "switch", "this", "throw", "true", "try", 
                 "typeof", "var", "void", "while", "with"
                 ]
               , caseSensitive   : true
               });

          
//lex = T.makeTokenParser javascriptDef

var lex = makeTokenParser(javascriptDef);

//-- everything but commaSep and semiSep
//identifier = T.identifier  lex
//reserved = T.reserved  lex
//operator = T.operator  lex
//reservedOp = T.reservedOp lex 
//charLiteral = T.charLiteral lex   
//stringLiteral = T.stringLiteral lex   
//natural = T.natural lex   
//integer = T.integer lex   
//float = T.float lex   
//naturalOrFloat = T.naturalOrFloat lex 
//decimal = T.decimal lex   
//hexadecimal = T.hexadecimal lex   
//octal = T.octal lex   
//symbol = T.symbol lex 
//whiteSpace = T.whiteSpace lex 
//parens = T.parens  lex
//braces = T.braces  lex
//squares = T.squares lex   
//semi = T.semi  lex
//comma = T.comma    lex
//colon = T.colon lex   
//dot = T.dot lex
//brackets = T.brackets lex
//lexeme = T.lexeme lex

namespace("BrownPLT_JavaScript_Lexer", {
    lex: lex
})/// <reference path="../../../../jshaskell/src/Haskell.js" local />
/// <reference path="../../../../base/src/Prelude.js" local />
/// <reference path="../../../../jsparsec/src/Text/Parsec.js" local />
/// <reference path="Lexer.js" />
/// <reference path="Syntax.js" />


//module BrownPLT.JavaScript.Parser
//  (parseScript
//  , parseExpression
//  , parseString
//  , parseScriptFromString
//  , emptyParsedJavaScript
//  , ParsedStatement
//  , ParsedExpression
//  , parseJavaScriptFromFile
//  , parseSimpleExpr'
//  , parseBlockStmt
//  , parseStatement
//  , StatementParser
//  , ExpressionParser
//  , assignExpr
//  ) where
//
//import BrownPLT.JavaScript.Lexer hiding (identifier)
//import qualified BrownPLT.JavaScript.Lexer as Lexer
//import BrownPLT.JavaScript.Syntax
//import Text.ParserCombinators.Parsec
//import Text.ParserCombinators.Parsec.Expr
//import Control.Monad(liftM,liftM2)
//import Control.Monad.Trans (MonadIO,liftIO)
//import Numeric(readDec,readOct,readHex)
//import Data.Char(chr)
//import Data.Char

/*

Changes to the original code:
 * added octal number parser (octLit, parseNumLit)
 * parseScript: parseStatement `sepBy` whitespace -> many(parseStatement)
   statements consume all trailing whitespace
 * Lexer: reservedOpNames and reservedNames was switched,
   removed "</" and added "in" (since "instanceof" was already there)
 * parseIfStmt: removed optional semicolon
 * parseContinueStmt: added optional semicolon
 * added automatic semicolon insertion for throw and return
 * unaryAssignExpr: added automatic semicolon insertion
   before prefix ++ or -- preceeded by a newline:
        //var a = 10, b = 3;
        a
        ++b
   actual: [VarRef "a", PrefixInc (VarRef "b")]
   original: [PostfixInc (VarRef "a"), VarRef "b"]
 * ASI with multiline comments is not consistent
 * optinal semicolons have been replaced with autoEndStmt:
   instead of optional(lex.semi) it checks the consumed(!) whitespace for a
   newline, if there's none then for a semi, closing brace, or eof
   
TODO:
 * throw should be followed by an expression (on the same line), i.e.
   "throw;" or "throw \n error;" is not allowed, see `onSameLine1`
 * future reserved names (?)
 * function declaration in blocks (implementation dependent)
    if(true)
        function f(){} 
   function expressions without name as statements
    function(){}    //Syntax error
    (function(){})  //OK
   ExpressionStatements cannot start with "function" or "{" (spec 12.4)
   FunctionDeclaration as Statements (spec 12.0)
   add SourceElement: Statement or  FunctionDeclaration
   add FunctionBody: SourceElements
   
*/

function dropWhile(p, a){
    var str = !!a.charAt;
    for(var i = 0, l = a.length; i < l; ++i)
        if(!p(str ? a.charAt(i) : a[i]))
            break;
    return slice(a, i);
}

var parseListExpr   = withScope(function(){ return parseListExpr   });
var parseStatement  = withScope(function(){ return parseStatement  });
var parseParenExpr  = withScope(function(){ return parseParenExpr  });
var parseNewExpr    = withScope(function(){ return parseNewExpr    });
var parseExpression = withScope(function(){ return parseExpression });
var assignExpr = parseExpression;

//used with break, continue, and return
function onSameLine(pos0, pos1, parser){
    return function(scope, state, k){
        return ((state.sourceLine(scope[pos0]) == state.sourceLine(scope[pos1])) ? 
                parserPlus(liftM(Maybe.Just, parser), return_(Maybe.Nothing)) :
                return_(Maybe.Nothing))
            (scope, state, k);
    }
}

//this is used with parseThrowStmt,
//but the error message appears in the beginning of the enclosing statement
//if followed by a semicolon, else if there's a new line it's skipped entirely!
function onSameLine1(pos0, pos1, parser){
    return function(scope, state, k){
        return ((state.sourceLine(scope[pos0]) == state.sourceLine(scope[pos1])) ? 
                parser :
                unexpected("end of line"))
            (scope, state, k);
    }
}


function autoEndStmt(scope, state, k){

    var ws = state.input.substring(0, state.index - state.dropped).match(/\s*$/),
        ended = ws && /\n|\r/.test(ws[0]),
        semi;
        
    if(!ended){
        var nextChar = state.at(0);
        if(nextChar == "" || nextChar == "}"){
            ended = true;
        }else if(nextChar == ";"){
            ended = true;
            semi = true;
        }
    }
    
    return (ended ?
                (semi ? lex.semi : return_(null)) :
                fail("end of statement"))
        (scope, state, k);
}



//-- We parameterize the parse tree over source-locations.
//type ParsedStatement = Statement SourcePos
//type ParsedExpression = Expression SourcePos


//-- These parsers can store some arbitrary state
//type StatementParser state = CharParser state ParsedStatement
//type ExpressionParser state = CharParser state ParsedExpression

//identifier =
//  liftM2 Id getPosition Lexer.identifier
var identifier = liftM2(Id.Id, getPosition, lex.identifier);


//--{{{ Statements
//
//-- Keep in mind that Token.reserved parsers (exported from the lexer) do not
//-- consume any input on failure.  Note that all statements (expect for labelled
//-- and expression statements) begin with a reserved-word.  If we fail to parse
//-- this reserved-word, no input is consumed.  Hence, we can have the massive or
//-- block that is parseExpression.  Note that if the reserved-word is parsed, it 
//-- must be whatever statement the reserved-word indicates.  If we fail after the
//-- reserved-word, we truly have a syntax error.  Since input has been consumed,
//-- <|> will not try its alternate in parseExpression, and we will fail.

//parseIfStmt:: StatementParser st  
//parseIfStmt = do
//  pos <- getPosition
//  reserved "if"
//  test <- parseParenExpr <?> "parenthesized test-expression in if statement"
//  consequent <- parseStatement <?> "true-branch of if statement"
//  optional semi -- TODO: in spec?
//  ((do reserved "else"
//       alternate <- parseStatement
//       return (IfStmt pos test consequent alternate))
//    <|> return (IfSingleStmt pos test consequent))

var parseIfStmt = do$
  ("pos"  ,"<-", getPosition)
  (lex.reserved, "if")
  ("test" ,"<-", parseParenExpr ,"<?>", "parenthesized test-expression in if statement")
  ("consequent" ,"<-", parseStatement ,"<?>", "true-branch of if statement")
  //the empty statement matches a single semi, but one more would be 
  //a syntax error if it's followed by `else`
  //(optional, lex.semi) //-- TODO: in spec?
  (  do$(lex.reserved, "else")
        ("alternate" ,"<-", parseStatement)
        (ret, function(scope){
            return Statement.IfStmt(scope.scope.pos, scope.scope.test,
                                    scope.scope.consequent, scope.alternate);
        })
    ,"<|>", returnCall(Statement.IfSingleStmt, "pos", "test", "consequent")
  )


//parseSwitchStmt:: StatementParser st
//parseSwitchStmt =
//  let parseDefault = do
//        pos <- getPosition
//        reserved "default"
//        colon
//        statements <- many parseStatement
//        return (CaseDefault pos statements)
//      parseCase = do
//        pos <- getPosition
//        reserved "case"
//        condition <- parseListExpr
//        colon
//        actions <- many parseStatement
//        return (CaseClause pos condition actions)
//  in do pos <- getPosition
//        reserved "switch"
//        test <- parseParenExpr
//        clauses <- braces $ many $ parseDefault <|> parseCase
//        return (SwitchStmt pos test clauses)
var _parseDefault = do$
  ("pos" ,"<-", getPosition)
  (lex.reserved, "default")
  (lex.colon)
  ("statements" ,"<-", many, parseStatement)
  (returnCall, CaseClause.CaseDefault, "pos",  "statements")
  
var _parseCase = do$
  ("pos" ,"<-", getPosition)
  (lex.reserved, "case")
  ("condition" ,"<-", parseListExpr)
  (lex.colon)
  ("actions"   ,"<-", many, parseStatement)
  (returnCall, CaseClause.CaseClause, "pos", "condition", "actions")
  
var parseSwitchStmt = do$
  ("pos"     ,"<-", getPosition)
  (lex.reserved, "switch")
  ("test"    ,"<-", parseParenExpr)
  ("clauses" ,"<-", lex.braces ,"$", many ,"$", _parseDefault ,"<|>", _parseCase)
  (returnCall, Statement.SwitchStmt, "pos", "test", "clauses")

//parseWhileStmt:: StatementParser st
//parseWhileStmt = do
//  pos <- getPosition
//  reserved "while"
//  test <- parseParenExpr <?> "parenthesized test-expression in while loop"
//  body <- parseStatement
//  return (WhileStmt pos test body)
var parseWhileStmt = do$
  ("pos"  ,"<-", getPosition)
  (lex.reserved, "while")
  ("test" ,"<-", parseParenExpr ,"<?>", "parenthesized test-expression in while loop")
  ("body" ,"<-", parseStatement)
  (returnCall, Statement.WhileStmt, "pos", "test", "body")


//parseBlockStmt:: StatementParser st
//parseBlockStmt = do
//  pos <- getPosition
//  statements <- braces (many parseStatement)
//  return (BlockStmt pos statements)
var parseBlockStmt = do$
  ("pos" ,"<-", getPosition)
  ("statements" ,"<-", lex.braces, [many, parseStatement])
  (returnCall, Statement.BlockStmt, "pos", "statements")


//parseDoWhileStmt:: StatementParser st
//parseDoWhileStmt = do
//  pos <- getPosition
//  reserved "do"
//  body <- parseBlockStmt
//  reserved "while" <?> "while at the end of a do block"
//  test <- parseParenExpr <?> "parenthesized test-expression in do loop"
//  optional semi
//  return (DoWhileStmt pos body test)
var parseDoWhileStmt = do$
  ("pos"  ,"<-", getPosition)
  (lex.reserved, "do")
  ("body" ,"<-", parseBlockStmt)
  (lex.reserved, "while" ,"<?>", "while at the end of a do block")
  ("test" ,"<-", parseParenExpr ,"<?>", "parenthesized test-expression in do loop")
  (autoEndStmt)
  (returnCall, Statement.DoWhileStmt, "pos", "body", "test")


//parseContinueStmt:: StatementParser st
//parseContinueStmt = do
//  pos <- getPosition
//  reserved "continue"
//  pos' <- getPosition
//  -- Ensure that the identifier is on the same line as 'continue.'
//  id <- (if (sourceLine pos == sourceLine pos')
//           then (liftM Just identifier) <|> (return Nothing)
//           else return Nothing)
//  return (ContinueStmt pos id)
var parseContinueStmt = do$
  ("pos"  ,"<-", getPosition)
  (lex.reserved, "continue")
  ("pos_" ,"<-", getPosition)
  // Ensure that the identifier is on the same line as 'continue.'
  ("id"   ,"<-", onSameLine, "pos", "pos_", identifier)
  (autoEndStmt)
  (returnCall, Statement.ContinueStmt, "pos", "id")


//parseBreakStmt:: StatementParser st
//parseBreakStmt = do
//  pos <- getPosition
//  reserved "break"
//  pos' <- getPosition
//  -- Ensure that the identifier is on the same line as 'break.'
//  id <- (if (sourceLine pos == sourceLine pos')
//           then (liftM Just identifier) <|> (return Nothing)
//           else return Nothing)
//  optional semi           
//  return (BreakStmt pos id)
var parseBreakStmt = do$
  ("pos"  ,"<-", getPosition)
  (lex.reserved, "break")
  ("pos_" ,"<-", getPosition)
  // Ensure that the identifier is on the same line as 'break.'
  ("id"   ,"<-", onSameLine, "pos", "pos_", identifier)
  (autoEndStmt)
  (returnCall, Statement.BreakStmt, "pos", "id")


//parseEmptyStmt:: StatementParser st 
//parseEmptyStmt = do
//  pos <- getPosition
//  semi
//  return (EmptyStmt pos)
var parseEmptyStmt = do$
  ("pos" ,"<-", getPosition)
  (lex.semi)
  (returnCall, Statement.EmptyStmt, "pos")


//parseLabelledStmt:: StatementParser st
//parseLabelledStmt = do
//  pos <- getPosition
//  -- Lookahead for the colon.  If we don't see it, we are parsing an identifier
//  -- for an expression statement.
//  label <- try (do label <- identifier
//                   colon
//                   return label)
//  statement <- parseStatement
//  return (LabelledStmt pos label statement)
var parseLabelledStmt = do$
  ("pos"   ,"<-", getPosition)
  // Lookahead for the colon.  If we don't see it, we are parsing an identifier
  // for an expression statement.
  ("label" ,"<-", try_, do$("label" ,"<-", identifier)
                           (lex.colon)
                           (ret, "label")
  )
  ("statement" ,"<-", parseStatement)
  (returnCall, Statement.LabelledStmt, "pos", "label", "statement")


//parseExpressionStmt:: StatementParser st
//parseExpressionStmt = do
//  pos <- getPosition
//  expr <- parseListExpr -- TODO: spec 12.4?
//  optional semi
//  return (ExprStmt pos expr)
var parseExpressionStmt = do$
  ("pos"  ,"<-", getPosition)
  ("expr" ,"<-", parseListExpr) // TODO: spec 12.4?
  (autoEndStmt)
  (returnCall, Statement.ExprStmt, "pos", "expr")


//parseForInStmt:: StatementParser st
//parseForInStmt =
//  let parseInit = (reserved "var" >> liftM ForInVar identifier)
//                  <|> (liftM ForInNoVar identifier)
//    in do pos <- getPosition
//          -- Lookahead, so that we don't clash with parseForStmt
//          (init,expr) <- try (do reserved "for"
//                                 parens (do init <- parseInit
//                                            reserved "in"
//                                            expr <- parseExpression
//                                            return (init,expr)))
//          body <- parseStatement
//          return (ForInStmt pos init expr body) 
var _parseInit = ex([lex.reserved, "var" ,">>", liftM, ForInInit.ForInVar, identifier]
    ,"<|>", [liftM, ForInInit.ForInNoVar, identifier]);

var parseForInStmt = do$
  ("pos" ,"<-", getPosition)
   // Lookahead, so that we don't clash with parseForStmt
  ("init_expr" ,"<-", try_, do$(lex.reserved, "for")
                               (lex.parens, do$("init" ,"<-", _parseInit)
                                          (lex.reserved, "in")
                                          ("expr" ,"<-", parseExpression)
                                          (ret, function(scope){ return [scope.init, scope.expr] })
                                )
  )
  ("body" ,"<-", parseStatement)
  (ret, function(scope){
    return Statement.ForInStmt(scope.pos, scope.init_expr[0], scope.init_expr[1], scope.body);
  })

//parseVarDecl = do
//  pos <- getPosition
//  id <- identifier
//  init <- (reservedOp "=" >> liftM Just parseExpression) <|> (return Nothing)
//  return (VarDecl pos id init)
var parseVarDecl = do$
  ("pos"  ,"<-", getPosition)
  ("id"   ,"<-", identifier)
  ("init" ,"<-", [lex.reservedOp("=") ,">>", liftM, Maybe.Just, parseExpression]
                  ,"<|>", return_(Maybe.Nothing))
  (returnCall, VarDecl.VarDecl, "pos", "id", "init")


//parseVarDeclStmt:: StatementParser st
//parseVarDeclStmt = do 
//  pos <- getPosition
//  reserved "var"
//  decls <- parseVarDecl `sepBy` comma
//  optional semi
//  return (VarDeclStmt pos decls)
var parseVarDeclStmt = do$ 
  ("pos" ,"<-", getPosition)
  (lex.reserved, "var")
  ("decls" ,"<-", parseVarDecl, op(sepBy), lex.comma)
  (autoEndStmt)
  (returnCall, Statement.VarDeclStmt, "pos",  "decls")

//parseForStmt:: StatementParser st
//parseForStmt =
//  let parseInit =
//        (reserved "var" >> liftM VarInit (parseVarDecl `sepBy` comma)) <|>
//        (liftM ExprInit parseListExpr) <|>
//        (return NoInit)
//    in do pos <- getPosition
//          reserved "for"
//          reservedOp "("
//          init <- parseInit
//          semi
//          test <- (liftM Just parseExpression) <|> (return Nothing)
//          semi
//          iter <- (liftM Just parseListExpr) <|> (return Nothing)
//          reservedOp ")" <?> "closing paren"
//          stmt <- parseStatement
//          return (ForStmt pos init test iter stmt)
var _parseInit2 = ex(
    [lex.reserved, "var" ,">>", liftM, ForInit.VarInit, [parseVarDecl ,op(sepBy), lex.comma]]
    ,"<|>", [liftM, ForInit.ExprInit, parseListExpr]
    ,"<|>", return_(ForInit.NoInit)
  );
  
var parseForStmt = do$
  ("pos"  ,"<-", getPosition)
  (lex.reserved, "for")
  (lex.reservedOp("("))
  ("init" ,"<-", _parseInit2)
  (lex.semi)
  ("test" ,"<-", [liftM, Maybe.Just, parseExpression] ,"<|>", return_(Maybe.Nothing))
  (lex.semi)
  ("iter" ,"<-", [liftM, Maybe.Just, parseListExpr] ,"<|>", return_(Maybe.Nothing))
  (lex.reservedOp(")") ,"<?>", "closing paren")
  ("stmt" ,"<-", parseStatement)
  (returnCall, Statement.ForStmt, "pos", "init", "test", "iter", "stmt")

//parseTryStmt:: StatementParser st
//parseTryStmt =
//  let parseCatchClause = do
//        pos <- getPosition
//        reserved "catch"
//        id <- parens identifier
//        stmt <- parseStatement
//        return (CatchClause pos id stmt)
//    in do reserved "try"
//          pos <- getPosition
//          guarded <- parseStatement
//          catches <- many parseCatchClause
//          finally <- (reserved "finally" >> liftM Just parseStatement) 
//                      <|> (return Nothing)
//          return (TryStmt pos guarded catches finally)
var _parseCatchClause = do$
    ("pos"  ,"<-", getPosition)
    (lex.reserved, "catch")
    ("id"   ,"<-", lex.parens, identifier)
    ("stmt" ,"<-", parseStatement)
    (returnCall, CatchClause.CatchClause, "pos", "id", "stmt")
            
var parseTryStmt = do$
  (lex.reserved, "try")
  ("pos"     ,"<-", getPosition)
  ("guarded" ,"<-", parseStatement)
  ("catches" ,"<-", many, _parseCatchClause)
  ("finally" ,"<-", [lex.reserved, "finally" ,">>", liftM, Maybe.Just, parseStatement]
               ,"<|>", return_(Maybe.Nothing))
  (returnCall, Statement.TryStmt, "pos", "guarded", "catches", "finally")


//parseThrowStmt:: StatementParser st
//parseThrowStmt = do
//  pos <- getPosition
//  reserved "throw"
//  expr <- parseExpression
//  optional semi
//  return (ThrowStmt pos expr)
var parseThrowStmt = do$
  ("pos"  ,"<-", getPosition)
  (lex.reserved, "throw")
  ("pos_" ,"<-", getPosition)
  ("expr" ,"<-", onSameLine1, "pos", "pos_", parseExpression)
  (autoEndStmt)
  (returnCall, Statement.ThrowStmt, "pos", "expr")


//parseReturnStmt:: StatementParser st
//parseReturnStmt = do
//  pos <- getPosition
//  reserved "return"
//  expr <- (liftM Just parseListExpr) <|> (return Nothing)
//  optional semi
//  return (ReturnStmt pos expr)
var parseReturnStmt = do$
  ("pos"  ,"<-", getPosition)
  (lex.reserved, "return")
  ("pos_" ,"<-", getPosition)
  ("expr" ,"<-", onSameLine, "pos", "pos_", parseListExpr)
  (autoEndStmt)
  (returnCall, Statement.ReturnStmt, "pos", "expr")


//parseWithStmt:: StatementParser st
//parseWithStmt = do
//  pos <- getPosition
//  reserved "with"
//  context <- parseParenExpr
//  stmt <- parseStatement
//  return (WithStmt pos context stmt)
var parseWithStmt = do$
  ("pos"     ,"<-", getPosition)
  (lex.reserved, "with")
  ("context" ,"<-", parseParenExpr)
  ("stmt"    ,"<-", parseStatement)
  (returnCall, Statement.WithStmt, "pos", "context", "stmt")


//parseFunctionStmt:: StatementParser st
//parseFunctionStmt = do
//  pos <- getPosition
//  name <- try (reserved "function" >> identifier) -- ambiguity with FuncExpr
//  args <- parens (identifier `sepBy` comma)
//  body <- parseBlockStmt <?> "function body in { ... }"
//  return (FunctionStmt pos name args body)
var parseFunctionStmt = do$
  ("pos"  ,"<-", getPosition)
  ("name" ,"<-", try_, [lex.reserved, "function", ">>", identifier]) // ambiguity with FuncExpr
  ("args" ,"<-", lex.parens, [identifier ,op(sepBy), lex.comma])
  ("body" ,"<-", parseBlockStmt ,"<?>", "function body in { ... }")
  (returnCall, Statement.FunctionStmt, "pos", "name", "args", "body")


//parseStatement:: StatementParser st
//parseStatement = parseIfStmt <|> parseSwitchStmt <|> parseWhileStmt 
//  <|> parseDoWhileStmt <|> parseContinueStmt <|> parseBreakStmt 
//  <|> parseBlockStmt <|> parseEmptyStmt <|> parseForInStmt <|> parseForStmt
//  <|> parseTryStmt <|> parseThrowStmt <|> parseReturnStmt <|> parseWithStmt 
//  <|> parseVarDeclStmt  <|> parseFunctionStmt
//  -- labelled, expression and the error message always go last, in this order
//  <|> parseLabelledStmt <|> parseExpressionStmt <?> "statement"
parseStatement = ex(
            parseIfStmt
    ,"<|>", parseSwitchStmt
    ,"<|>", parseWhileStmt
    ,"<|>", parseDoWhileStmt
    ,"<|>", parseContinueStmt
    ,"<|>", parseBreakStmt
    ,"<|>", parseBlockStmt
    ,"<|>", parseEmptyStmt
    ,"<|>", parseForInStmt
    ,"<|>", parseForStmt
    ,"<|>", parseTryStmt
    ,"<|>", parseThrowStmt
    ,"<|>", parseReturnStmt
    ,"<|>", parseWithStmt
    ,"<|>", parseVarDeclStmt
    ,"<|>", parseFunctionStmt 
    // labelled, expression and the error message always go last, in this order
    ,"<|>", parseLabelledStmt 
    ,"<|>", parseExpressionStmt
    ,"<?>", "statement"
);

//--{{{ Expressions

//-- References used to construct this stuff:
//-- + http://developer.mozilla.org/en/docs/
//--     Core_JavaScript_1.5_Reference:Operators:Operator_Precedence
//-- + http://www.mozilla.org/js/language/grammar14.html
//--
//-- Aren't expression tables nice?  Well, we can't quite use them, because of 
//-- JavaScript's ternary (?:) operator.  We have to use two expression tables.
//-- We use one expression table for the assignment operators that bind looser 
//-- than ?: (assignTable).  The terms of assignTable are ternary expressions 
//-- (parseTernaryExpr).  parseTernaryExpr left-factors the left-recursive
//-- production for ?:, and is defined over the second expression table, 
//-- exprTable, which consists of operators that bind tighter than ?:.  The terms
//-- of exprTable are atomic expressions, parenthesized expressions, functions and
//-- array references.

//--{{{ Primary expressions

//parseThisRef:: ExpressionParser st
//parseThisRef = do
//  pos <- getPosition
//  reserved "this"
//  return (ThisRef pos)
var parseThisRef = do$
  ("pos" ,"<-", getPosition)
  (lex.reserved, "this")
  (returnCall, Expression.ThisRef, "pos")
//ex(Expression.ThisRef ,"<$>", [getPosition ,"<*", lex.reserved, "this"])

//parseNullLit:: ExpressionParser st
//parseNullLit = do
//  pos <- getPosition
//  lex.reserved "null"
//  return (NullLit pos)
var parseNullLit = do$
  ("pos" ,"<-", getPosition)
  (lex.reserved, "null")
  (returnCall, Expression.NullLit, "pos")
//ex(Expression.NullLit ,"<$>", [getPosition ,"<*", lex.reserved, "null"])


//parseBoolLit:: ExpressionParser st
//parseBoolLit = do
//    pos <- getPosition
//    let parseTrueLit  = reserved "true"  >> return (BoolLit pos True)
//        parseFalseLit = reserved "false" >> return (BoolLit pos False)
//    parseTrueLit <|> parseFalseLit
var parseBoolLit = do$
  ("pos"   ,"<-", getPosition)
  ("true"  ,"<-", return_, true)
  ("false" ,"<-", return_, false)
  ([lex.reserved, "true"  ,">>", returnCall(Expression.BoolLit, "pos", "true")]
   ,"<|>",
   [lex.reserved, "false" ,">>", returnCall(Expression.BoolLit, "pos", "false")]
  )


//parseVarRef:: ExpressionParser st
//parseVarRef = liftM2 VarRef getPosition identifier
var parseVarRef = liftM2(Expression.VarRef, getPosition, identifier);


//parseArrayLit:: ExpressionParser st
//parseArrayLit = liftM2 ArrayLit getPosition (squares (parseExpression `sepEndBy` comma))
var parseArrayLit = liftM2(Expression.ArrayLit, getPosition, lex.squares(sepEndBy(parseExpression, lex.comma)));


//parseFuncExpr = do
//  pos <- getPosition
//  reserved "function"
//  name <- (identifier >>= return . Just) <|> return Nothing
//  args <- parens (identifier `sepBy` comma)
//  body <- parseBlockStmt
//  return $ FuncExpr pos name args body
var parseFuncExpr = do$
  ("pos"  ,"<-", getPosition)
  (lex.reserved, "function")
  ("name" ,"<-", [identifier, ">>=", return_ ,".", Maybe.Just] ,"<|>", return_, Maybe.Nothing)
  ("args" ,"<-", lex.parens, [identifier ,op(sepBy), lex.comma])
  ("body" ,"<-", parseBlockStmt)
  (returnCall, Expression.FuncExpr, "pos", "name", "args", "body")


//--{{{ parsing strings

//escapeChars =
// [('\'','\''),('\"','\"'),('\\','\\'),('b','\b'),('f','\f'),('n','\n'),
//  ('r','\r'),('t','\t'),('v','\v'),('/','/'),(' ',' '),('0','\0')]
var escapeChars =
 [['\'','\''],['\"','\"'],['\\','\\'],['b','\b'],['f','\f'],['n','\n'],
  ['r','\r'],['t','\t'],['v','\v'],['/','/'],[' ',' '],['0','\0']];


//allEscapes:: String
//allEscapes = map fst escapeChars
var allEscapes = map(fst, escapeChars);


//parseEscapeChar = do
//  c <- oneOf allEscapes
//  let (Just c') = lookup c escapeChars -- will succeed due to line above
//  return c'
var parseEscapeChar = do$
  ("c" ,"<-", oneOf(allEscapes))
  (ret, function(scope){
    return lookup(scope.c, escapeChars)[0]; // will succeed due to line above
  })

//parseAsciiHexChar = do
//  char 'x'
//  d1 <- hexDigit
//  d2 <- hexDigit
//  return ((chr.fst.head.readHex) (d1:d2:""))
var parseAsciiHexChar = do$
  (char_, 'x')
  ("d1" ,"<-", hexDigit)
  ("d2" ,"<-", hexDigit)
  (ret, function(scope){ return chr(readHex("" + scope.d1 + scope.d2)) })


//parseUnicodeHexChar = do
//  char 'u'
//  liftM (chr.fst.head.readHex) 
//        (sequence [hexDigit,hexDigit,hexDigit,hexDigit])
var parseUnicodeHexChar = do$
  (char_, 'u')
  (liftM, [chr ,".", readHex],
          sequence([hexDigit, hexDigit, hexDigit, hexDigit]))


//isWhitespace ch = ch `elem` " \t"
function isWhitespace(ch){ return (ch == " ") || (ch == "\t") }


//-- The endWith argument is either single-quote or double-quote, depending on how
//-- we opened the string.
//parseStringLit' endWith =
//  (char endWith >> return "") <|>
//  (do try (string "\\'")
//      cs <- parseStringLit' endWith
//      return $ "'" ++ cs) <|>
//  (do char '\\'
//      c <- parseEscapeChar <|> parseAsciiHexChar <|> parseUnicodeHexChar <|> 
//           char '\r' <|> char '\n'
//      cs <- parseStringLit' endWith
//      if c == '\r' || c == '\n' 
//        then return (c:(dropWhile isWhitespace cs)) 
//        else return (c:cs)) <|>
//   (liftM2 (:) anyChar (parseStringLit' endWith))

function lazyParseStringLit_(endWith){
    return function(scope, state, k){
        return parseStringLit_(endWith)(scope, state, k);
    }
}


function parseStringLit_(endWith){
  return ex(
    [char_(endWith) ,">>", return_, ""] ,"<|>",
    [do$(try_, [string, "\\'"])
        ("cs" ,"<-", lazyParseStringLit_(endWith))
        (ret, function(scope){ return "'" + scope.cs })
    ] ,"<|>",
    [do$(char_('\\'))
        ("c"  ,"<-", parseEscapeChar ,"<|>", parseAsciiHexChar ,"<|>", parseUnicodeHexChar ,"<|>",
                     char_('\r') ,"<|>", char_('\n'))
        ("cs" ,"<-", lazyParseStringLit_(endWith))
        (ret, function(scope){
                return (scope.c == '\r' || scope.c == '\n' ) ?
                      cons(scope.c, dropWhile(isWhitespace, scope.cs)) :
                      cons(scope.c, scope.cs);
        })
    ],"<|>",
    [liftM2, cons, anyChar, lazyParseStringLit_(endWith)]
  );
}


//parseStringLit:: ExpressionParser st
//parseStringLit = do
//  pos <- getPosition
//  -- parseStringLit' takes as an argument the quote-character that opened the
//  -- string.
//  str <- lexeme $ (char '\'' >>= parseStringLit') <|> (char '\"' >>= parseStringLit')
//  -- CRUCIAL: Parsec.Token parsers expect to find their token on the first
//  -- character, and read whitespaces beyond their tokens.  Without 'lexeme'
//  -- above, expressions like:
//  --   var s = "string"   ;
//  -- do not parse.
//  return $ StringLit pos str
var parseStringLit = do$
  ("pos" ,"<-", getPosition)
  // parseStringLit' takes as an argument the quote-character that opened the)
  // string.)
  ("str" ,"<-", lex.lexeme, "$", [char_('\'') ,">>=", parseStringLit_]
                            ,"<|>", [char_('"') ,">>=", parseStringLit_])
  // CRUCIAL: Parsec.Token parsers expect to find their token on the first
  // character, and read whitespaces beyond their tokens.  Without 'lexeme'
  // above, expressions like:
  //   var s = "string"   ;
  // do not parse.
  (returnCall, Expression.StringLit, "pos", "str")



//parseRegexpLit:: ExpressionParser st
//parseRegexpLit = do
//  let parseFlags = do
//        flags <- many (oneOf "mgi")
//        return $ \f -> f ('g' `elem` flags) ('i' `elem` flags) 
//  let parseEscape = char '\\' >> anyChar
//  let parseChar = noneOf "/"
//  let parseRe = (char '/' >> return "") <|> 
//                (do char '\\'
//                    ch <- anyChar -- TOOD: too lenient
//                    rest <- parseRe
//                    return ('\\':ch:rest)) <|> 
//                (liftM2 (:) anyChar parseRe)
//  pos <- getPosition
//  char '/'
//  pat <- parseRe --many1 parseChar
//  flags <- parseFlags
//  spaces -- crucial for Parsec.Token parsers
//  return $ flags (RegexpLit pos pat)
var parseFlags = do$
  ("flags" ,"<-", many, oneOf("mgi"))
  (ret, function(scope){
    return function(f){
        return f( elem('g', scope.flags), elem('i', scope.flags) );
    }
  })
var parseEscape = ex(char_('\\') ,">>", anyChar).resolve();
var parseChar = noneOf("/");
var _parseRe = function(scope, state, k){ return parseRe(scope, state, k) }
var parseRe = ex([char_('/') ,">>", return_, ""] ,"<|>", 
  do$(char_('\\'))
     ("ch"   ,"<-", anyChar) // TOOD: too lenient
     ("rest" ,"<-", _parseRe)
     (ret, function(scope){ return '\\' + scope.ch + scope.rest }) ,"<|>",
  [liftM2, cons, anyChar, _parseRe]
);

var parseRegexpLit = do$
  ("pos"   ,"<-", getPosition)
  (char_('/'))
  ("pat"   ,"<-", parseRe) //many1 parseChar
  ("flags" ,"<-", parseFlags)
  (spaces) // crucial for Parsec.Token parsers
  (ret, function(scope){
        return scope.flags(function(global, ci){
            return Expression.RegexpLit(scope.pos, scope.pat, global, ci);
        });
  })

//parseObjectLit:: ExpressionParser st
//parseObjectLit =
//  let parseProp = do
//        -- Parses a string, identifier or integer as the property name.  I
//        -- apologize for the abstruse style, but it really does make the code
//        -- much shorter.
//        name <- (liftM (uncurry PropString) 
//                       (liftM (\(StringLit p s) -> (p,s)) parseStringLit))
//                <|> (liftM2 PropId getPosition identifier)
//                <|> (liftM2 PropNum getPosition decimal)
//        colon
//        val <- assignExpr
//        return (name,val)
//    in do pos <- getPosition
//          props <- braces (parseProp `sepEndBy` comma) <?> "object literal"
//          return $ ObjectLit pos props
// Parses a string, identifier or integer as the property name. I apologize
// for the abstruse style, but it really does make the code much shorter. 
var _parseProp = do$
  ("name" ,"<-", [liftM, uncurry(Prop.PropString), 
                       [liftM, function(stringLit){
                            return [stringLit[0], stringLit[1]];
                        }, parseStringLit]]
                ,"<|>", [liftM2, Prop.PropId, getPosition, identifier]
                ,"<|>", [liftM2, Prop.PropNum, getPosition, lex.decimal]
  )
  (lex.colon)
  ("val" ,"<-", assignExpr)
  (ret, function(scope){ return [scope.name, scope.val] })
  
var parseObjectLit = do$
    ("pos"   ,"<-", getPosition)
    ("props" ,"<-", lex.braces, [_parseProp ,op(sepEndBy), lex.comma] ,"<?>", "object literal")
    (returnCall, Expression.ObjectLit, "pos", "props")

//--{{{ Parsing numbers.  From pg. 17-18 of ECMA-262.

//hexLit = do
//  try (string "0x")
//  digits <- many1 (oneOf "0123456789abcdefABCDEF")
//  [(hex,"")] <- return $ Numeric.readHex digits
//  return (True, hex)
var hexLit = do$
  (try_, string("0x"))
  ("digits" ,"<-", many1, oneOf("0123456789abcdefABCDEF"))
  ("hex"    ,"<-", returnCall, readHex, "digits")
  (ret, function(scope){ return [true, scope.hex] })

//TODO: 0.0
var octLit = do$
  ("digits" ,"<-", try_ ,"$",
            [char_('0') ,">>", [many1, oneOf("01234567") ,"<|>", return_("0")] ]
            ,"<*", notFollowedBy, oneOf(".89")
  )
  ("oct"    ,"<-", returnCall, readOct, "digits")
  (ret, function(scope){ return [true, scope.oct] })


//-- Creates a decimal value from a whole, fractional and exponent part.
//mkDecimal:: Double -> Double -> Int -> Double
//mkDecimal w f e =
//  if (f >= 1.0)
//    then mkDecimal w (f / 10.0) e
//    else (w + f) * (10.0 ^^ e)
function mkDecimal(w, f, e){
  //return parseFloat(w + "." + f) * Math.pow(10.0, e);
  return (f >= 1.0) ?
    mkDecimal(w, f / 10.0, e) :
    (w + f) * Math.pow(10.0, e);
}


//exponentPart = do
//  oneOf "eE"
//  (char '+' >> decimal) <|> (char '-' >> negate `fmap` decimal) <|> decimal
var exponentPart = do$
  (oneOf, "eE")
  ([char_('+') ,">>", lex.decimal]
    ,"<|>", [char_('-') ,">>",  negate ,op(fmap), lex.decimal]
    ,"<|>", lex.decimal)


//--wrap a parser's result in a Just:
//jparser p = p >>= (return . Just) 
function jparser(p){
    return ex(p ,">>=", [return_ ,".", Maybe.Just]);
}


//decLit = 
//  (do whole <- decimal
//      mfrac <- option Nothing (jparser (char '.' >> decimal))
//      mexp <-  option Nothing (jparser exponentPart)
//      if (mfrac == Nothing && mexp == Nothing)
//        then return (True, fromIntegral whole)
//        else return (False, mkDecimal (fromIntegral whole) 
//                                      (fromIntegral (maybe 0 id mfrac))
//                                      (fromIntegral (maybe 0 id mexp)))) <|>
//  (do frac <- char '.' >> decimal
//      exp <- option 0 exponentPart
//      return (False, mkDecimal 0.0 (fromIntegral frac) (fromIntegral exp)))
var decLit = ex(do$
  ("whole" ,"<-", lex.decimal)
  ("mfrac" ,"<-", option, Maybe.Nothing, [jparser, [char_('.') ,">>", lex.decimal]])
  ("mexp"  ,"<-", option, Maybe.Nothing, [jparser, exponentPart])
  (ret, function(scope){
   return (scope.mfrac == Maybe.Nothing && scope.mexp == Maybe.Nothing) ?
            [true, scope.whole] :
            [false, mkDecimal(scope.whole,
                              maybe(0, id, scope.mfrac),
                              maybe(0, id, scope.mexp)
                              )];
  })
  ,"<|>", do$
  ("frac" ,"<-", char_('.') ,">>", lex.decimal)
  ("exp"  ,"<-", option, 0, exponentPart)
  (ret, function(scope){
    return [false, mkDecimal(0.0, scope.frac, scope.exp)];
  })
);

//parseNumLit:: ExpressionParser st
//parseNumLit = do
//    pos <- getPosition
//    (isint, num) <- lexeme $ hexLit <|> decLit
//    notFollowedBy identifierStart <?> "whitespace"
//    if isint
//      then return $ IntLit pos (round num) 
//      else return $ NumLit pos num
var parseNumLit = do$
  ("pos" ,"<-", getPosition)
  ("isint_num" ,"<-", lex.lexeme ,"$", hexLit ,"<|>", octLit ,"<|>", decLit)
  (notFollowedBy, identifierStart ,"<?>", "whitespace")
  (ret, function(scope){
    var isint = scope.isint_num[0];
    var num = scope.isint_num[1];
    return isint ?
            Expression.IntLit(scope.pos, round(num)) :
            Expression.NumLit(scope.pos, num)
  })
    

//------------------------------------------------------------------------------
//-- Position Helper
//------------------------------------------------------------------------------

//withPos cstr p = do { pos <- getPosition; e <- p; return $ cstr pos e }
function withPos(cstr, p){
    return do_(
               bind("pos", getPosition),
               bind("e", p),
               ret(function(scope){ return cstr(scope.pos, scope.e) })
              );
}

//-------------------------------------------------------------------------------
//-- Compound Expression Parsers
//-------------------------------------------------------------------------------

//dotRef e = (reservedOp "." >> withPos cstr identifier) <?> "property.ref"
//    where cstr pos key = DotRef pos e key
function dotRef(e){
    function cstr(pos, key){ return Expression.DotRef(pos, e, key) }
    return ex([lex.reservedOp(".") ,">>", withPos, cstr, identifier]
            ,"<?>", "property.ref");
}


//funcApp e = (parens $ withPos cstr (parseExpression `sepBy` comma)) <?> "(function application)"
//    where cstr pos args = CallExpr pos e args
function funcApp(e){
    function cstr(pos, args){ return Expression.CallExpr(pos, e, args) }   
    return ex([lex.parens ,"$", withPos, cstr, [parseExpression ,op(sepBy), lex.comma]]
            ,"<?>", "(function application)");
}


//bracketRef e = (brackets $ withPos cstr parseExpression) <?> "[property-ref]"
//    where cstr pos key = BracketRef pos e key
function bracketRef(e){
    function cstr(pos, key){ return Expression.BracketRef(pos, e, key) }
    return ex([lex.brackets ,"$", withPos, cstr, parseExpression]
            ,"<?>", "[property-ref]");
}

//-------------------------------------------------------------------------------
//-- Expression Parsers
//-------------------------------------------------------------------------------

//parseParenExpr:: ExpressionParser st
//parseParenExpr = withPos ParenExpr (parens parseListExpr)
parseParenExpr = withPos(Expression.ParenExpr, lex.parens(parseListExpr));

//-- everything above expect functions
//parseExprForNew = parseThisRef <|> parseNullLit <|> parseBoolLit <|> parseStringLit 
//  <|> parseArrayLit <|> parseParenExpr <|> parseNewExpr <|> parseNumLit 
//  <|> parseRegexpLit <|> parseObjectLit <|> parseVarRef
var parseExprForNew = ex(
            parseThisRef
    ,"<|>", parseNullLit
    ,"<|>", parseBoolLit
    ,"<|>", parseStringLit 
    ,"<|>", parseArrayLit
    ,"<|>", parseParenExpr
    ,"<|>", parseNewExpr
    ,"<|>", parseNumLit
    ,"<|>", parseRegexpLit
    ,"<|>", parseObjectLit
    ,"<|>", parseVarRef
);
  

//-- all the expression parsers defined above
//parseSimpleExpr' = parseThisRef <|> parseNullLit <|> parseBoolLit 
//  <|> parseStringLit <|> parseArrayLit <|> parseParenExpr
//  <|> parseFuncExpr <|> parseNumLit <|> parseRegexpLit <|> parseObjectLit
//  <|> parseVarRef
var parseSimpleExpr_ = ex(
            parseThisRef
    ,"<|>", parseNullLit
    ,"<|>", parseBoolLit 
    ,"<|>", parseStringLit
    ,"<|>", parseArrayLit
    ,"<|>", parseParenExpr
    ,"<|>", parseFuncExpr
    ,"<|>", parseNumLit
    ,"<|>", parseRegexpLit
    ,"<|>", parseObjectLit
    ,"<|>", parseVarRef
);

//parseSimpleExprForNew (Just e) = (do
//    e' <- dotRef e <|> bracketRef e
//    parseSimpleExprForNew $ Just e') <|> (return e)
//parseSimpleExprForNew Nothing = do
//  e <- parseNewExpr <?> "expression (3)"
//  parseSimpleExprForNew (Just e)
function parseSimpleExprForNew(maybeVal){
    if(maybeVal.Just){
        var e = maybeVal[0];
        return ex(do$
            ("e_" ,"<-", dotRef(e) ,"<|>", bracketRef(e))
            (function(scope, state, k){
                return parseSimpleExprForNew(Maybe.Just(scope.e_))(scope, state, k);
            }) ,"<|>", return_(e))
    }
    if(maybeVal.Nothing){
        return do$
            ("e"  ,"<-", parseNewExpr ,"<?>", "expression (3)")
            (function(scope, state, k){
                return parseSimpleExprForNew(Maybe.Just(scope.e))(scope, state, k);
            })
    }
}

//parseNewExpr =
//  (do pos <- getPosition
//      reserved "new"
//      constructor <- parseSimpleExprForNew Nothing -- right-associativity
//      arguments <- (try (parens (parseExpression `sepBy` comma))) <|> (return [])
//      return (NewExpr pos constructor arguments)) <|>
//  parseSimpleExpr'
parseNewExpr = ex(
  do$("pos" ,"<-", getPosition)
     (lex.reserved, "new")
     ("constructor_" ,"<-", parseSimpleExprForNew, Maybe.Nothing) // right-associativity
     ("arguments" ,"<-", [try_, [lex.parens, [parseExpression ,op(sepBy), lex.comma]]] ,"<|>", return_([]))
     (returnCall, Expression.NewExpr, "pos", "constructor_", "arguments")
  ,"<|>", parseSimpleExpr_
  );
   

//parseSimpleExpr (Just e) = (do
//    e' <- dotRef e <|> funcApp e <|> bracketRef e
//    parseSimpleExpr $ Just e') <|> (return e)
//parseSimpleExpr Nothing = do
//  e <- parseNewExpr <?> "expression (3)"
//  parseSimpleExpr (Just e)
function parseSimpleExpr(maybeVal){
    if(maybeVal.Just){
        var e = maybeVal[0];
        return ex(do$
            ("e_" ,"<-", dotRef(e) ,"<|>", funcApp(e) ,"<|>", bracketRef(e))
            (function(scope, state, k){
                return parseSimpleExpr(Maybe.Just(scope.e_))(scope, state, k);
            }) ,"<|>", return_(e))
    }
    if(maybeVal.Nothing){
        return do$
            ("e" ,"<-", parseNewExpr ,"<?>", "expression (3)")
            (function(scope, state, k){
                return parseSimpleExpr(Maybe.Just(scope.e))(scope, state, k);
            })
    }
}


//makeInfixExpr str constr = Infix parser AssocLeft where
//  parser:: CharParser st (Expression SourcePos -> Expression SourcePos -> Expression SourcePos)
//  parser = do
//    pos <- getPosition
//    reservedOp str
//    return (InfixExpr pos constr)  -- points-free, returns a function
function makeInfixExpr(str, constr){
    var parser = do$
        ("pos" ,"<-", getPosition)
        (lex.reservedOp(str))
        (ret, function(scope){
            return function(exp1, exp2){
                return Expression.InfixExpr(scope.pos, constr, exp1, exp2);
            }
        })
    
    return Operator.Infix(parser, Assoc.AssocLeft);
}

//-- apparently, expression tables can't handle immediately-nested prefixes
//parsePrefixedExpr = do
//  pos <- getPosition
//  op <- optionMaybe $ (reservedOp "!" >> return PrefixLNot) <|> 
//                      (reservedOp "~" >> return PrefixBNot) <|>
//                      (try (lexeme $ char '-' >> notFollowedBy (char '-')) >>
//                       return PrefixMinus) <|>
//                      (try (lexeme $ char '+' >> notFollowedBy (char '+')) >>
//                       return PrefixPlus) <|>
//                      (reserved "typeof" >> return PrefixTypeof) <|>
//                      (reserved "void" >> return PrefixVoid) <|>
//                      (reserved "delete" >> return PrefixDelete)
//  case op of
//    Nothing -> unaryAssignExpr
//    Just op -> do
//      innerExpr <- parsePrefixedExpr
//      return (PrefixExpr pos op innerExpr)
var parsePrefixedExpr = do$
  ("pos" ,"<-", getPosition)
  ("op"  ,"<-", optionMaybe ,"$",
                        [lex.reservedOp("!")    ,">>", return_, PrefixOp.PrefixLNot]
                ,"<|>", [lex.reservedOp("~")    ,">>", return_, PrefixOp.PrefixBNot]
                ,"<|>", [try_, [lex.lexeme ,"$", char_('-') ,">>", notFollowedBy, char_('-')]
                                                ,">>", return_, PrefixOp.PrefixMinus]
                ,"<|>", [try_, [lex.lexeme ,"$", char_('+') ,">>", notFollowedBy, char_('+')]
                                                ,">>", return_, PrefixOp.PrefixPlus]
                ,"<|>", [lex.reserved, "typeof" ,">>", return_, PrefixOp.PrefixTypeof]
                ,"<|>", [lex.reserved, "void"   ,">>", return_, PrefixOp.PrefixVoid]
                ,"<|>", [lex.reserved, "delete" ,">>", return_, PrefixOp.PrefixDelete]
  )
  (function(scope, state, k){
    var op = scope.op, res;
    if(op.Nothing)
        res = unaryAssignExpr;
    if(op.Just)
        res = do$
            ("innerExpr" ,"<-", parsePrefixedExpr)
            (ret, function(_scope){ return Expression.PrefixExpr(scope.pos, op[0], _scope.innerExpr) })
    return res(scope, state, k);
  })

//exprTable:: [[Operator Char st ParsedExpression]]
//exprTable = 
//  [
//   [makeInfixExpr "*" OpMul, makeInfixExpr "/" OpDiv, makeInfixExpr "%" OpMod],
//   [makeInfixExpr "+" OpAdd, makeInfixExpr "-" OpSub],
//   [makeInfixExpr "<<" OpLShift, makeInfixExpr ">>" OpSpRShift,
//    makeInfixExpr ">>>" OpZfRShift],
//   [makeInfixExpr "<" OpLT, makeInfixExpr "<=" OpLEq, makeInfixExpr ">" OpGT,
//    makeInfixExpr ">=" OpGEq, 
//    makeInfixExpr "instanceof" OpInstanceof, makeInfixExpr "in" OpIn],
//   [makeInfixExpr "&" OpBAnd], 
//   [makeInfixExpr "^" OpBXor], 
//   [makeInfixExpr "|" OpBOr],
//   [makeInfixExpr "&&" OpLAnd],
//   [makeInfixExpr "||" OpLOr],  
//   [makeInfixExpr "==" OpEq, makeInfixExpr "!=" OpNEq,
//    makeInfixExpr "===" OpStrictEq, makeInfixExpr "!==" OpStrictNEq]
//    ]
var exprTable = 
  [
   [makeInfixExpr("*",  InfixOp.OpMul),
    makeInfixExpr("/",  InfixOp.OpDiv),
    makeInfixExpr("%",  InfixOp.OpMod)],
   [makeInfixExpr("+",  InfixOp.OpAdd),
    makeInfixExpr("-",  InfixOp.OpSub)],
   [makeInfixExpr("<<", InfixOp.OpLShift),
    makeInfixExpr(">>", InfixOp.OpSpRShift),
    makeInfixExpr(">>>",InfixOp.OpZfRShift)],
   [makeInfixExpr("<",  InfixOp.OpLT),
    makeInfixExpr("<=", InfixOp.OpLEq),
    makeInfixExpr(">",  InfixOp.OpGT),
    makeInfixExpr(">=", InfixOp.OpGEq),
    makeInfixExpr("instanceof", InfixOp.OpInstanceof),
    makeInfixExpr("in", InfixOp.OpIn)],
   [makeInfixExpr("&",  InfixOp.OpBAnd)],
   [makeInfixExpr("^",  InfixOp.OpBXor)],
   [makeInfixExpr("|",  InfixOp.OpBOr)],
   [makeInfixExpr("&&", InfixOp.OpLAnd)],
   [makeInfixExpr("||", InfixOp.OpLOr)],
   [makeInfixExpr("==", InfixOp.OpEq),
    makeInfixExpr("!=", InfixOp.OpNEq),
    makeInfixExpr("===",InfixOp.OpStrictEq),
    makeInfixExpr("!==",InfixOp.OpStrictNEq)]
  ].reverse();


//parseExpression' = 
//  buildExpressionParser exprTable parsePrefixedExpr <?> "simple expression"
var parseExpression_ = 
  label(buildExpressionParser(exprTable, parsePrefixedExpr), "simple expression");


//asLValue :: SourcePos
//         -> Expression SourcePos 
//         -> CharParser st (LValue SourcePos)
//asLValue p' e = case e of
//  VarRef p (Id _ x) -> return (LVar p x)
//  DotRef p e (Id _ x) -> return (LDot p e x)
//  BracketRef p e1 e2 -> return (LBracket p e1 e2)
//  otherwise -> fail $ "expeceted l-value at " ++ show p'
function asLValue(p_, e){
    if(e.VarRef)
        if(e[1].Id)
            return return_(LValue.LVar(e[0], e[1][1]));
    if(e.DotRef)
        if(e[2].Id)
            return return_(LValue.LDot(e[0], e[1], e[2][1]));
    if(e.BracketRef)
        return return_(LValue.LBracket(e[0], e[1], e[2]));
    return fail("expeceted l-value at " + p_)
}

//lvalue :: CharParser st (LValue SourcePos)
//lvalue = do
//  p <- getPosition
//  e <- parseSimpleExpr Nothing
//  asLValue p e
var lvalue = do$
  ("p" ,"<-", getPosition)
  ("e" ,"<-", parseSimpleExpr, Maybe.Nothing)
  (function(scope, state, k){ return asLValue(scope.p, scope.e)(scope, state, k) })
  
  
//unaryAssignExpr :: CharParser st ParsedExpression
//unaryAssignExpr = do
//  p <- getPosition
//  let prefixInc = do
//        reservedOp "++"
//        liftM (UnaryAssignExpr p PrefixInc) lvalue
//  let prefixDec = do
//        reservedOp "--"
//        liftM (UnaryAssignExpr p PrefixDec) lvalue
//  let postfixInc e = do
//        reservedOp "++"
//        liftM (UnaryAssignExpr p PostfixInc) (asLValue p e)
//  let postfixDec e = do
//        reservedOp "--"
//        liftM (UnaryAssignExpr p PostfixDec) (asLValue p e)
//  let other = do
//        e <- parseSimpleExpr Nothing
//        postfixInc e <|> postfixDec e <|> return e
//  prefixInc <|> prefixDec <|> other

function _createExpr(pos, ctr){
    return function(lval){
        return Expression.UnaryAssignExpr(pos, UnaryAssignOp[ctr], lval)
    }
}

function onSameLineExp(postfix, notpostfix){
    return function(scope, state, k){
        var op = state.substring(0, 2);
            
        if(op == "--" || op == "++"){
            var m = state.input.substring(0, state.index - state.dropped).match(/\s*$/);
            var autosemi = m && /\n|\r/.test(m[0]);
            return (autosemi ? notpostfix : postfix)(scope, state, k);
        }else
            return notpostfix(scope, state, k);
    }
}

var unaryAssignExpr = parserBind(getPosition, function(pos){
    
    var prefixInc = do_(lex.reservedOp("++"),
                        liftM(_createExpr(pos, "PrefixInc"), lvalue));
                    
    var prefixDec = do_(lex.reservedOp("--"),
                        liftM(_createExpr(pos, "PrefixDec"), lvalue));
                    
    function postfixInc(e){
        return do_(lex.reservedOp("++"),
                   liftM(_createExpr(pos, "PostfixInc"), asLValue(pos, e)));
    }
    
    function postfixDec(e){
        return do_(lex.reservedOp("--"),
                   liftM(_createExpr(pos, "PostfixDec"), asLValue(pos, e)));
    }
    
    //var other = parserBind(parseSimpleExpr(Maybe.Nothing), function(e){
    //    return parserPlus(postfixInc(e), parserPlus(postfixDec(e), return_(e)));
    //});
    var postFixOrSimpleExpr = parserBind(parseSimpleExpr(Maybe.Nothing), function(e){
        return  onSameLineExp(
                    //if ++ or -- is on the same line
                    //then they are postfix operators
                    parserPlus(postfixInc(e), parserPlus(postfixDec(e), return_(e))),
                    //otherwise a semicolon is needed before the ops
                    //so they will be consumed by the next parser,
                    //and the current expression is returned
                    return_(e)
                )
    });
    
    return parserPlus(prefixInc, parserPlus(prefixDec, postFixOrSimpleExpr));
  
})


//parseTernaryExpr':: CharParser st (ParsedExpression,ParsedExpression)
//parseTernaryExpr' = do
//    reservedOp "?"
//    l <- assignExpr
//    colon
//    r <- assignExpr
//    return $(l,r)
var parseTernaryExpr_ = do$
  (lex.reservedOp("?"))
  ("l" ,"<-", assignExpr)
  (lex.colon)
  ("r" ,"<-", assignExpr)
  (ret, function(scope){ return [scope.l, scope.r] })
  

//parseTernaryExpr:: ExpressionParser st
//parseTernaryExpr = do
//  e <- parseExpression'
//  e' <- optionMaybe parseTernaryExpr'
//  case e' of
//    Nothing -> return e
//    Just (l,r) -> do p <- getPosition
//                     return $ CondExpr p e l r
var parseTernaryExpr = do$
  ("e"  ,"<-", parseExpression_)
  ("e_" ,"<-", optionMaybe, parseTernaryExpr_)
  (function(scope, state, k){
    var e_ = scope.e_,
        e = scope.e,
        res;
    if(e_.Nothing)
        res = return_(scope.e);
    if(e_.Just){
        var l = e_[0][0],
            r = e_[0][1];
        res = do$("p" ,"<-", getPosition)
                 (ret, function(scope){ return Expression.CondExpr(scope.p, e, l, r) })
    }
    return res(scope, state, k);
  })


//assignOp :: CharParser st AssignOp
//assignOp = 
//  (reservedOp "=" >> return OpAssign) <|>
//  (reservedOp "+=" >> return OpAssignAdd) <|>
//  (reservedOp "-=" >> return OpAssignSub) <|>
//  (reservedOp "*=" >> return OpAssignMul) <|>
//  (reservedOp "/=" >> return OpAssignDiv) <|>
//  (reservedOp "%=" >> return OpAssignMod) <|>
//  (reservedOp "<<=" >> return OpAssignLShift) <|>
//  (reservedOp ">>=" >> return OpAssignSpRShift) <|>
//  (reservedOp ">>>=" >> return OpAssignZfRShift) <|>
//  (reservedOp "&=" >> return OpAssignBAnd) <|>
//  (reservedOp "^=" >> return OpAssignBXor) <|>
//  (reservedOp "|=" >> return OpAssignBOr)
var assignOp = ex(
            [lex.reservedOp("=")   ,">>", return_, AssignOp.OpAssign     ]
    ,"<|>", [lex.reservedOp("+=")  ,">>", return_, AssignOp.OpAssignAdd  ]
    ,"<|>", [lex.reservedOp("-=")  ,">>", return_, AssignOp.OpAssignSub  ]
    ,"<|>", [lex.reservedOp("*=")  ,">>", return_, AssignOp.OpAssignMul  ]
    ,"<|>", [lex.reservedOp("/=")  ,">>", return_, AssignOp.OpAssignDiv  ]
    ,"<|>", [lex.reservedOp("%=")  ,">>", return_, AssignOp.OpAssignMod  ]
    ,"<|>", [lex.reservedOp("<<=") ,">>", return_, AssignOp.OpAssignLShift   ]
    ,"<|>", [lex.reservedOp(">>=") ,">>", return_, AssignOp.OpAssignSpRShift ]
    ,"<|>", [lex.reservedOp(">>>="),">>", return_, AssignOp.OpAssignZfRShift ]
    ,"<|>", [lex.reservedOp("&=")  ,">>", return_, AssignOp.OpAssignBAnd ]
    ,"<|>", [lex.reservedOp("^=")  ,">>", return_, AssignOp.OpAssignBXor ]
    ,"<|>", [lex.reservedOp("|=")  ,">>", return_, AssignOp.OpAssignBOr  ]
);

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
assignExpr = do$
  ("p"   ,"<-", getPosition)
  ("lhs" ,"<-", parseTernaryExpr)
  (do$("op"  ,"<-", assignOp)
      ("lhs" ,"<-", withScope, function(scope){
        //bring p to the current scope, so that returnCall can be used:
        scope.p = scope.scope.p;
        return asLValue(scope.scope.p, scope.scope.lhs);
      })
      ("rhs" ,"<-", withScope, function(){ return assignExpr })
      (returnCall, Expression.AssignExpr, "p", "op", "lhs", "rhs")
  ,"<|>", ret, "lhs")


//parseExpression:: ExpressionParser st
//parseExpression = assignExpr
parseExpression = assignExpr;

//parseListExpr =
//  liftM2 ListExpr getPosition (assignExpr `sepBy1` comma)
parseListExpr =
    liftM2(Expression.ListExpr, getPosition, sepBy1(assignExpr, lex.comma));


//parseScript:: CharParser state (JavaScript SourcePos)
//parseScript = do
//  whiteSpace
//  liftM2 Script getPosition (parseStatement `sepBy` whiteSpace)
var parseScript = do$
  (lex.whiteSpace)
  (liftM2, JavaScript.Script, getPosition, many(parseStatement))
  

//parseJavaScriptFromFile :: MonadIO m => String -> m [Statement SourcePos]
//parseJavaScriptFromFile filename = do
//  chars <- liftIO $ readFile filename
//  case parse parseScript filename chars of
//    Left err               -> fail (show err)
//    Right (Script _ stmts) -> return stmts


//parseScriptFromString :: String -> String 
//                      -> Either ParseError (JavaScript SourcePos)
//parseScriptFromString src script = parse parseScript src script


//emptyParsedJavaScript = 
//  Script (error "Parser.emptyParsedJavaScript--no annotation") []
  
  
//parseString :: String -> [Statement SourcePos]
//parseString str = case parse parseScript "" str of
//  Left err -> error (show err)
//  Right (Script _ stmts) -> stmts


namespace("BrownPLT_JavaScript_Parser", {
     parseScript            : parseScript
    ,parseExpression        : parseExpression
    //,parseString            : parseString
    //,parseScriptFromString  : parseScriptFromString
    //,emptyParsedJavaScript  : emptyParsedJavaScript
    //,ParsedStatement        : ParsedStatement
    //,ParsedExpression       : ParsedExpression
    //,parseJavaScriptFromFile: parseJavaScriptFromFile
    ,parseSimpleExpr_       : parseSimpleExpr_
    ,parseBlockStmt         : parseBlockStmt
    ,parseStatement         : parseStatement
    //,StatementParser        : StatementParser
    //,ExpressionParser       : ExpressionParser
    ,assignExpr             : assignExpr
})
}());;(function(){
/// <reference path="JavaScript/Lexer.js" />
/// <reference path="JavaScript/Syntax.js" />
/// <reference path="JavaScript/Parser.js" />


namespace("BrownPLT_JavaScript")
importSubmodules("BrownPLT_JavaScript",
    ["Lexer"
    ,"Syntax"
    ,"Parser"
    ])


}());
}());
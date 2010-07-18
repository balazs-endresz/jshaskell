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

}());
}());
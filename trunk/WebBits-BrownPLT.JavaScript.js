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


var operators = {};



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
    ,asTypeOf    : asTypeOf
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
;var Bool = NS['Haskell'].Bool, isArray = NS['Haskell'].isArray, isDefined = NS['Haskell'].isDefined, slice = NS['Haskell'].slice, imap = NS['Haskell'].imap, ifilter = NS['Haskell'].ifilter, indexOf = NS['Haskell'].indexOf, lastIndexOf = NS['Haskell'].lastIndexOf, isort = NS['Haskell'].isort, range = NS['Haskell'].range, extend = NS['Haskell'].extend, namespace = NS['Haskell'].namespace, typeOf = NS['Haskell'].typeOf, strictEq = NS['Haskell'].strictEq, strictNe = NS['Haskell'].strictNe, unsafeAdd = NS['Haskell'].unsafeAdd, unsafeSub = NS['Haskell'].unsafeSub, unsafeMul = NS['Haskell'].unsafeMul, unsafeDiv = NS['Haskell'].unsafeDiv, lt = NS['Haskell'].lt, le = NS['Haskell'].le, gt = NS['Haskell'].gt, ge = NS['Haskell'].ge, negate = NS['Haskell'].negate, evalThunks = NS['Haskell'].evalThunks, toArray = NS['Haskell'].toArray, curry = NS['Haskell'].curry, error = NS['Haskell'].error, data = NS['Haskell'].data, ADT = NS['Haskell'].ADT, record = NS['Haskell'].record, accessor = NS['Haskell'].accessor, accessors = NS['Haskell'].accessors, typeclass = NS['Haskell'].typeclass, VARARG = NS['Haskell'].VARARG, instance = NS['Haskell'].instance, getInstance = NS['Haskell'].getInstance, asTypeOf = NS['Haskell'].asTypeOf, operators = NS['Haskell'].operators, infix = NS['Haskell'].infix, infixl = NS['Haskell'].infixl, infixr = NS['Haskell'].infixr, op = NS['Haskell'].op, no = NS['Haskell'].no, resolve = NS['Haskell'].resolve, recurse = NS['Haskell'].recurse, Recurse = NS['Haskell'].Recurse, exl = NS['Haskell'].exl, exs = NS['Haskell'].exs, createDo = NS['Haskell'].createDo, Scope = NS['Haskell'].Scope;;;;;;;﻿/// <reference path="../../../jshaskell/src/Haskell.js" local />

function Ordering(){}
data(Ordering, ["LT", "EQ", "GT"]);

namespace("GHC_Ordering", {
    Ordering : Ordering
})﻿/// <reference path="../../../jshaskell/src/Haskell.js" local />
/// <reference path="Ordering.js" local />

var a = "a";

//{-# OPTIONS_GHC -XNoImplicitPrelude #-}
//{-# OPTIONS_HADDOCK hide #-}
//-----------------------------------------------------------------------------
//-- |
//-- Module      :  GHC.Classes
//-- Copyright   :  (c) The University of Glasgow, 1992-2002
//-- License     :  see libraries/base/LICENSE
//--
//-- Maintainer  :  cvs-ghc@haskell.org
//-- Stability   :  internal
//-- Portability :  non-portable (GHC extensions)
//--
//-- Basic classes.
//--
//-----------------------------------------------------------------------------

//module GHC.Classes where

//import GHC.Bool
//import GHC.Ordering

//TODO:
//infix  4  ==, /=, <, <=, >=, >
//infixr 3  &&
//infixr 2  ||

//default ()              -- Double isn't available yet

//-- | The 'Eq' class defines equality ('==') and inequality ('/=').
//-- All the basic datatypes exported by the "Prelude" are instances of 'Eq',
//-- and 'Eq' may be derived for any datatype whose constituents are also
//-- instances of 'Eq'.
//--
//-- Minimal complete definition: either '==' or '/='.
//--
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
//class  (Eq a) => Ord a  where
//    compare              :: a -> a -> Ordering
//    (<), (<=), (>), (>=) :: a -> a -> Bool
//    max, min             :: a -> a -> a

//    compare x y = if x == y then EQ
//                  -- NB: must be '<=' not '<' to validate the
//                  -- above claim about the minimal things that
//                  -- can be defined for an instance of Ord:
//                  else if x <= y then LT
//                  else GT

//    x <  y = case compare x y of { LT -> True;  _ -> False }
//    x <= y = case compare x y of { GT -> False; _ -> True }
//    x >  y = case compare x y of { GT -> True;  _ -> False }
//    x >= y = case compare x y of { LT -> False; _ -> True }

//        -- These two default methods use '<=' rather than 'compare'
//        -- because the latter is often more expensive
//    max x y = if x <= y then y else x
//    min x y = if x <= y then x else y

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
    .impl(function(inst){ 
      var eq = getInstance(Eq, inst._type).eq;
      return {
        compare: function (x, y) {
            return  eq(x, y)         ? Ordering.EQ :
                    inst["<="](x, y) ? Ordering.LT :
                                       Ordering.GT ;
        },
        "<"  : function (x, y) { return inst.compare(x, y).LT ? true  : false },
        "<=" : function (x, y) { return inst.compare(x, y).GT ? false :  true },
        ">"  : function (x, y) { return inst.compare(x, y).GT ? true  : false },
        ">=" : function (x, y) { return inst.compare(x, y).LT ? false :  true },
        max  : function (x, y) { return inst["<="](x, y) ? y : x },
        min  : function (x, y) { return inst["<="](x, y) ? x : y }
    }});

//-- OK, so they're technically not part of a class...:

//-- Boolean functions

//-- | Boolean \"and\"
//(&&)                    :: Bool -> Bool -> Bool
//True  && x              =  x
//False && _              =  False
function andOp(a, b){
    return (a === true)  ? b :
           (a === false) ? false : error(and);
}

//-- | Boolean \"or\"
//(||)                    :: Bool -> Bool -> Bool
//True  || _              =  True
//False || x              =  x
function orOp(a, b){
    return (a === true)  ? true :
           (a === false) ? b : error(or);
}

//-- | Boolean \"not\"
//not                     :: Bool -> Bool
//not True                =  False
//not False               =  True
function not(a){
    return (a === true)  ? false :
           (a === false) ? true  : error(not);
}


namespace("GHC_Classes", {
     Eq    : Eq 
    ,Ord   : Ord
    ,andOp : andOp
    ,orOp  : orOp
    ,not   : not
})﻿/// <reference path="../../../jshaskell/src/Haskell.js" local />

// tuples are not defined with `data`, they are just simple classes
var Tuple = {};
imap(function (n) {
    var ntuple = "Tuple" + n;
    Tuple[ntuple] = function () { };
    Tuple[ntuple]._name = ntuple;
    Tuple["tuple" + n] = function (t) {
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

namespace("GHC_Tuple", {
    Tuple : Tuple,
    tuple : tuple
})﻿/// <reference path="../../../jshaskell/src/Haskell.js" local />

//the () type
function Unit(){}
data(Unit, ["Unit"])
var unit = Unit.Unit;

namespace("GHC_Unit", {
    Unit : Unit,
    unit : unit
})﻿/// <reference path="../../../jshaskell/src/Haskell.js" local />
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
﻿/// <reference path="../../../jshaskell/src/Haskell.js" local />
/// <reference path="Base.js" local />

//\begin{code}
//{-# OPTIONS_GHC -XNoImplicitPrelude #-}
//{-# OPTIONS_HADDOCK hide #-}
//-----------------------------------------------------------------------------
//-- |
//-- Module      :  GHC.Show
//-- Copyright   :  (c) The University of Glasgow, 1992-2002
//-- License     :  see libraries/base/LICENSE
//-- 
//-- Maintainer  :  cvs-ghc@haskell.org
//-- Stability   :  internal
//-- Portability :  non-portable (GHC Extensions)
//--
//-- The 'Show' class, and related operations.
//--
//-----------------------------------------------------------------------------

//-- #hide
//module GHC.Show
//        (
//        Show(..), ShowS,

//        -- Instances for Show: (), [], Bool, Ordering, Int, Char

//        -- Show support code
//        shows, showChar, showString, showParen, showList__, showSpace,
//        showLitChar, protectEsc,
//        intToDigit, showSignedInt,
//        appPrec, appPrec1,

//        -- Character operations
//        asciiTab,
//  )
//        where

//import GHC.Base
//import Data.Maybe
//import GHC.List ((!!), foldr1)
//\end{code}



//%*********************************************************
//%*                                                      *
//\subsection{The @Show@ class}
//%*                                                      *
//%*********************************************************

//\begin{code}
//-- | The @shows@ functions return a function that prepends the
//-- output 'String' to an existing 'String'.  This allows constant-time
//-- concatenation of results using function composition.
//type ShowS = String -> String

//-- | Conversion of values to readable 'String's.
//--
//-- Minimal complete definition: 'showsPrec' or 'show'.
//--
//-- Derived instances of 'Show' have the following properties, which
//-- are compatible with derived instances of 'Text.Read.Read':
//--
//-- * The result of 'show' is a syntactically correct Haskell
//--   expression containing only constants, given the fixity
//--   declarations in force at the point where the type is declared.
//--   It contains only the constructor names defined in the data type,
//--   parentheses, and spaces.  When labelled constructor fields are
//--   used, braces, commas, field names, and equal signs are also used.
//--
//-- * If the constructor is defined to be an infix operator, then
//--   'showsPrec' will produce infix applications of the constructor.
//--
//-- * the representation will be enclosed in parentheses if the
//--   precedence of the top-level constructor in @x@ is less than @d@
//--   (associativity is ignored).  Thus, if @d@ is @0@ then the result
//--   is never surrounded in parentheses; if @d@ is @11@ it is always
//--   surrounded in parentheses, unless it is an atomic expression.
//--
//-- * If the constructor is defined using record syntax, then 'show'
//--   will produce the record-syntax form, with the fields given in the
//--   same order as the original declaration.
//--
//-- For example, given the declarations
//--
//-- > infixr 5 :^:
//-- > data Tree a =  Leaf a  |  Tree a :^: Tree a
//--
//-- the derived instance of 'Show' is equivalent to
//--
//-- > instance (Show a) => Show (Tree a) where
//-- >
//-- >        showsPrec d (Leaf m) = showParen (d > app_prec) $
//-- >             showString "Leaf " . showsPrec (app_prec+1) m
//-- >          where app_prec = 10
//-- >
//-- >        showsPrec d (u :^: v) = showParen (d > up_prec) $
//-- >             showsPrec (up_prec+1) u . 
//-- >             showString " :^: "      .
//-- >             showsPrec (up_prec+1) v
//-- >          where up_prec = 5
//--
//-- Note that right-associativity of @:^:@ is ignored.  For example,
//--
//-- * @'show' (Leaf 1 :^: Leaf 2 :^: Leaf 3)@ produces the string
//--   @\"Leaf 1 :^: (Leaf 2 :^: Leaf 3)\"@.

//class  Show a  where
//    -- | Convert a value to a readable 'String'.
//    --
//    -- 'showsPrec' should satisfy the law
//    --
//    -- > showsPrec d x r ++ s  ==  showsPrec d x (r ++ s)
//    --
//    -- Derived instances of 'Text.Read.Read' and 'Show' satisfy the following:
//    --
//    -- * @(x,\"\")@ is an element of
//    --   @('Text.Read.readsPrec' d ('showsPrec' d x \"\"))@.
//    --
//    -- That is, 'Text.Read.readsPrec' parses the string produced by
//    -- 'showsPrec', and delivers the value that 'showsPrec' started with.

//    showsPrec :: Int    -- ^ the operator precedence of the enclosing
//                        -- context (a number from @0@ to @11@).
//                        -- Function application has precedence @10@.
//              -> a      -- ^ the value to be converted to a 'String'
//              -> ShowS

//    -- | A specialised variant of 'showsPrec', using precedence context
//    -- zero, and returning an ordinary 'String'.
//    show      :: a   -> String

//    -- | The method 'showList' is provided to allow the programmer to
//    -- give a specialised way of showing lists of values.
//    -- For example, this is used by the predefined 'Show' instance of
//    -- the 'Char' type, where values of type 'String' should be shown
//    -- in double quotes, rather than between square brackets.
//    showList  :: [a] -> ShowS

//    showsPrec _ x s = show x ++ s
//    show x          = shows x ""
//    showList ls   s = showList__ shows ls s

var Show = typeclass("Show", "a")
    .types({
        show      : ["a", String]
    })
    .impl({
        show      : function(a){ return a.toString() }
    })

/*
var Show = typeclass("Show", "a")
    .types({
        showsPrec : [Int, "a", VARARG],
        show      : ["a", String],
        showList  : ["a",  VARARG]
    }).impl(function(inst){ return{
        showsPrec : function(_, x, s){ return append(inst.show(x), s) },
        show      : function(x){ return shows(x, "") },
        showList  : function(ls, s){ return showList__(shows, ls, s) }
    }})
*/


//showList__ :: (a -> ShowS) ->  [a] -> ShowS
//showList__ _     []     s = "[]" ++ s
//showList__ showx (x:xs) s = '[' : showx x (showl xs)
//  where
//    showl []     = ']' : s
//    showl (y:ys) = ',' : showx y (showl ys)

//appPrec, appPrec1 :: Int
//        -- Use unboxed stuff because we don't have overloaded numerics yet
//appPrec = I# 10#        -- Precedence of application:
//                        --   one more than the maximum operator precedence of 9
//appPrec1 = I# 11#       -- appPrec + 1
//\end{code}

//%*********************************************************
//%*                                                      *
//\subsection{Simple Instances}
//%*                                                      *
//%*********************************************************

//\begin{code}
// 
//instance  Show ()  where
//    showsPrec _ () = showString "()"
instance(Show, Unit, {
    show: function(a){ return "()" }
})

//instance Show a => Show [a]  where
//    showsPrec _         = showList

//instance Show Bool where
//  showsPrec _ True  = showString "True"
//  showsPrec _ False = showString "False"
instance(Show, Boolean)

//instance Show Ordering where
//  showsPrec _ LT = showString "LT"
//  showsPrec _ EQ = showString "EQ"
//  showsPrec _ GT = showString "GT"


//instance  Show Char  where
//    showsPrec _ '\'' = showString "'\\''"
//    showsPrec _ c    = showChar '\'' . showLitChar c . showChar '\''

//    showList cs = showChar '"' . showl cs
//                 where showl ""       s = showChar '"' s
//                       showl ('"':xs) s = showString "\\\"" (showl xs s)
//                       showl (x:xs)   s = showLitChar x (showl xs s)
//                -- Making 's' an explicit parameter makes it clear to GHC
//                -- that showl has arity 2, which avoids it allocating an extra lambda
//                -- The sticking point is the recursive call to (showl xs), which
//                -- it can't figure out would be ok with arity 2.
instance(Show, String, {
    show: function(a){ return '"' + a.toString() + '"' }
})

//instance Show Int where
//    showsPrec = showSignedInt
instance(Show, Number)

//instance Show a => Show (Maybe a) where
//    showsPrec _p Nothing s = showString "Nothing" s
//    showsPrec p (Just x) s
//                          = (showParen (p > appPrec) $ 
//                             showString "Just " . 
//                             showsPrec appPrec1 x) s
//\end{code}


//%*********************************************************
//%*                                                      *
//\subsection{Show instances for the first few tuples
//%*                                                      *
//%*********************************************************

//\begin{code}
//-- The explicit 's' parameters are important
//-- Otherwise GHC thinks that "shows x" might take a lot of work to compute
//-- and generates defns like
//--      showsPrec _ (x,y) = let sx = shows x; sy = shows y in
//--                          \s -> showChar '(' (sx (showChar ',' (sy (showChar ')' s))))

//instance  (Show a, Show b) => Show (a,b)  where
//  showsPrec _ (a,b) s = show_tuple [shows a, shows b] s

//instance (Show a, Show b, Show c) => Show (a, b, c) where
//  showsPrec _ (a,b,c) s = show_tuple [shows a, shows b, shows c] s

//instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
//  showsPrec _ (a,b,c,d) s = show_tuple [shows a, shows b, shows c, shows d] s

//instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
//  showsPrec _ (a,b,c,d,e) s = show_tuple [shows a, shows b, shows c, shows d, shows e] s

//instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a,b,c,d,e,f) where
//  showsPrec _ (a,b,c,d,e,f) s = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f] s

//instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g)
//        => Show (a,b,c,d,e,f,g) where
//  showsPrec _ (a,b,c,d,e,f,g) s 
//        = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g] s

//instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h)
//         => Show (a,b,c,d,e,f,g,h) where
//  showsPrec _ (a,b,c,d,e,f,g,h) s 
//        = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h] s

//instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i)
//         => Show (a,b,c,d,e,f,g,h,i) where
//  showsPrec _ (a,b,c,d,e,f,g,h,i) s 
//        = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h, 
//                      shows i] s

//instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j)
//         => Show (a,b,c,d,e,f,g,h,i,j) where
//  showsPrec _ (a,b,c,d,e,f,g,h,i,j) s 
//        = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h, 
//                      shows i, shows j] s

//instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k)
//         => Show (a,b,c,d,e,f,g,h,i,j,k) where
//  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k) s 
//        = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h, 
//                      shows i, shows j, shows k] s

//instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k,
//          Show l)
//         => Show (a,b,c,d,e,f,g,h,i,j,k,l) where
//  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l) s 
//        = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h, 
//                      shows i, shows j, shows k, shows l] s

//instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k,
//          Show l, Show m)
//         => Show (a,b,c,d,e,f,g,h,i,j,k,l,m) where
//  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l,m) s 
//        = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h, 
//                      shows i, shows j, shows k, shows l, shows m] s

//instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k,
//          Show l, Show m, Show n)
//         => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
//  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l,m,n) s 
//        = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h, 
//                      shows i, shows j, shows k, shows l, shows m, shows n] s

//instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k,
//          Show l, Show m, Show n, Show o)
//         => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
//  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) s 
//        = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h, 
//                      shows i, shows j, shows k, shows l, shows m, shows n, shows o] s

//show_tuple :: [ShowS] -> ShowS
//show_tuple ss = showChar '('
//              . foldr1 (\s r -> s . showChar ',' . r) ss
//              . showChar ')'
//\end{code}


//%*********************************************************
//%*                                                      *
//\subsection{Support code for @Show@}
//%*                                                      *
//%*********************************************************

//\begin{code}
//-- | equivalent to 'showsPrec' with a precedence of 0.
//shows           :: (Show a) => a -> ShowS
//shows           =  showsPrec zeroInt

//-- | utility function converting a 'Char' to a show function that
//-- simply prepends the character unchanged.
//showChar        :: Char -> ShowS
//showChar        =  (:)

//-- | utility function converting a 'String' to a show function that
//-- simply prepends the string unchanged.
//showString      :: String -> ShowS
//showString      =  (++)

//-- | utility function that surrounds the inner show function with
//-- parentheses when the 'Bool' parameter is 'True'.
//showParen       :: Bool -> ShowS -> ShowS
//showParen b p   =  if b then showChar '(' . p . showChar ')' else p

//showSpace :: ShowS
//showSpace = {-showChar ' '-} \ xs -> ' ' : xs
//\end{code}

//Code specific for characters

//\begin{code}
//-- | Convert a character to a string using only printable characters,
//-- using Haskell source-language escape conventions.  For example:
//--
//-- > showLitChar '\n' s  =  "\\n" ++ s
//--
//showLitChar                :: Char -> ShowS
//showLitChar c s | c > '\DEL' =  showChar '\\' (protectEsc isDec (shows (ord c)) s)
//showLitChar '\DEL'         s =  showString "\\DEL" s
//showLitChar '\\'           s =  showString "\\\\" s
//showLitChar c s | c >= ' '   =  showChar c s
//showLitChar '\a'           s =  showString "\\a" s
//showLitChar '\b'           s =  showString "\\b" s
//showLitChar '\f'           s =  showString "\\f" s
//showLitChar '\n'           s =  showString "\\n" s
//showLitChar '\r'           s =  showString "\\r" s
//showLitChar '\t'           s =  showString "\\t" s
//showLitChar '\v'           s =  showString "\\v" s
//showLitChar '\SO'          s =  protectEsc (== 'H') (showString "\\SO") s
//showLitChar c              s =  showString ('\\' : asciiTab!!ord c) s
//        -- I've done manual eta-expansion here, becuase otherwise it's
//        -- impossible to stop (asciiTab!!ord) getting floated out as an MFE

//isDec :: Char -> Bool
//isDec c = c >= '0' && c <= '9'

//protectEsc :: (Char -> Bool) -> ShowS -> ShowS
//protectEsc p f             = f . cont
//                             where cont s@(c:_) | p c = "\\&" ++ s
//                                   cont s             = s


//asciiTab :: [String]
//asciiTab = -- Using an array drags in the array module.  listArray ('\NUL', ' ')
//           ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
//            "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI", 
//            "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
//            "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US", 
//            "SP"] 
//\end{code}

//Code specific for Ints.

//\begin{code}
//-- | Convert an 'Int' in the range @0@..@15@ to the corresponding single
//-- digit 'Char'.  This function fails on other inputs, and generates
//-- lower-case hexadecimal digits.
//intToDigit :: Int -> Char
//intToDigit (I# i)
//    | i >=# 0#  && i <=#  9# =  unsafeChr (ord '0' `plusInt` I# i)
//    | i >=# 10# && i <=# 15# =  unsafeChr (ord 'a' `minusInt` ten `plusInt` I# i)
//    | otherwise           =  error ("Char.intToDigit: not a digit " ++ show (I# i))

//ten :: Int
//ten = I# 10#

//showSignedInt :: Int -> Int -> ShowS
//showSignedInt (I# p) (I# n) r
//    | n <# 0# && p ># 6# = '(' : itos n (')' : r)
//    | otherwise          = itos n r

//itos :: Int# -> String -> String
//itos n# cs
//    | n# <# 0# =
//        let !(I# minInt#) = minInt in
//        if n# ==# minInt#
//                -- negateInt# minInt overflows, so we can't do that:
//           then '-' : itos' (negateInt# (n# `quotInt#` 10#))
//                             (itos' (negateInt# (n# `remInt#` 10#)) cs)
//           else '-' : itos' (negateInt# n#) cs
//    | otherwise = itos' n# cs
//    where
//    itos' :: Int# -> String -> String
//    itos' x# cs'
//        | x# <# 10#  = C# (chr# (ord# '0'# +# x#)) : cs'
//        | otherwise = case chr# (ord# '0'# +# (x# `remInt#` 10#)) of { c# ->
//                      itos' (x# `quotInt#` 10#) (C# c# : cs') }
//\end{code}



// -------------------------------------------------
// Additional instances
// -------------------------------------------------

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


namespace("GHC_Show", {
     Show       : Show
    //,intToDigit : intToDigit
})﻿/// <reference path="../../../jshaskell/src/Haskell.js" local />
/// <reference path="Base.js" local />

//moved here to simplifiy dependencies (GHC.List)

//data  Maybe a  =  Nothing | Just a
//  deriving (Eq, Ord)
function Maybe(){}
data(Maybe, [["Just", "a"], "Nothing"]);

namespace("GHC_Maybe", {
     Maybe       : Maybe
})﻿/// <reference path="../../../jshaskell/src/Haskell.js" local />
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
})﻿/// <reference path="../../../jshaskell/src/Haskell.js" local />
/// <reference path="Base.js" local />

//{-# OPTIONS_GHC -XNoImplicitPrelude #-}
//{-# OPTIONS -#include "WCsubst.h" #-}
//{-# OPTIONS_HADDOCK hide #-}
//-----------------------------------------------------------------------------
//-- |
//-- Module      :  GHC.Unicode
//-- Copyright   :  (c) The University of Glasgow, 2003
//-- License     :  see libraries/base/LICENSE
//-- 
//-- Maintainer  :  cvs-ghc@haskell.org
//-- Stability   :  internal
//-- Portability :  non-portable (GHC extensions)
//--
//-- Implementations for the character predicates (isLower, isUpper, etc.)
//-- and the conversions (toUpper, toLower).  The implementation uses
//-- libunicode on Unix systems if that is available.
//--
//-----------------------------------------------------------------------------

//-- #hide
//module GHC.Unicode (
//    isAscii, isLatin1, isControl,
//    isAsciiUpper, isAsciiLower,
//    isPrint, isSpace,  isUpper,
//    isLower, isAlpha,  isDigit,
//    isOctDigit, isHexDigit, isAlphaNum,
//    toUpper, toLower, toTitle,
//    wgencat,
//  ) where

//import GHC.Base
//import GHC.Real        (fromIntegral)
//import Foreign.C.Types (CInt)
//import GHC.Num         (fromInteger)

//#include "HsBaseConfig.h"

//-- | Selects the first 128 characters of the Unicode character set,
//-- corresponding to the ASCII character set.
//isAscii                 :: Char -> Bool
//isAscii c               =  c <  '\x80'

//-- | Selects the first 256 characters of the Unicode character set,
//-- corresponding to the ISO 8859-1 (Latin-1) character set.
//isLatin1                :: Char -> Bool
//isLatin1 c              =  c <= '\xff'

//-- | Selects ASCII lower-case letters,
//-- i.e. characters satisfying both 'isAscii' and 'isLower'.
//isAsciiLower :: Char -> Bool
//isAsciiLower c          =  c >= 'a' && c <= 'z'

//-- | Selects ASCII upper-case letters,
//-- i.e. characters satisfying both 'isAscii' and 'isUpper'.
//isAsciiUpper :: Char -> Bool
//isAsciiUpper c          =  c >= 'A' && c <= 'Z'

//-- | Selects control characters, which are the non-printing characters of
//-- the Latin-1 subset of Unicode.
//isControl               :: Char -> Bool

//-- | Selects printable Unicode characters
//-- (letters, numbers, marks, punctuation, symbols and spaces).
//isPrint                 :: Char -> Bool

//-- | Selects white-space characters in the Latin-1 range.
//-- (In Unicode terms, this includes spaces and some control characters.)
//isSpace                 :: Char -> Bool
//-- isSpace includes non-breaking space
//-- Done with explicit equalities both for efficiency, and to avoid a tiresome
//-- recursion with GHC.List elem
//isSpace c               =  c == ' '     ||
//                           c == '\t'    ||
//                           c == '\n'    ||
//                           c == '\r'    ||
//                           c == '\f'    ||
//                           c == '\v'    ||
//                           c == '\xa0'  ||
//                           iswspace (fromIntegral (ord c)) /= 0
function isSpace(c){
    return /^\s$/.test(c);
}

//-- | Selects upper-case or title-case alphabetic Unicode characters (letters).
//-- Title case is used by a small number of letter ligatures like the
//-- single-character form of /Lj/.
//isUpper                 :: Char -> Bool
function isUpper(c){
    return c.toUpperCase() == c;
}

//-- | Selects lower-case alphabetic Unicode characters (letters).
//isLower                 :: Char -> Bool
function isLower(c){
    return c.toLowerCase() == c;
}

//-- | Selects alphabetic Unicode characters (lower-case, upper-case and
//-- title-case letters, plus letters of caseless scripts and modifiers letters).
//-- This function is equivalent to 'Data.Char.isLetter'.
//isAlpha                 :: Char -> Bool
function isAlpha(c){
    return /^\w$/.test(c) && /^\D$/.test(c);
}
//-- | Selects alphabetic or numeric digit Unicode characters.
//--
//-- Note that numeric digits outside the ASCII range are selected by this
//-- function but not by 'isDigit'.  Such digits may be part of identifiers
//-- but are not used by the printer and reader to represent numbers.
//isAlphaNum              :: Char -> Bool
function isAlphaNum(c){
    return /^\w$/.test(c);
}

//-- | Selects ASCII digits, i.e. @\'0\'@..@\'9\'@.
//isDigit                 :: Char -> Bool
//isDigit c               =  c >= '0' && c <= '9'
function isDigit(c){
    return /^\d$/.test(c);
}

//-- | Selects ASCII octal digits, i.e. @\'0\'@..@\'7\'@.
//isOctDigit              :: Char -> Bool
//isOctDigit c            =  c >= '0' && c <= '7'
function isOctDigit(c){
    return /^[0-7]$/.test(c);
}

//-- | Selects ASCII hexadecimal digits,
//-- i.e. @\'0\'@..@\'9\'@, @\'a\'@..@\'f\'@, @\'A\'@..@\'F\'@.
//isHexDigit              :: Char -> Bool
//isHexDigit c            =  isDigit c || c >= 'A' && c <= 'F' ||
//                                        c >= 'a' && c <= 'f'
function isHexDigit(c){
    return /^[0-9A-Fa-f]$/.test(c);
}

//-- | Convert a letter to the corresponding upper-case letter, if any.
//-- Any other character is returned unchanged.
//toUpper                 :: Char -> Char
function toUpper(c){
    return c.toUpperCase();
}

//-- | Convert a letter to the corresponding lower-case letter, if any.
//-- Any other character is returned unchanged.
//toLower                 :: Char -> Char
function toLower(c){
    return c.toLowerCase();
}

//-- | Convert a letter to the corresponding title-case or upper-case
//-- letter, if any.  (Title case differs from upper case only for a small
//-- number of ligature letters.)
//-- Any other character is returned unchanged.
//toTitle                 :: Char -> Char
function toTitle(c){
    return c.toUpperCase();
}







//-- -----------------------------------------------------------------------------
//-- Implementation with the supplied auto-generated Unicode character properties
//-- table (default)

//#if 1

//-- Regardless of the O/S and Library, use the functions contained in WCsubst.c

//isAlpha    c = iswalpha (fromIntegral (ord c)) /= 0
//isAlphaNum c = iswalnum (fromIntegral (ord c)) /= 0
//--isSpace    c = iswspace (fromIntegral (ord c)) /= 0
//isControl  c = iswcntrl (fromIntegral (ord c)) /= 0
//isPrint    c = iswprint (fromIntegral (ord c)) /= 0
//isUpper    c = iswupper (fromIntegral (ord c)) /= 0
//isLower    c = iswlower (fromIntegral (ord c)) /= 0

//toLower c = chr (fromIntegral (towlower (fromIntegral (ord c))))
//toUpper c = chr (fromIntegral (towupper (fromIntegral (ord c))))
//toTitle c = chr (fromIntegral (towtitle (fromIntegral (ord c))))



//-- -----------------------------------------------------------------------------
//-- No libunicode, so fall back to the ASCII-only implementation (never used, indeed)

//#else

//isControl c             =  c < ' ' || c >= '\DEL' && c <= '\x9f'
//isPrint c               =  not (isControl c)

//-- The upper case ISO characters have the multiplication sign dumped
//-- randomly in the middle of the range.  Go figure.
//isUpper c               =  c >= 'A' && c <= 'Z' ||
//                           c >= '\xC0' && c <= '\xD6' ||
//                           c >= '\xD8' && c <= '\xDE'
//-- The lower case ISO characters have the division sign dumped
//-- randomly in the middle of the range.  Go figure.
//isLower c               =  c >= 'a' && c <= 'z' ||
//                           c >= '\xDF' && c <= '\xF6' ||
//                           c >= '\xF8' && c <= '\xFF'

//isAlpha c               =  isLower c || isUpper c
//isAlphaNum c            =  isAlpha c || isDigit c

//-- Case-changing operations

//toUpper c@(C# c#)
//  | isAsciiLower c    = C# (chr# (ord# c# -# 32#))
//  | isAscii c         = c
//    -- fall-through to the slower stuff.
//  | isLower c   && c /= '\xDF' && c /= '\xFF'
//  = unsafeChr (ord c `minusInt` ord 'a' `plusInt` ord 'A')
//  | otherwise
//  = c


//toLower c@(C# c#)
//  | isAsciiUpper c = C# (chr# (ord# c# +# 32#))
//  | isAscii c      = c
//  | isUpper c      = unsafeChr (ord c `minusInt` ord 'A' `plusInt` ord 'a')
//  | otherwise      =  c

//#endif



namespace("GHC_Unicode", {
     isSpace    : isSpace
    ,isUpper    : isUpper
    ,isLower    : isLower
    ,isAlpha    : isAlpha
    ,isAlphaNum : isAlphaNum
    ,isDigit    : isDigit
    ,isHexDigit : isHexDigit
    ,isOctDigit : isOctDigit
    ,toUpper    : toUpper
    ,toLower    : toLower
    ,toTitle    : toTitle
})
}());;(function(){
;var Bool = NS['Haskell'].Bool, isArray = NS['Haskell'].isArray, isDefined = NS['Haskell'].isDefined, slice = NS['Haskell'].slice, imap = NS['Haskell'].imap, ifilter = NS['Haskell'].ifilter, indexOf = NS['Haskell'].indexOf, lastIndexOf = NS['Haskell'].lastIndexOf, isort = NS['Haskell'].isort, range = NS['Haskell'].range, extend = NS['Haskell'].extend, namespace = NS['Haskell'].namespace, typeOf = NS['Haskell'].typeOf, strictEq = NS['Haskell'].strictEq, strictNe = NS['Haskell'].strictNe, unsafeAdd = NS['Haskell'].unsafeAdd, unsafeSub = NS['Haskell'].unsafeSub, unsafeMul = NS['Haskell'].unsafeMul, unsafeDiv = NS['Haskell'].unsafeDiv, lt = NS['Haskell'].lt, le = NS['Haskell'].le, gt = NS['Haskell'].gt, ge = NS['Haskell'].ge, negate = NS['Haskell'].negate, evalThunks = NS['Haskell'].evalThunks, toArray = NS['Haskell'].toArray, curry = NS['Haskell'].curry, error = NS['Haskell'].error, data = NS['Haskell'].data, ADT = NS['Haskell'].ADT, record = NS['Haskell'].record, accessor = NS['Haskell'].accessor, accessors = NS['Haskell'].accessors, typeclass = NS['Haskell'].typeclass, VARARG = NS['Haskell'].VARARG, instance = NS['Haskell'].instance, getInstance = NS['Haskell'].getInstance, asTypeOf = NS['Haskell'].asTypeOf, operators = NS['Haskell'].operators, infix = NS['Haskell'].infix, infixl = NS['Haskell'].infixl, infixr = NS['Haskell'].infixr, op = NS['Haskell'].op, no = NS['Haskell'].no, resolve = NS['Haskell'].resolve, recurse = NS['Haskell'].recurse, Recurse = NS['Haskell'].Recurse, exl = NS['Haskell'].exl, exs = NS['Haskell'].exs, createDo = NS['Haskell'].createDo, Scope = NS['Haskell'].Scope;;var Maybe = NS['GHC_Maybe'].Maybe;;var Unit = NS['GHC_Base'].Unit, Tuple = NS['GHC_Base'].Tuple, Bool = NS['GHC_Base'].Bool, Ordering = NS['GHC_Base'].Ordering, Eq = NS['GHC_Base'].Eq, Ord = NS['GHC_Base'].Ord, andOp = NS['GHC_Base'].andOp, orOp = NS['GHC_Base'].orOp, not = NS['GHC_Base'].not, Functor = NS['GHC_Base'].Functor, Monad = NS['GHC_Base'].Monad, emptyListOf = NS['GHC_Base'].emptyListOf, cons = NS['GHC_Base'].cons, consJoin = NS['GHC_Base'].consJoin, uncons = NS['GHC_Base'].uncons, augment = NS['GHC_Base'].augment, map = NS['GHC_Base'].map, foldr = NS['GHC_Base'].foldr, append = NS['GHC_Base'].append, otherwise = NS['GHC_Base'].otherwise, ord = NS['GHC_Base'].ord, chr = NS['GHC_Base'].chr, eqString = NS['GHC_Base'].eqString, minInt = NS['GHC_Base'].minInt, maxInt = NS['GHC_Base'].maxInt, id = NS['GHC_Base'].id, lazy = NS['GHC_Base'].lazy, assert = NS['GHC_Base'].assert, const_ = NS['GHC_Base'].const_, compose1 = NS['GHC_Base'].compose1, compose = NS['GHC_Base'].compose, flip = NS['GHC_Base'].flip, call = NS['GHC_Base'].call, until = NS['GHC_Base'].until, asTypeOf = NS['GHC_Base'].asTypeOf, elemIndex = NS['GHC_Base'].elemIndex, unsafeCompare = NS['GHC_Base'].unsafeCompare, readHex = NS['GHC_Base'].readHex, readOct = NS['GHC_Base'].readOct, round = NS['GHC_Base'].round, toInteger = NS['GHC_Base'].toInteger, fromInteger = NS['GHC_Base'].fromInteger, fromIntegral = NS['GHC_Base'].fromIntegral;;var map = NS['GHC_List'].map, append = NS['GHC_List'].append, filter = NS['GHC_List'].filter, concat = NS['GHC_List'].concat, head = NS['GHC_List'].head, last = NS['GHC_List'].last, tail = NS['GHC_List'].tail, init = NS['GHC_List'].init, null_ = NS['GHC_List'].null_, length = NS['GHC_List'].length, index = NS['GHC_List'].index, foldl = NS['GHC_List'].foldl, scanl = NS['GHC_List'].scanl, scanl1 = NS['GHC_List'].scanl1, foldr = NS['GHC_List'].foldr, foldr1 = NS['GHC_List'].foldr1, scanr = NS['GHC_List'].scanr, scanr1 = NS['GHC_List'].scanr1, replicate = NS['GHC_List'].replicate, take = NS['GHC_List'].take, drop = NS['GHC_List'].drop, splitAt = NS['GHC_List'].splitAt, takeWhile = NS['GHC_List'].takeWhile, dropWhile = NS['GHC_List'].dropWhile, span = NS['GHC_List'].span, break_ = NS['GHC_List'].break_, reverse = NS['GHC_List'].reverse, and = NS['GHC_List'].and, or = NS['GHC_List'].or, any = NS['GHC_List'].any, all = NS['GHC_List'].all, elem = NS['GHC_List'].elem, notElem = NS['GHC_List'].notElem, lookup = NS['GHC_List'].lookup, concatMap = NS['GHC_List'].concatMap, zip = NS['GHC_List'].zip, zip3 = NS['GHC_List'].zip3, zipWith = NS['GHC_List'].zipWith, zipWith3 = NS['GHC_List'].zipWith3, unzip = NS['GHC_List'].unzip, unzip3 = NS['GHC_List'].unzip3, errorEmptyList = NS['GHC_List'].errorEmptyList;;;var Unit = NS['GHC_Unit'].Unit, unit = NS['GHC_Unit'].unit;;var Tuple = NS['GHC_Tuple'].Tuple, tuple = NS['GHC_Tuple'].tuple;;var Ordering = NS['GHC_Ordering'].Ordering;;var Eq = NS['GHC_Classes'].Eq, Ord = NS['GHC_Classes'].Ord, andOp = NS['GHC_Classes'].andOp, orOp = NS['GHC_Classes'].orOp, not = NS['GHC_Classes'].not;;var Bool = NS['Haskell_Main'].Bool, isArray = NS['Haskell_Main'].isArray, isDefined = NS['Haskell_Main'].isDefined, slice = NS['Haskell_Main'].slice, imap = NS['Haskell_Main'].imap, ifilter = NS['Haskell_Main'].ifilter, indexOf = NS['Haskell_Main'].indexOf, lastIndexOf = NS['Haskell_Main'].lastIndexOf, isort = NS['Haskell_Main'].isort, range = NS['Haskell_Main'].range, extend = NS['Haskell_Main'].extend, namespace = NS['Haskell_Main'].namespace, typeOf = NS['Haskell_Main'].typeOf, strictEq = NS['Haskell_Main'].strictEq, strictNe = NS['Haskell_Main'].strictNe, unsafeAdd = NS['Haskell_Main'].unsafeAdd, unsafeSub = NS['Haskell_Main'].unsafeSub, unsafeMul = NS['Haskell_Main'].unsafeMul, unsafeDiv = NS['Haskell_Main'].unsafeDiv, lt = NS['Haskell_Main'].lt, le = NS['Haskell_Main'].le, gt = NS['Haskell_Main'].gt, ge = NS['Haskell_Main'].ge, negate = NS['Haskell_Main'].negate, evalThunks = NS['Haskell_Main'].evalThunks, toArray = NS['Haskell_Main'].toArray, curry = NS['Haskell_Main'].curry, error = NS['Haskell_Main'].error;;var data = NS['Haskell_DataType'].data, ADT = NS['Haskell_DataType'].ADT, record = NS['Haskell_DataType'].record, accessor = NS['Haskell_DataType'].accessor, accessors = NS['Haskell_DataType'].accessors;;var isSpace = NS['GHC_Unicode'].isSpace, isUpper = NS['GHC_Unicode'].isUpper, isLower = NS['GHC_Unicode'].isLower, isAlpha = NS['GHC_Unicode'].isAlpha, isAlphaNum = NS['GHC_Unicode'].isAlphaNum, isDigit = NS['GHC_Unicode'].isDigit, isHexDigit = NS['GHC_Unicode'].isHexDigit, isOctDigit = NS['GHC_Unicode'].isOctDigit, toUpper = NS['GHC_Unicode'].toUpper, toLower = NS['GHC_Unicode'].toLower, toTitle = NS['GHC_Unicode'].toTitle;;var Show = NS['GHC_Show'].Show;﻿/// <reference path="../../../jshaskell/src/Haskell.js" local />
/// <reference path="../../../base/src/GHC/Maybe.js" local />


//{-# OPTIONS_GHC -XNoImplicitPrelude #-}
//-----------------------------------------------------------------------------
//-- |
//-- Module      :  Data.Maybe
//-- Copyright   :  (c) The University of Glasgow 2001
//-- License     :  BSD-style (see the file libraries/base/LICENSE)
//-- 
//-- Maintainer  :  libraries@haskell.org
//-- Stability   :  stable
//-- Portability :  portable
//--
//-- The Maybe type, and associated operations.
//--
//-----------------------------------------------------------------------------

//module Data.Maybe
//   (
//     Maybe(Nothing,Just)-- instance of: Eq, Ord, Show, Read,
//                        --              Functor, Monad, MonadPlus

//   , maybe              -- :: b -> (a -> b) -> Maybe a -> b

//   , isJust             -- :: Maybe a -> Bool
//   , isNothing          -- :: Maybe a -> Bool
//   , fromJust           -- :: Maybe a -> a
//   , fromMaybe          -- :: a -> Maybe a -> a
//   , listToMaybe        -- :: [a] -> Maybe a
//   , maybeToList        -- :: Maybe a -> [a]
//   , catMaybes          -- :: [Maybe a] -> [a]
//   , mapMaybe           -- :: (a -> Maybe b) -> [a] -> [b]
//   ) where

//#ifdef __GLASGOW_HASKELL__
//import GHC.Base
//#endif

//#ifdef __NHC__
//import Prelude
//import Prelude (Maybe(..), maybe)
//import Maybe
//    ( isJust
//    , isNothing
//    , fromJust
//    , fromMaybe
//    , listToMaybe
//    , maybeToList
//    , catMaybes
//    , mapMaybe
//    )
//#else

//#ifndef __HUGS__
//-- ---------------------------------------------------------------------------
//-- The Maybe type, and instances

//-- | The 'Maybe' type encapsulates an optional value.  A value of type
//-- @'Maybe' a@ either contains a value of type @a@ (represented as @'Just' a@), 
//-- or it is empty (represented as 'Nothing').  Using 'Maybe' is a good way to 
//-- deal with errors or exceptional cases without resorting to drastic
//-- measures such as 'error'.
//--
//-- The 'Maybe' type is also a monad.  It is a simple kind of error
//-- monad, where all errors are represented by 'Nothing'.  A richer
//-- error monad can be built using the 'Data.Either.Either' type.

//data  Maybe a  =  Nothing | Just a
//  deriving (Eq, Ord)

//declared in GHC.Maybe
//function Maybe(){}
//data(Maybe, [["Just", "a"], "Nothing"]);

//instance  Functor Maybe  where
//    fmap _ Nothing       = Nothing
//    fmap f (Just a)      = Just (f a)
instance(Functor, Maybe, {
    fmap: function(f, m){
        return  m.Nothing ? Maybe.Nothing : 
                m.Just ? Maybe.Just(f(m[0])) : error("Functor.Maybe.fmap")
    }
})

//instance  Monad Maybe  where
//    (Just x) >>= k      = k x
//    Nothing  >>= _      = Nothing

//    (Just _) >>  k      = k
//    Nothing  >>  _      = Nothing

//    return              = Just
//    fail _              = Nothing
instance(Monad, Maybe, {
    ">>=" : function(m, k){
        return  m.Just    ? k(m[0]) :
                m.Nothing ? Maybe.Nothing :
                error("Monad.Maybe.>>=");
    },
    ">>" : function(m, k){
        return  m.Just    ? k :
                m.Nothing ? Maybe.Nothing :
                error("Monad.Maybe.>>");
    },
    return_ : Maybe.Just,
    fail    : Maybe.Nothing
})

//-- ---------------------------------------------------------------------------
//-- Functions over Maybe

//-- | The 'maybe' function takes a default value, a function, and a 'Maybe'
//-- value.  If the 'Maybe' value is 'Nothing', the function returns the
//-- default value.  Otherwise, it applies the function to the value inside
//-- the 'Just' and returns the result.
//maybe :: b -> (a -> b) -> Maybe a -> b
//maybe n _ Nothing  = n
//maybe _ f (Just x) = f x
//#endif  /* __HUGS__ */
function maybe(n, f, m){
    if(m.Nothing)
        return n;
    if(m.Just)
        return f(m[0]);
}

//-- | The 'isJust' function returns 'True' iff its argument is of the
//-- form @Just _@.
//isJust         :: Maybe a -> Bool
//isJust Nothing = False
//isJust _       = True
function isJust(a){
    return !!a.Just;
}

//-- | The 'isNothing' function returns 'True' iff its argument is 'Nothing'.
//isNothing         :: Maybe a -> Bool
//isNothing Nothing = True
//isNothing _       = False
function isNothing(a){
    return !!a.Nothing;
}

//-- | The 'fromJust' function extracts the element out of a 'Just' and
//-- throws an error if its argument is 'Nothing'.
//fromJust          :: Maybe a -> a
//fromJust Nothing  = error "Maybe.fromJust: Nothing" -- yuck
//fromJust (Just x) = x
function fromJust(a){
    return a.Nothing ? error("Maybe.fromJust: Nothing") : 
           a.Just ? a[0] : error();
}

//-- | The 'fromMaybe' function takes a default value and and 'Maybe'
//-- value.  If the 'Maybe' is 'Nothing', it returns the default values;
//-- otherwise, it returns the value contained in the 'Maybe'.
//fromMaybe     :: a -> Maybe a -> a
//fromMaybe d x = case x of {Nothing -> d;Just v  -> v}
function fromMaybe(d, x){
    return x.Nothing ? d : x.Just ? x[0] : error(fromMaybe);
}

//-- | The 'maybeToList' function returns an empty list when given
//-- 'Nothing' or a singleton list when not given 'Nothing'.
//maybeToList            :: Maybe a -> [a]
//maybeToList  Nothing   = []
//maybeToList  (Just x)  = [x]
function maybeToList(m){
    return m.Nothing ? [] : m.Just ? [m[0]] : error(maybeToList);
}

//-- | The 'listToMaybe' function returns 'Nothing' on an empty list
//-- or @'Just' a@ where @a@ is the first element of the list.
//listToMaybe           :: [a] -> Maybe a
//listToMaybe []        =  Nothing
//listToMaybe (a:_)     =  Just a
function listToMaybe(l){
    if(!l.length)
        return emptyListOf(l);
    return Maybe.Just(l[0]);
}

//-- | The 'catMaybes' function takes a list of 'Maybe's and returns
//-- a list of all the 'Just' values. 
//catMaybes              :: [Maybe a] -> [a]
//catMaybes ls = [x | Just x <- ls]
function catMaybes(ls){
    var acc = [];
    for(var i = 0, l = ls.length; i < l; ++i)
        if(ls[i].Just)
            acc.push(ls[i][0]);
    return acc;
}

//-- | The 'mapMaybe' function is a version of 'map' which can throw
//-- out elements.  In particular, the functional argument returns
//-- something of type @'Maybe' b@.  If this is 'Nothing', no element
//-- is added on to the result list.  If it just @'Just' b@, then @b@ is
//-- included in the result list.
//mapMaybe          :: (a -> Maybe b) -> [a] -> [b]
//mapMaybe _ []     = []
//mapMaybe f (x:xs) =
// let rs = mapMaybe f xs in
// case f x of
//  Nothing -> rs
//  Just r  -> r:rs
function mapMaybe(f, xs){
    var a = uncons(xs),
        rs = mapMaybe(f, a.tail),
        fx = f(a.head);
    return  fx.Nothing ? rs :
            fx.Just    ? cons(r, rs) :
            error(mapMaybe);
}

//#endif /* else not __NHC__ */


namespace("Data_Maybe", {
     Maybe       : Maybe        //instance of: Eq, Ord, Show, Read, 
                                //        Functor, Monad, MonadPlus
                            
    ,maybe       : maybe        //:: b -> (a -> b) -> Maybe a -> b
                            
    ,isJust      : isJust       //:: Maybe a -> Bool
    ,isNothing   : isNothing    //:: Maybe a -> Bool
    ,fromJust    : fromJust     //:: Maybe a -> a
    ,fromMaybe   : fromMaybe    //:: a -> Maybe a -> a
    ,listToMaybe : listToMaybe  //:: [a] -> Maybe a
    ,maybeToList : maybeToList  //:: Maybe a -> [a]
    ,catMaybes   : catMaybes    //:: [Maybe a] -> [a]
    ,mapMaybe    : mapMaybe     //:: (a -> Maybe b) -> [a] -> [b]
})﻿/// <reference path="../../../jshaskell/src/Haskell.js" local />
/// <reference path="../../../base/src/GHC/Base.js" local />
/// <reference path="../../../base/src/GHC/List.js" local />
/// <reference path="../../../base/src/Data/Maybe.js" local />



//{-# OPTIONS_GHC -XNoImplicitPrelude #-}
//-----------------------------------------------------------------------------
//-- |
//-- Module      :  Data.List
//-- Copyright   :  (c) The University of Glasgow 2001
//-- License     :  BSD-style (see the file libraries/base/LICENSE)
//-- 
//-- Maintainer  :  libraries@haskell.org
//-- Stability   :  stable
//-- Portability :  portable
//--
//-- Operations on lists.
//--
//-----------------------------------------------------------------------------

//module Data.List
//   (
//#ifdef __NHC__
//     [] (..)
//   ,
//#endif

//   -- * Basic functions

//     (++)              -- :: [a] -> [a] -> [a]
//   , head              -- :: [a] -> a
//   , last              -- :: [a] -> a
//   , tail              -- :: [a] -> [a]
//   , init              -- :: [a] -> [a]
//   , null              -- :: [a] -> Bool
//   , length            -- :: [a] -> Int

//   -- * List transformations
//   , map               -- :: (a -> b) -> [a] -> [b]
//   , reverse           -- :: [a] -> [a]

//   , intersperse       -- :: a -> [a] -> [a]
//   , intercalate       -- :: [a] -> [[a]] -> [a]
//   , transpose         -- :: [[a]] -> [[a]]
//   
//   , subsequences      -- :: [a] -> [[a]]
//   , permutations      -- :: [a] -> [[a]]

//   -- * Reducing lists (folds)

//   , foldl             -- :: (a -> b -> a) -> a -> [b] -> a
//   , foldl'            -- :: (a -> b -> a) -> a -> [b] -> a
//   , foldl1            -- :: (a -> a -> a) -> [a] -> a
//   , foldl1'           -- :: (a -> a -> a) -> [a] -> a
//   , foldr             -- :: (a -> b -> b) -> b -> [a] -> b
//   , foldr1            -- :: (a -> a -> a) -> [a] -> a

//   -- ** Special folds

//   , concat            -- :: [[a]] -> [a]
//   , concatMap         -- :: (a -> [b]) -> [a] -> [b]
//   , and               -- :: [Bool] -> Bool
//   , or                -- :: [Bool] -> Bool
//   , any               -- :: (a -> Bool) -> [a] -> Bool
//   , all               -- :: (a -> Bool) -> [a] -> Bool
//   , sum               -- :: (Num a) => [a] -> a
//   , product           -- :: (Num a) => [a] -> a
//   , maximum           -- :: (Ord a) => [a] -> a
//   , minimum           -- :: (Ord a) => [a] -> a

//   -- * Building lists

//   -- ** Scans
//   , scanl             -- :: (a -> b -> a) -> a -> [b] -> [a]
//   , scanl1            -- :: (a -> a -> a) -> [a] -> [a]
//   , scanr             -- :: (a -> b -> b) -> b -> [a] -> [b]
//   , scanr1            -- :: (a -> a -> a) -> [a] -> [a]

//   -- ** Accumulating maps
//   , mapAccumL         -- :: (a -> b -> (a,c)) -> a -> [b] -> (a,[c])
//   , mapAccumR         -- :: (a -> b -> (a,c)) -> a -> [b] -> (a,[c])

//   -- ** Infinite lists
//   , iterate           -- :: (a -> a) -> a -> [a]
//   , repeat            -- :: a -> [a]
//   , replicate         -- :: Int -> a -> [a]
//   , cycle             -- :: [a] -> [a]

//   -- ** Unfolding
//   , unfoldr           -- :: (b -> Maybe (a, b)) -> b -> [a]

//   -- * Sublists

//   -- ** Extracting sublists
//   , take              -- :: Int -> [a] -> [a]
//   , drop              -- :: Int -> [a] -> [a]
//   , splitAt           -- :: Int -> [a] -> ([a], [a])

//   , takeWhile         -- :: (a -> Bool) -> [a] -> [a]
//   , dropWhile         -- :: (a -> Bool) -> [a] -> [a]
//   , span              -- :: (a -> Bool) -> [a] -> ([a], [a])
//   , break             -- :: (a -> Bool) -> [a] -> ([a], [a])

//   , stripPrefix       -- :: Eq a => [a] -> [a] -> Maybe [a]

//   , group             -- :: Eq a => [a] -> [[a]]

//   , inits             -- :: [a] -> [[a]]
//   , tails             -- :: [a] -> [[a]]

//   -- ** Predicates
//   , isPrefixOf        -- :: (Eq a) => [a] -> [a] -> Bool
//   , isSuffixOf        -- :: (Eq a) => [a] -> [a] -> Bool
//   , isInfixOf         -- :: (Eq a) => [a] -> [a] -> Bool

//   -- * Searching lists

//   -- ** Searching by equality
//   , elem              -- :: a -> [a] -> Bool
//   , notElem           -- :: a -> [a] -> Bool
//   , lookup            -- :: (Eq a) => a -> [(a,b)] -> Maybe b

//   -- ** Searching with a predicate
//   , find              -- :: (a -> Bool) -> [a] -> Maybe a
//   , filter            -- :: (a -> Bool) -> [a] -> [a]
//   , partition         -- :: (a -> Bool) -> [a] -> ([a], [a])

//   -- * Indexing lists
//   -- | These functions treat a list @xs@ as a indexed collection,
//   -- with indices ranging from 0 to @'length' xs - 1@.

//   , (!!)              -- :: [a] -> Int -> a

//   , elemIndex         -- :: (Eq a) => a -> [a] -> Maybe Int
//   , elemIndices       -- :: (Eq a) => a -> [a] -> [Int]

//   , findIndex         -- :: (a -> Bool) -> [a] -> Maybe Int
//   , findIndices       -- :: (a -> Bool) -> [a] -> [Int]

//   -- * Zipping and unzipping lists

//   , zip               -- :: [a] -> [b] -> [(a,b)]
//   , zip3
//   , zip4, zip5, zip6, zip7

//   , zipWith           -- :: (a -> b -> c) -> [a] -> [b] -> [c]
//   , zipWith3
//   , zipWith4, zipWith5, zipWith6, zipWith7

//   , unzip             -- :: [(a,b)] -> ([a],[b])
//   , unzip3
//   , unzip4, unzip5, unzip6, unzip7

//   -- * Special lists

//   -- ** Functions on strings
//   , lines             -- :: String   -> [String]
//   , words             -- :: String   -> [String]
//   , unlines           -- :: [String] -> String
//   , unwords           -- :: [String] -> String

//   -- ** \"Set\" operations

//   , nub               -- :: (Eq a) => [a] -> [a]

//   , delete            -- :: (Eq a) => a -> [a] -> [a]
//   , (\\)              -- :: (Eq a) => [a] -> [a] -> [a]

//   , union             -- :: (Eq a) => [a] -> [a] -> [a]
//   , intersect         -- :: (Eq a) => [a] -> [a] -> [a]

//   -- ** Ordered lists
//   , sort              -- :: (Ord a) => [a] -> [a]
//   , insert            -- :: (Ord a) => a -> [a] -> [a]

//   -- * Generalized functions

//   -- ** The \"@By@\" operations
//   -- | By convention, overloaded functions have a non-overloaded
//   -- counterpart whose name is suffixed with \`@By@\'.
//   --
//   -- It is often convenient to use these functions together with
//   -- 'Data.Function.on', for instance @'sortBy' ('compare'
//   -- \`on\` 'fst')@.

//   -- *** User-supplied equality (replacing an @Eq@ context)
//   -- | The predicate is assumed to define an equivalence.
//   , nubBy             -- :: (a -> a -> Bool) -> [a] -> [a]
//   , deleteBy          -- :: (a -> a -> Bool) -> a -> [a] -> [a]
//   , deleteFirstsBy    -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
//   , unionBy           -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
//   , intersectBy       -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
//   , groupBy           -- :: (a -> a -> Bool) -> [a] -> [[a]]

//   -- *** User-supplied comparison (replacing an @Ord@ context)
//   -- | The function is assumed to define a total ordering.
//   , sortBy            -- :: (a -> a -> Ordering) -> [a] -> [a]
//   , insertBy          -- :: (a -> a -> Ordering) -> a -> [a] -> [a]
//   , maximumBy         -- :: (a -> a -> Ordering) -> [a] -> a
//   , minimumBy         -- :: (a -> a -> Ordering) -> [a] -> a

//   -- ** The \"@generic@\" operations
//   -- | The prefix \`@generic@\' indicates an overloaded function that
//   -- is a generalized version of a "Prelude" function.

//   , genericLength     -- :: (Integral a) => [b] -> a
//   , genericTake       -- :: (Integral a) => a -> [b] -> [b]
//   , genericDrop       -- :: (Integral a) => a -> [b] -> [b]
//   , genericSplitAt    -- :: (Integral a) => a -> [b] -> ([b], [b])
//   , genericIndex      -- :: (Integral a) => [b] -> a -> b
//   , genericReplicate  -- :: (Integral a) => a -> b -> [b]

//   ) where

//#ifdef __NHC__
//import Prelude
//#endif

//import Data.Maybe
//import Data.Char        ( isSpace )      // \s is used instead

//#ifdef __GLASGOW_HASKELL__
//import GHC.Num
//import GHC.Real
//import GHC.List
//import GHC.Base
//#endif

//infix 5 \\ -- comment to fool cpp

//-- -----------------------------------------------------------------------------
//-- List functions

//-- | The 'stripPrefix' function drops the given prefix from a list.
//-- It returns 'Nothing' if the list did not start with the prefix
//-- given, or 'Just' the list after the prefix, if it does.
//--
//-- > stripPrefix "foo" "foobar" -> Just "bar"
//-- > stripPrefix "foo" "foo" -> Just ""
//-- > stripPrefix "foo" "barfoo" -> Nothing
//-- > stripPrefix "foo" "barfoobaz" -> Nothing
//stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
//stripPrefix [] ys = Just ys
//stripPrefix (x:xs) (y:ys)
// | x == y = stripPrefix xs ys
//stripPrefix _ _ = Nothing
function stripPrefix(xs, ys){
    if(!xs.length)
        return Maybe.Just(ys);

    var a = uncons(xs),
        b = uncons(ys);
    if(a.head === b.head)
        return stripPrefix(a.tail, b.tail);

    return Maybe.Nothing;
}


//-- | The 'elemIndex' function returns the index of the first element
//-- in the given list which is equal (by '==') to the query element,
//-- or 'Nothing' if there is no such element.
//elemIndex       :: Eq a => a -> [a] -> Maybe Int
//elemIndex x     = findIndex (x==)
function elemIndex(x, xs){
    return findIndex(function(e){ return Eq.eq(x) }, xs);
}

//-- | The 'elemIndices' function extends 'elemIndex', by returning the
//-- indices of all elements equal to the query element, in ascending order.
//elemIndices     :: Eq a => a -> [a] -> [Int]
//elemIndices x   = findIndices (x==)
function elemIndices(x, xs){
    return findIndices(function(e){ return Eq.eq(x) }, xs);
}

//-- | The 'find' function takes a predicate and a list and returns the
//-- first element in the list matching the predicate, or 'Nothing' if
//-- there is no such element.
//find            :: (a -> Bool) -> [a] -> Maybe a
//find p          = listToMaybe . filter p
function find(p, a){
    return listToMaybe(filter(p, a));
}

//-- | The 'findIndex' function takes a predicate and a list and returns
//-- the index of the first element in the list satisfying the predicate,
//-- or 'Nothing' if there is no such element.
//findIndex       :: (a -> Bool) -> [a] -> Maybe Int
//findIndex p     = listToMaybe . findIndices p
function findIndex(p, a){
    return listToMaybe(findIndices(p, a));
}

//-- | The 'findIndices' function extends 'findIndex', by returning the
//-- indices of all elements satisfying the predicate, in ascending order.
//findIndices      :: (a -> Bool) -> [a] -> [Int]

//#if defined(USE_REPORT_PRELUDE) || !defined(__GLASGOW_HASKELL__)
//findIndices p xs = [ i | (x,i) <- zip xs [0..], p x]
//#else
//-- Efficient definition
//findIndices p ls = loop 0# ls
//                 where
//                   loop _ [] = []
//                   loop n (x:xs) | p x       = I# n : loop (n +# 1#) xs
//                                 | otherwise = loop (n +# 1#) xs
//#endif  /* USE_REPORT_PRELUDE */
function findIndices(p, ls){

    function loop(n, xs){
        if(!ls.length)
            return ls;

        return p(x) ? cons(n, loop(n + 1, xs)) : loop(n + 1, xs);
    }

    return loop(0, ls);
}


//-- | The 'isPrefixOf' function takes two lists and returns 'True'
//-- iff the first list is a prefix of the second.
//isPrefixOf              :: (Eq a) => [a] -> [a] -> Bool
//isPrefixOf [] _         =  True
//isPrefixOf _  []        =  False
//isPrefixOf (x:xs) (y:ys)=  x == y && isPrefixOf xs ys
function isPrefixOf(xs, ys){
    if(!xs.length)
        return true;

    if(!ys.length)
        return false;

    var a = uncons(xs),
        b = uncons(ys);
    return (a.head === b.head) && isPrefixOf(a.tail, b.tail);
}



//-- | The 'isSuffixOf' function takes two lists and returns 'True'
//-- iff the first list is a suffix of the second.
//-- Both lists must be finite.
//isSuffixOf              :: (Eq a) => [a] -> [a] -> Bool
//isSuffixOf x y          =  reverse x `isPrefixOf` reverse y
function isSuffixOf(xs, ys){
    return isPrefixOf(reverse(xs), reverse(ys));
}

//-- | The 'isInfixOf' function takes two lists and returns 'True'
//-- iff the first list is contained, wholly and intact,
//-- anywhere within the second.
//--
//-- Example:
//--
//-- >isInfixOf "Haskell" "I really like Haskell." -> True
//-- >isInfixOf "Ial" "I really like Haskell." -> False
//isInfixOf               :: (Eq a) => [a] -> [a] -> Bool
//isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
function isInfixOf(needle, haystack){
    return any(function(xs){ return isPrefixOf(needle, xs) }, tails(haystack));
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
function nub(l){
    function nub_(xs, ls){        
        var a = uncons(xs);
        
        return !xs.length ? emptyListOf(xs) :
                elem(a.head, ls) ? nub_(a.tail, ls) : 
                cons(a.head, nub_(a.tail, cons(a.head, ls)) );
    }
    return nub_(l, emptyListOf(l));
}

//-- | The 'nubBy' function behaves just like 'nub', except it uses a
//-- user-supplied equality predicate instead of the overloaded '=='
//-- function.
//nubBy                   :: (a -> a -> Bool) -> [a] -> [a]
//#ifdef USE_REPORT_PRELUDE
//nubBy eq []             =  []
//nubBy eq (x:xs)         =  x : nubBy eq (filter (\ y -> not (eq x y)) xs)
//#else
//nubBy eq l              = nubBy' l []
//  where
//    nubBy' [] _         = []
//    nubBy' (y:ys) xs
//       | elem_by eq y xs = nubBy' ys xs
//       | otherwise       = y : nubBy' ys (y:xs)
function nubBy(eq, l){
    function nubBy_(ys, xs){        
        var a = uncons(ys);
        
        return !ys.length ? emptyListOf(ys) :
                elem_by(eq, a.head, xs) ? nubBy_(a.tail, xs) : 
                cons(a.head, nubBy_(a.tail, cons(a.head, xs)) );
    }
    return nub_(l, emptyListOf(l));
}

//-- Not exported:
//-- Note that we keep the call to `eq` with arguments in the
//-- same order as in the reference implementation
//-- 'xs' is the list of things we've seen so far, 
//-- 'y' is the potential new element
//elem_by :: (a -> a -> Bool) -> a -> [a] -> Bool
//elem_by _  _ []         =  False
//elem_by eq y (x:xs)     =  y `eq` x || elem_by eq y xs
//#endif
function elem_by(eq, y, xs){
    if(!xs.length)
        return false;
    var a = uncons(xs);
    return eq(y, a.head) || elem_by(eq, y, a.tail);
}

//-- | 'delete' @x@ removes the first occurrence of @x@ from its list argument.
//-- For example,
//--
//-- > delete 'a' "banana" == "bnana"
//--
//-- It is a special case of 'deleteBy', which allows the programmer to
//-- supply their own equality test.

//delete                  :: (Eq a) => a -> [a] -> [a]
//delete                  =  deleteBy (==)
function delete_(x, xs){
    return deleteBy(Eq.eq, x, xs);
}

//-- | The 'deleteBy' function behaves like 'delete', but takes a
//-- user-supplied equality predicate.
//deleteBy                :: (a -> a -> Bool) -> a -> [a] -> [a]
//deleteBy _  _ []        = []
//deleteBy eq x (y:ys)    = if x `eq` y then ys else y : deleteBy eq x ys
function deleteBy(eq, x, ys){
    if(!ys.length)
        return emptyListOf(ys);
    var a = uncons(ys);
    return eq(x, a.head) ? a.tail : cons(a.head, deleteBy(eq, x, a.tail));
}

//-- | The '\\' function is list difference ((non-associative).
//-- In the result of @xs@ '\\' @ys@, the first occurrence of each element of
//-- @ys@ in turn (if any) has been removed from @xs@.  Thus
//--
//-- > (xs ++ ys) \\ xs == ys.
//--
//-- It is a special case of 'deleteFirstsBy', which allows the programmer
//-- to supply their own equality test.

//(\\)                    :: (Eq a) => [a] -> [a] -> [a]
//(\\)                    =  foldl (flip delete)
function difference(xs, ys){
    return foldl(flip(delete_), xs, ys)
}

//-- | The 'union' function returns the list union of the two lists.
//-- For example,
//--
//-- > "dog" `union` "cow" == "dogcw"
//--
//-- Duplicates, and elements of the first list, are removed from the
//-- the second list, but if the first list contains duplicates, so will
//-- the result.
//-- It is a special case of 'unionBy', which allows the programmer to supply
//-- their own equality test.

//union                   :: (Eq a) => [a] -> [a] -> [a]
//union                   = unionBy (==)
function union(xs, ys){
    return unionBy(Eq.eq, xs, ys);
}

//-- | The 'unionBy' function is the non-overloaded version of 'union'.
//unionBy                 :: (a -> a -> Bool) -> [a] -> [a] -> [a]
//unionBy eq xs ys        =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs
function unionBy(eq, xs, ys){
    return append(xs, foldl(function(ys, x){ return deleteBy(eq, x, ys) }, nubBy(eq, ys), xs));
}

//-- | The 'intersect' function takes the list intersection of two lists.
//-- For example,
//--
//-- > [1,2,3,4] `intersect` [2,4,6,8] == [2,4]
//--
//-- If the first list contains duplicates, so will the result.
//--
//-- > [1,2,2,3,4] `intersect` [6,4,4,2] == [2,2,4]
//--
//-- It is a special case of 'intersectBy', which allows the programmer to
//-- supply their own equality test.

//intersect               :: (Eq a) => [a] -> [a] -> [a]
//intersect               =  intersectBy (==)
function intersect(xs, ys){
    return intersectBy(Eq.eq, xs, ys)
}

//-- | The 'intersectBy' function is the non-overloaded version of 'intersect'.
//intersectBy             :: (a -> a -> Bool) -> [a] -> [a] -> [a]
//intersectBy eq xs ys    =  [x | x <- xs, any (eq x) ys]
function intersectBy(eq, xs, ys){
    return filter(function(x){
        return any(function(y){ return eq(x, y) }, ys);
    }, xs);
}

//-- | The 'intersperse' function takes an element and a list and
//-- \`intersperses\' that element between the elements of the list.
//-- For example,
//--
//-- > intersperse ',' "abcde" == "a,b,c,d,e"

//intersperse             :: a -> [a] -> [a]
//intersperse _   []      = []
//intersperse _   [x]     = [x]
//intersperse sep (x:xs)  = x : sep : intersperse sep xs
function intersperse(sep, xs){
    if(!xs.length)
        return emptyListOf(xs);
    if(xs.length == 1)
        return cons(head(xs), emptyListOf(xs));
    var a = uncons(xs);
    return cons(a.head, cons(sep, intersperse(sep, a.tail)));
}

//-- | 'intercalate' @xs xss@ is equivalent to @('concat' ('intersperse' xs xss))@.
//-- It inserts the list @xs@ in between the lists in @xss@ and concatenates the
//-- result.
//intercalate :: [a] -> [[a]] -> [a]
//intercalate xs xss = concat (intersperse xs xss)
function intercalate(xs, xss){
    return concat(intersperse(xs, xss));
}

//-- | The 'transpose' function transposes the rows and columns of its argument.
//-- For example,
//--
//-- > transpose [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]]

//transpose               :: [[a]] -> [[a]]
//transpose []             = []
//transpose ([]   : xss)   = transpose xss
//transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])
function transpose(xss){
    if(!xss.length)
        return emptyListOf(xss);
    var a = uncons(xss);
    if(!a.head.length)
        return transpose(a.tail);
    var b = uncons(a.head),
        x = b.head,
        xs = b.tail;
    xss = a.tail;
    return uncons( uncons(x, map(head, xss)) , transpose(uncons(xs,  map(tail, xss) )) );
}

//-- | The 'partition' function takes a predicate a list and returns
//-- the pair of lists of elements which do and do not satisfy the
//-- predicate, respectively; i.e.,
//--
//-- > partition p xs == (filter p xs, filter (not . p) xs)

//partition               :: (a -> Bool) -> [a] -> ([a],[a])
//{-# INLINE partition #-}
//partition p xs = foldr (select p) ([],[]) xs
function partition(p, xs){
    return foldr(function(x, t){ return select(p, x, t) }, [emptyListOf(xs), emptyListOf(xs)], xs);
}

//select :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
//select p x ~(ts,fs) | p x       = (x:ts,fs)
//                    | otherwise = (ts, x:fs)
function select(p, x, t){
    var ts = t[0], fs = t[1];
    return p(x) ? [cons(x, ts), fs] : [ts, cons(x, fs)];
}

//-- | The 'mapAccumL' function behaves like a combination of 'map' and
//-- 'foldl'; it applies a function to each element of a list, passing
//-- an accumulating parameter from left to right, and returning a final
//-- value of this accumulator together with the new list.
//mapAccumL :: (acc -> x -> (acc, y)) -- Function of elt of input list
//                                    -- and accumulator, returning new
//                                    -- accumulator and elt of result list
//          -> acc            -- Initial accumulator 
//          -> [x]            -- Input list
//          -> (acc, [y])     -- Final accumulator and result list
//mapAccumL _ s []        =  (s, [])
//mapAccumL f s (x:xs)    =  (s'',y:ys)
//                           where (s', y ) = f s x
//                                 (s'',ys) = mapAccumL f s' xs
function mapAccumL(f, s, xs){
    if(!xs.length)
        return [s, emptyListOf(xs)];

    var a  = uncons(xs),
        b  = f(s, x),
        s_ = b[0],
        y  = b[1],
        c  = mapAccumL(f, s_, a.tail),
        s__= c[0],
        ys = c[1];

    return [s__, cons(y, ys)];
}

//-- | The 'mapAccumR' function behaves like a combination of 'map' and
//-- 'foldr'; it applies a function to each element of a list, passing
//-- an accumulating parameter from right to left, and returning a final
//-- value of this accumulator together with the new list.
//mapAccumR :: (acc -> x -> (acc, y))     -- Function of elt of input list
//                                        -- and accumulator, returning new
//                                        -- accumulator and elt of result list
//            -> acc              -- Initial accumulator
//            -> [x]              -- Input list
//            -> (acc, [y])               -- Final accumulator and result list
//mapAccumR _ s []        =  (s, [])
//mapAccumR f s (x:xs)    =  (s'', y:ys)
//                           where (s'',y ) = f s' x
//                                 (s', ys) = mapAccumR f s xs
function mapAccumR(f, s, xs){
    if(!xs.length)
        return [s, emptyListOf(xs)];

    var a  = uncons(xs),
        b  = mapAccumR(f, s, a.tail),
        s_ = b[0],
        ys = b[1],
        c  = f(s_, x),
        s__= c[0],
        y  = c[1];

    return [s__, cons(y, ys)];
}

//-- | The 'insert' function takes an element and a list and inserts the
//-- element into the list at the last position where it is still less
//-- than or equal to the next element.  In particular, if the list
//-- is sorted before the call, the result will also be sorted.
//-- It is a special case of 'insertBy', which allows the programmer to
//-- supply their own comparison function.
//insert :: Ord a => a -> [a] -> [a]
//insert e ls = insertBy (compare) e ls
function insert(e, ls){
    var inst = getInstance(Ord, typeOf(e));
    return insertBy(inst.compare, e, ls);
}

//-- | The non-overloaded version of 'insert'.
//insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
//insertBy _   x [] = [x]
//insertBy cmp x ys@(y:ys')
// = case cmp x y of
//     GT -> y : insertBy cmp x ys'
//     _  -> x : ys
function insertBy(cmp, x, ys){
    if(!ys.length)
        return cons(x, emptyListOf(ys));

    var a = uncons(ys);
    return cmp(x, a.head).GT ?
            cons(a.head, insertBy(cmp, x, a.tail)) :
            cons(x, a.tail);
}

//#ifdef __GLASGOW_HASKELL__

//-- | 'maximum' returns the maximum value from a list,
//-- which must be non-empty, finite, and of an ordered type.
//-- It is a special case of 'Data.List.maximumBy', which allows the
//-- programmer to supply their own comparison function.
//maximum                 :: (Ord a) => [a] -> a
//maximum []              =  errorEmptyList "maximum"
//maximum xs              =  foldl1 max xs
function maximum(xs){
    if(!xs.length)
        return errorEmptyList("maximum");

    var inst = getInstance(Ord, typeOf(head(xs)));

    return foldl1(inst.max, xs);
}

//-- | 'minimum' returns the minimum value from a list,
//-- which must be non-empty, finite, and of an ordered type.
//-- It is a special case of 'Data.List.minimumBy', which allows the
//-- programmer to supply their own comparison function.
//minimum                 :: (Ord a) => [a] -> a
//minimum []              =  errorEmptyList "minimum"
//minimum xs              =  foldl1 min xs
function minimum(xs){
    if(!xs.length)
        return errorEmptyList("minimum");
    
    var inst = getInstance(Ord, typeOf(head(xs)));

    return foldl1(inst.min, xs);
}


//#endif /* __GLASGOW_HASKELL__ */

//-- | The 'maximumBy' function takes a comparison function and a list
//-- and returns the greatest element of the list by the comparison function.
//-- The list must be finite and non-empty.
//maximumBy               :: (a -> a -> Ordering) -> [a] -> a
//maximumBy _ []          =  error "List.maximumBy: empty list"
//maximumBy cmp xs        =  foldl1 maxBy xs
//                        where
//                           maxBy x y = case cmp x y of
//                                       GT -> x
//                                       _  -> y
function maximumBy(cmp, xs){
    if(!xs.length)
        return errorEmptyList("maximumBy");

    function maxBy(x, y){
        return cmp(x, y).GT ? x : y;
    }

    return foldl1(maxBy, xs);
}

//-- | The 'minimumBy' function takes a comparison function and a list
//-- and returns the least element of the list by the comparison function.
//-- The list must be finite and non-empty.
//minimumBy               :: (a -> a -> Ordering) -> [a] -> a
//minimumBy _ []          =  error "List.minimumBy: empty list"
//minimumBy cmp xs        =  foldl1 minBy xs
//                        where
//                           minBy x y = case cmp x y of
//                                       GT -> y
//                                       _  -> x
function minimumBy(cmp, xs){
    if(!xs.length)
        return errorEmptyList("minimumBy");
    
    function minBy(x, y){
        return cmp(x, y).GT ? y : x;
    }

    return foldl1(minBy, xs);
}


//#region zips

//-- | The 'zip4' function takes four lists and returns a list of
//-- quadruples, analogous to 'zip'.
//zip4                    :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
//zip4                    =  zipWith4 (,,,)

//-- | The 'zip5' function takes five lists and returns a list of
//-- five-tuples, analogous to 'zip'.
//zip5                    :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
//zip5                    =  zipWith5 (,,,,)

//-- | The 'zip6' function takes six lists and returns a list of six-tuples,
//-- analogous to 'zip'.
//zip6                    :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] ->
//                              [(a,b,c,d,e,f)]
//zip6                    =  zipWith6 (,,,,,)

//-- | The 'zip7' function takes seven lists and returns a list of
//-- seven-tuples, analogous to 'zip'.
//zip7                    :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] ->
//                              [g] -> [(a,b,c,d,e,f,g)]
//zip7                    =  zipWith7 (,,,,,,)


//-- | The 'zipWith4' function takes a function which combines four
//-- elements, as well as four lists and returns a list of their point-wise
//-- combination, analogous to 'zipWith'.
//zipWith4                :: (a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]
//zipWith4 z (a:as) (b:bs) (c:cs) (d:ds)
//                        =  z a b c d : zipWith4 z as bs cs ds
//zipWith4 _ _ _ _ _      =  []

//-- | The 'zipWith5' function takes a function which combines five
//-- elements, as well as five lists and returns a list of their point-wise
//-- combination, analogous to 'zipWith'.
//zipWith5                :: (a->b->c->d->e->f) ->
//                           [a]->[b]->[c]->[d]->[e]->[f]
//zipWith5 z (a:as) (b:bs) (c:cs) (d:ds) (e:es)
//                        =  z a b c d e : zipWith5 z as bs cs ds es
//zipWith5 _ _ _ _ _ _    = []

//-- | The 'zipWith6' function takes a function which combines six
//-- elements, as well as six lists and returns a list of their point-wise
//-- combination, analogous to 'zipWith'.
//zipWith6                :: (a->b->c->d->e->f->g) ->
//                           [a]->[b]->[c]->[d]->[e]->[f]->[g]
//zipWith6 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs)
//                        =  z a b c d e f : zipWith6 z as bs cs ds es fs
//zipWith6 _ _ _ _ _ _ _  = []

//-- | The 'zipWith7' function takes a function which combines seven
//-- elements, as well as seven lists and returns a list of their point-wise
//-- combination, analogous to 'zipWith'.
//zipWith7                :: (a->b->c->d->e->f->g->h) ->
//                           [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]
//zipWith7 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs)
//                   =  z a b c d e f g : zipWith7 z as bs cs ds es fs gs
//zipWith7 _ _ _ _ _ _ _ _ = []

//-- | The 'unzip4' function takes a list of quadruples and returns four
//-- lists, analogous to 'unzip'.
//unzip4                  :: [(a,b,c,d)] -> ([a],[b],[c],[d])
//unzip4                  =  foldr (\(a,b,c,d) ~(as,bs,cs,ds) ->
//                                        (a:as,b:bs,c:cs,d:ds))
//                                 ([],[],[],[])

//-- | The 'unzip5' function takes a list of five-tuples and returns five
//-- lists, analogous to 'unzip'.
//unzip5                  :: [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
//unzip5                  =  foldr (\(a,b,c,d,e) ~(as,bs,cs,ds,es) ->
//                                        (a:as,b:bs,c:cs,d:ds,e:es))
//                                 ([],[],[],[],[])

//-- | The 'unzip6' function takes a list of six-tuples and returns six
//-- lists, analogous to 'unzip'.
//unzip6                  :: [(a,b,c,d,e,f)] -> ([a],[b],[c],[d],[e],[f])
//unzip6                  =  foldr (\(a,b,c,d,e,f) ~(as,bs,cs,ds,es,fs) ->
//                                        (a:as,b:bs,c:cs,d:ds,e:es,f:fs))
//                                 ([],[],[],[],[],[])

//-- | The 'unzip7' function takes a list of seven-tuples and returns
//-- seven lists, analogous to 'unzip'.
//unzip7          :: [(a,b,c,d,e,f,g)] -> ([a],[b],[c],[d],[e],[f],[g])
//unzip7          =  foldr (\(a,b,c,d,e,f,g) ~(as,bs,cs,ds,es,fs,gs) ->
//                                (a:as,b:bs,c:cs,d:ds,e:es,f:fs,g:gs))
//                         ([],[],[],[],[],[],[])

//#endregion

var tupleFunctions = {};

imap(function(n){
    
    tupleFunctions["zip" + n] = function(){
        return tupleFunctions["zipWith" + n](Tuple["tuple" + n](slice(arguments)));
    };

    tupleFunctions["zipWith" + n] = function(z){
        var lists = map(uncons, slice(arguments, 1)),
            heads = map(fst, lists),
            tails = map(snd, lists);
        
        return cons(z.apply(null, heads)
                   ,tupleFunctions["zipWith" + n].apply(null, [z].concat(tails))
                   );
    };

    tupleFunctions["unzip" + n] = function(listOfTuples){
        return foldr(function(tuple, acc){ 
            return map(cons, tuple, acc);
        }, replicate(n, []), listOfTuples);
    };

}, [
    //1, 2, 3, 
    4, 5, 6, 7
    //, 8, 9, 10, 11, 12, 13, 14, 15
    ]);


//-- | The 'deleteFirstsBy' function takes a predicate and two lists and
//-- returns the first list with the first occurrence of each element of
//-- the second list removed.
//deleteFirstsBy          :: (a -> a -> Bool) -> [a] -> [a] -> [a]
//deleteFirstsBy eq       =  foldl (flip (deleteBy eq))
function deleteFirstsBy(eq, xs, ys){
    return foldl(function(ys, x){ return deleteBy(eq, x, ys) }, xs, ys);
}

//-- | The 'group' function takes a list and returns a list of lists such
//-- that the concatenation of the result is equal to the argument.  Moreover,
//-- each sublist in the result contains only equal elements.  For example,
//--
//-- > group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
//--
//-- It is a special case of 'groupBy', which allows the programmer to supply
//-- their own equality test.
//group                   :: Eq a => [a] -> [[a]]
//group                   =  groupBy (==)
function group(xs){
    return groupBy(Eq.eq, xs);
}

//-- | The 'groupBy' function is the non-overloaded version of 'group'.
//groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
//groupBy _  []           =  []
//groupBy eq (x:xs)       =  (x:ys) : groupBy eq zs
//                           where (ys,zs) = span (eq x) xs
function groupBy(eq, xs){
    if(!xs.length)
        return emptyListOf(xs);
    var a = uncons(xs),
        b = span(function(e){ return eq(x, e) }, xs);
    return cons(cons(a.head, b.ys), groupBy(eq, b.zs));
}

//-- | The 'inits' function returns all initial segments of the argument,
//-- shortest first.  For example,
//--
//-- > inits "abc" == ["","a","ab","abc"]
//--
//inits                   :: [a] -> [[a]]
//inits []                =  [[]]
//inits (x:xs)            =  [[]] ++ map (x:) (inits xs)
function inits(xs){
    if(!xs.length)
        return [emptyListOf(xs)];
    return append([emptyListOf(xs)], map(function(e){ return cons(x, e) }, inits(xs)));
}

//-- | The 'tails' function returns all final segments of the argument,
//-- longest first.  For example,
//--
//-- > tails "abc" == ["abc", "bc", "c",""]
//--
//tails                   :: [a] -> [[a]]
//tails []                =  [[]]
//tails xxs@(_:xs)        =  xxs : tails xs
function tails(xxs){
    if(!xs.length)
        return [emptyListOf(xxs)];
    return cons(xxs, tails(tail(xxs)));
}

//-- | The 'subsequences' function returns the list of all subsequences of the argument.
//--
//-- > subsequences "abc" == ["","a","b","ab","c","ac","bc","abc"]
//subsequences            :: [a] -> [[a]]
//subsequences xs         =  [] : nonEmptySubsequences xs
function subsequences(xs){
    return cons([], nonEmptySubsequences(xs));
}

//-- | The 'nonEmptySubsequences' function returns the list of all subsequences of the argument,
//--   except for the empty list.
//--
//-- > nonEmptySubsequences "abc" == ["a","b","ab","c","ac","bc","abc"]
//nonEmptySubsequences         :: [a] -> [[a]]
//nonEmptySubsequences []      =  []
//nonEmptySubsequences (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences xs)
//  where f ys r = ys : (x : ys) : r
function nonEmptySubsequences(xs){
    var a = uncons(xs);
    function f(ys, r){
        return cons(ys, cons(cons(a.head, ys), r));
    }
    return cons(a.head, foldr(f, [], nonEmptySubsequences(a.tail)));
}

//-- | The 'permutations' function returns the list of all permutations of the argument.
//--
//-- > permutations "abc" == ["abc","bac","cba","bca","cab","acb"]
//permutations            :: [a] -> [[a]]
//permutations xs0        =  xs0 : perms xs0 []
//  where
//    perms []     _  = []
//    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
//      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
//            interleave' _ []     r = (ts, r)
//            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
//                                     in  (y:us, f (t:y:us) : zs)
function permutations(xs0){
    
    function perms(ts, is){
        if(!ts.length)
            return emptyListOf(ts);

        var a = uncons(ts);

        function interleave(xs, r){
            return interleave_(id, xs, r)[1];
        }

        function interleave_(f, ys, r){
            if(!ys.length)
                return [a.tail, r];

            var b = uncons(ys),
                y = b.head,
                c = interleave_( compose1(f, function(e){ cons(y, e) }), ys, r),
                us = c[0],
                zs = c[1];

            return [cons(y, us), cons( f(cons(a.head, cons(y, us))) , zs)];
        }

        return foldr(interleave, perms(a.tail, cons(a.head, is)), permutations(is))
    }
    return cons(xs0, perms(xs0, []));
}


//------------------------------------------------------------------------------
//-- Quick Sort algorithm taken from HBC's QSort library.

//-- | The 'sort' function implements a stable sorting algorithm.
//-- It is a special case of 'sortBy', which allows the programmer to supply
//-- their own comparison function.

//sort :: (Ord a) => [a] -> [a]
function sort(l){
    var inst = getInstance(Ord, typeOf(l.charAt ? l.charAt(0) : l[0]));
    return sortBy(inst.compare, l);
}
//-- | The 'sortBy' function is the non-overloaded version of 'sort'.
//sortBy :: (a -> a -> Ordering) -> [a] -> [a]
function sortBy(cmp, arr){
    function sortFn(a, b){
        var res = cmp(a, b);
        return  res.LT ? -1 :
                res.GT ?  1 :
                res.EQ ?  0 :
                error(sort);
    }
    if(arr.sort)
        return arr.sort(sortFn);
    return slice(arr).sort(sortFn).join("");
}

//#ifdef USE_REPORT_PRELUDE
//sort = sortBy compare
function qsort(l){
    return sortBy(compare, l);
}
//sortBy cmp = foldr (insertBy cmp) []
function qsortBy(cmp, l){
    return foldr(function(a, b){ return insertBy(cmp, a, b) }, emptyListOf(l), l);
}
//#else

//sortBy cmp l = mergesort cmp l
function msortBy(cmp, l){
    return mergesort(cmp, l);
}
//sort l = mergesort compare l
function msort(l){
    return mergesort(compare, l);
}

//{-
//Quicksort replaced by mergesort, 14/5/2002.

//From: Ian Lynagh <igloo@earth.li>

//I am curious as to why the List.sort implementation in GHC is a
//quicksort algorithm rather than an algorithm that guarantees n log n
//time in the worst case? I have attached a mergesort implementation along
//with a few scripts to time it's performance, the results of which are
//shown below (* means it didn't finish successfully - in all cases this
//was due to a stack overflow).

//If I heap profile the random_list case with only 10000 then I see
//random_list peaks at using about 2.5M of memory, whereas in the same
//program using List.sort it uses only 100k.

//Input style     Input length     Sort data     Sort alg    User time
//stdin           10000            random_list   sort        2.82
//stdin           10000            random_list   mergesort   2.96
//stdin           10000            sorted        sort        31.37
//stdin           10000            sorted        mergesort   1.90
//stdin           10000            revsorted     sort        31.21
//stdin           10000            revsorted     mergesort   1.88
//stdin           100000           random_list   sort        *
//stdin           100000           random_list   mergesort   *
//stdin           100000           sorted        sort        *
//stdin           100000           sorted        mergesort   *
//stdin           100000           revsorted     sort        *
//stdin           100000           revsorted     mergesort   *
//func            10000            random_list   sort        0.31
//func            10000            random_list   mergesort   0.91
//func            10000            sorted        sort        19.09
//func            10000            sorted        mergesort   0.15
//func            10000            revsorted     sort        19.17
//func            10000            revsorted     mergesort   0.16
//func            100000           random_list   sort        3.85
//func            100000           random_list   mergesort   *
//func            100000           sorted        sort        5831.47
//func            100000           sorted        mergesort   2.23
//func            100000           revsorted     sort        5872.34
//func            100000           revsorted     mergesort   2.24
//-}

//mergesort :: (a -> a -> Ordering) -> [a] -> [a]
//mergesort cmp = mergesort' cmp . map wrap
function mergesort(cmp, xs){
    return mergesort_(cmp, map(wrap, xs));
}

//mergesort' :: (a -> a -> Ordering) -> [[a]] -> [a]
//mergesort' _   [] = []
//mergesort' _   [xs] = xs
//mergesort' cmp xss = mergesort' cmp (merge_pairs cmp xss)
function mergesort_(cmp, xss){
    var xssLength = xss.length
    if(xssLength == 0)
        return emptyListOf(xss);
    if(xssLength == 1)
        return xss.charAt ? xss.charAt(0) : xss[0];
    return mergesort_(cmp, merge_pairs(cmp, xss));
}

//merge_pairs :: (a -> a -> Ordering) -> [[a]] -> [[a]]
//merge_pairs _   [] = []
//merge_pairs _   [xs] = [xs]
//merge_pairs cmp (xs:ys:xss) = merge cmp xs ys : merge_pairs cmp xss
function merge_pairs(cmp, xss){
    var a = uncons(xss),
        xs = a.head,
        b = uncons(a.tail),
        ys = b.head;
    return cons(merge(cmp, xs, ys), merge_pairs(cmp, b.tail));
}

//merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
//merge _   [] ys = ys
//merge _   xs [] = xs
//merge cmp (x:xs) (y:ys)
// = case x `cmp` y of
//        GT -> y : merge cmp (x:xs)   ys
//        _  -> x : merge cmp    xs (y:ys)
function merge(cmp, xs, ys){
    var a = uncons(xs),
        b = uncons(ys);
    return cmp(a.head, b.head).GT ? 
                cons(b.head, merge(cmp, xs, b.tail)) :
                cons(a.head, merge(cmp, a.tail, ys)) ;
}

//wrap :: a -> [a]
//wrap x = [x]
function wrap(x){
    return [x];
}

//{-
//OLD: qsort version

//-- qsort is stable and does not concatenate.
//qsort :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
//qsort _   []     r = r
//qsort _   [x]    r = x:r
//qsort cmp (x:xs) r = qpart cmp x xs [] [] r

//-- qpart partitions and sorts the sublists
//qpart :: (a -> a -> Ordering) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
//qpart cmp x [] rlt rge r =
//    -- rlt and rge are in reverse order and must be sorted with an
//    -- anti-stable sorting
//    rqsort cmp rlt (x:rqsort cmp rge r)
//qpart cmp x (y:ys) rlt rge r =
//    case cmp x y of
//        GT -> qpart cmp x ys (y:rlt) rge r
//        _  -> qpart cmp x ys rlt (y:rge) r

//-- rqsort is as qsort but anti-stable, i.e. reverses equal elements
//rqsort :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
//rqsort _   []     r = r
//rqsort _   [x]    r = x:r
//rqsort cmp (x:xs) r = rqpart cmp x xs [] [] r

//rqpart :: (a -> a -> Ordering) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
//rqpart cmp x [] rle rgt r =
//    qsort cmp rle (x:qsort cmp rgt r)
//rqpart cmp x (y:ys) rle rgt r =
//    case cmp y x of
//        GT -> rqpart cmp x ys rle (y:rgt) r
//        _  -> rqpart cmp x ys (y:rle) rgt r
//-}

//#endif /* USE_REPORT_PRELUDE */

//-- | The 'unfoldr' function is a \`dual\' to 'foldr': while 'foldr'
//-- reduces a list to a summary value, 'unfoldr' builds a list from
//-- a seed value.  The function takes the element and returns 'Nothing'
//-- if it is done producing the list or returns 'Just' @(a,b)@, in which
//-- case, @a@ is a prepended to the list and @b@ is used as the next
//-- element in a recursive call.  For example,
//--
//-- > iterate f == unfoldr (\x -> Just (x, f x))
//--
//-- In some cases, 'unfoldr' can undo a 'foldr' operation:
//--
//-- > unfoldr f' (foldr f z xs) == xs
//--
//-- if the following holds:
//--
//-- > f' (f x y) = Just (x,y)
//-- > f' z       = Nothing
//--
//-- A simple use of unfoldr:
//--
//-- > unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
//-- >  [10,9,8,7,6,5,4,3,2,1]
//--
//unfoldr      :: (b -> Maybe (a, b)) -> b -> [a]
//unfoldr f b  =
//  case f b of
//   Just (a,new_b) -> a : unfoldr f new_b
//   Nothing        -> []
function unfoldr(f, b){
    var x = f(b);
    return x.Just    ? cons(x[0][0], unfoldr(f, x[0][1])) :
           x.Nothing ? [] : error();
}

//-- -----------------------------------------------------------------------------


//#ifdef __GLASGOW_HASKELL__
//-- | 'foldl1' is a variant of 'foldl' that has no starting value argument,
//-- and thus must be applied to non-empty lists.
//foldl1                  :: (a -> a -> a) -> [a] -> a
//foldl1 f (x:xs)         =  foldl f x xs
//foldl1 _ []             =  errorEmptyList "foldl1"
//#endif /* __GLASGOW_HASKELL__ */
function foldl1(f, xs){
    if(!xs.length)
        return errorEmptyList("foldl1");
    var a = uncons(xs);
    return foldl(f, a.head, a.tail);
}


//#ifdef __GLASGOW_HASKELL__
//-- -----------------------------------------------------------------------------
//-- List sum and product

//{-# SPECIALISE sum     :: [Int] -> Int #-}
//{-# SPECIALISE sum     :: [Integer] -> Integer #-}
//{-# SPECIALISE product :: [Int] -> Int #-}
//{-# SPECIALISE product :: [Integer] -> Integer #-}
//-- | The 'sum' function computes the sum of a finite list of numbers.
//sum                     :: (Num a) => [a] -> a
//-- | The 'product' function computes the product of a finite list of numbers.
//product                 :: (Num a) => [a] -> a
//#ifdef USE_REPORT_PRELUDE

//sum                     =  foldl (+) 0
function sum(xs){
    return foldl(unsafeAdd, 0, xs);
}

//product                 =  foldl (*) 1
function product(xs){
    return foldl(unsafeMul, 1, xs);
}

//#else
//sum     l       = sum' l 0
//  where
//    sum' []     a = a
//    sum' (x:xs) a = sum' xs (a+x)
//product l       = prod l 1
//  where
//    prod []     a = a
//    prod (x:xs) a = prod xs (a*x)
//#endif

//-- -----------------------------------------------------------------------------
//-- Functions on strings

//-- | 'lines' breaks a string up into a list of strings at newline
//-- characters.  The resulting strings do not contain newlines.
//lines                   :: String -> [String]
//lines ""                =  []
//lines s                 =  let (l, s') = break (== '\n') s
//                           in  l : case s' of
//                                        []      -> []
//                                        (_:s'') -> lines s''
function lines(s){
    var lines = s.split("\n");
    if(lines[lines.length - 1] == "")
        lines.pop();
    return lines;
}

//-- | 'unlines' is an inverse operation to 'lines'.
//-- It joins lines, after appending a terminating newline to each.
//unlines                 :: [String] -> String
//#ifdef USE_REPORT_PRELUDE
//unlines                 =  concatMap (++ "\n")
//#else
//-- HBC version (stolen)
//-- here's a more efficient version
//unlines [] = []
//unlines (l:ls) = l ++ '\n' : unlines ls
//#endif
function unlines(ls){
    return ls.join("\n");
}

//-- | 'words' breaks a string up into a list of words, which were delimited
//-- by white space.
//words                   :: String -> [String]
//words s                 =  case dropWhile {-partain:Char.-}isSpace s of
//                                "" -> []
//                                s' -> w : words s''
//                                      where (w, s'') =
//                                             break {-partain:Char.-}isSpace s'
function words(s){
    var words = s.split(/\s+/);
    if(words[0] == "")
        words.shift();
    if(words[words.length - 1] == "")
        words.pop();
    return words;
}

//-- | 'unwords' is an inverse operation to 'words'.
//-- It joins words with separating spaces.
//unwords                 :: [String] -> String
//#ifdef USE_REPORT_PRELUDE
//unwords []              =  ""
//unwords ws              =  foldr1 (\w s -> w ++ ' ':s) ws
//#else
//-- HBC version (stolen)
//-- here's a more efficient version
//unwords []              =  ""
//unwords [w]             = w
//unwords (w:ws)          = w ++ ' ' : unwords ws
//#endif
function unwords(ws){
    return ws.join(" ");
}


namespace("Data_List", {
    //   -- * Basic functions

    //TODO:  (++)    
     append : append
    ,head   : head           
    ,last   : last           
    ,tail   : tail           
    ,init   : init           
    ,null_  : null_           
    ,length : length         

    //   -- * List transformations
    ,map        : map            
    ,reverse    : reverse        

    ,intersperse    : intersperse    
    ,intercalate    : intercalate    
    ,transpose      : transpose      

    ,subsequences : subsequences   
    ,permutations : permutations   

    //   -- * Reducing lists (folds)

    ,foldl  : foldl          
    ,foldl1 : foldl1         
    ,foldr  : foldr          
    ,foldr1 : foldr1         

    //   -- ** Special folds

    ,concat     : concat            
    ,concatMap  : concatMap         
    ,and        : and               
    ,or         : or                
    ,any        : any               
    ,all        : all               
    ,sum        : sum               
    ,product    : product           
    ,maximum    : maximum           
    ,minimum    : minimum           

    //   -- * Building lists

    //   -- ** Scans
    ,scanl      : scanl             
    ,scanl1     : scanl1            
    ,scanr      : scanr             
    ,scanr1     : scanr1            

    //   -- ** Accumulating maps
    ,mapAccumL  : mapAccumL         
    ,mapAccumR  : mapAccumR         

    //   -- ** Infinite lists
    //TODO
    //,iterate    : iterate           
    //,repeat     : repeat            
    ,replicate  : replicate         
    //,cycle      : cycle             

    //   -- ** Unfolding
    ,unfoldr    : unfoldr          

    //   -- * Sublists

    //   -- ** Extracting sublists
    ,take       : take              
    ,drop       : drop              
    ,splitAt    : splitAt           

    ,takeWhile  : takeWhile         
    ,dropWhile  : dropWhile         
    ,span       : span              
    ,break_     : break_             

    ,stripPrefix : stripPrefix       

    ,group      : group             

    ,inits      : inits             
    ,tails      : tails             

    //   -- ** Predicates
    ,isPrefixOf : isPrefixOf        
    ,isSuffixOf : isSuffixOf        
    ,isInfixOf  : isInfixOf         

    //   -- * Searching lists

    //   -- ** Searching by equality
    ,elem       : elem              
    ,notElem    : notElem           
    ,lookup     : lookup            

    //   -- ** Searching with a predicate
    ,find       : find              
    ,filter     : filter            
    ,partition  : partition         

    //   -- * Indexing lists
    //   -- | These functions treat a list @xs@ as a indexed collection,
    //   -- with indices ranging from 0 to @'length' xs - 1@.

    //TODO: , (!!)              
    ,index          : index
    ,elemIndex      : elemIndex         
    ,elemIndices    : elemIndices       

    ,findIndex      : findIndex         
    ,findIndices    : findIndices       

    //   -- * Zipping and unzipping lists

    ,zip  : zip               
    ,zip3 : zip3
    ,zip4 : tupleFunctions.zip4
    ,zip5 : tupleFunctions.zip5
    ,zip6 : tupleFunctions.zip6
    ,zip7 : tupleFunctions.zip7

    ,zipWith  : zipWith           
    ,zipWith3 : zipWith3
    ,zipWith4 : tupleFunctions.zipWith4
    ,zipWith5 : tupleFunctions.zipWith5
    ,zipWith6 : tupleFunctions.zipWith6
    ,zipWith7 : tupleFunctions.zipWith7

    ,unzip  : unzip            
    ,unzip3 : unzip3
    ,unzip4 : tupleFunctions.unzip4
    ,unzip5 : tupleFunctions.unzip5
    ,unzip6 : tupleFunctions.unzip6
    ,unzip7 : tupleFunctions.unzip7

    //   -- * Special lists

    //   -- ** Functions on strings
    ,lines   : lines            
    ,words   : words            
    ,unlines : unlines          
    ,unwords : unwords          

    //   -- ** \"Set\" operations

    ,nub : nub               

    ,delete_    : delete_           
    ,difference : difference
    //TODO: , (\\)              

    ,union      : union             
    ,intersect  : intersect         

    //   -- ** Ordered lists
    ,sort       : sort              
    ,insert     : insert            

    //   -- * Generalized functions

    //   -- ** The \"@By@\" operations
    //   -- | By convention, overloaded functions have a non-overloaded
    //   -- counterpart whose name is suffixed with \`@By@\'.
    //   --
    //   -- It is often convenient to use these functions together with
    //   -- 'Data.Function.on', for instance @'sortBy' ('compare'
    //   -- \`on\` 'fst')@.

    //   -- *** User-supplied equality (replacing an @Eq@ context)
    //   -- | The predicate is assumed to define an equivalence.
    ,nubBy          : nubBy            
    ,deleteBy       : deleteBy         
    ,deleteFirstsBy : deleteFirstsBy   
    ,unionBy        : unionBy          
    ,intersectBy    : intersectBy      
    ,groupBy        : groupBy          

    //   -- *** User-supplied comparison (replacing an @Ord@ context)
    //   -- | The function is assumed to define a total ordering.
    ,sortBy         : sortBy            
    ,insertBy       : insertBy          
    ,maximumBy      : maximumBy         
    ,minimumBy      : minimumBy         

})﻿/// <reference path="../../../jshaskell/src/Haskell.js" local />
/// <reference path="../../../base/src/GHC/Unit.js" local />
/// <reference path="../../../base/src/GHC/Tuple.js" local />
/// <reference path="../../../base/src/GHC/Ordering.js" local />
/// <reference path="../../../base/src/GHC/Classes.js" local />



//{-# OPTIONS_GHC -XNoImplicitPrelude #-}
//{-# OPTIONS_GHC -fno-warn-unused-imports #-}
//{-# OPTIONS_GHC -fno-warn-orphans #-}
//-- XXX -fno-warn-unused-imports needed for the GHC.Tuple import below. Sigh.
//-----------------------------------------------------------------------------
//-- |
//-- Module      :  Data.Tuple
//-- Copyright   :  (c) The University of Glasgow 2001
//-- License     :  BSD-style (see the file libraries/base/LICENSE)
//-- 
//-- Maintainer  :  libraries@haskell.org
//-- Stability   :  experimental
//-- Portability :  portable
//--
//-- The tuple data types, and associated functions.
//--
//-----------------------------------------------------------------------------

//module Data.Tuple
//  ( fst         -- :: (a,b) -> a
//  , snd         -- :: (a,b) -> a
//  , curry       -- :: ((a, b) -> c) -> a -> b -> c
//  , uncurry     -- :: (a -> b -> c) -> ((a, b) -> c)
//#ifdef __NHC__
//  , (,)(..)
//  , (,,)(..)
//  , (,,,)(..)
//  , (,,,,)(..)
//  , (,,,,,)(..)
//  , (,,,,,,)(..)
//  , (,,,,,,,)(..)
//  , (,,,,,,,,)(..)
//  , (,,,,,,,,,)(..)
//  , (,,,,,,,,,,)(..)
//  , (,,,,,,,,,,,)(..)
//  , (,,,,,,,,,,,,)(..)
//  , (,,,,,,,,,,,,,)(..)
//  , (,,,,,,,,,,,,,,)(..)
//#endif
//  )
//    where

//#ifdef __GLASGOW_HASKELL__
//import GHC.Bool
//import GHC.Classes
//import GHC.Ordering
//-- XXX The standalone deriving clauses fail with
//--     The data constructors of `(,)' are not all in scope
//--       so you cannot derive an instance for it
//--     In the stand-alone deriving instance for `Eq (a, b)'
//-- if we don't import GHC.Tuple
//import GHC.Tuple
//#endif  /* __GLASGOW_HASKELL__ */

//#ifdef __NHC__
//import Prelude
//import Prelude
//  ( (,)(..)
//  , (,,)(..)
//  , (,,,)(..)
//  , (,,,,)(..)
//  , (,,,,,)(..)
//  , (,,,,,,)(..)
//  , (,,,,,,,)(..)
//  , (,,,,,,,,)(..)
//  , (,,,,,,,,,)(..)
//  , (,,,,,,,,,,)(..)
//  , (,,,,,,,,,,,)(..)
//  , (,,,,,,,,,,,,)(..)
//  , (,,,,,,,,,,,,,)(..)
//  , (,,,,,,,,,,,,,,)(..)
//  -- nhc98's prelude only supplies tuple instances up to size 15
//  , fst, snd
//  , curry, uncurry
//  )
//#endif

//#ifdef __GLASGOW_HASKELL__
//import GHC.Unit ()
//#endif

//default ()              -- Double isn't available yet

//#ifdef __GLASGOW_HASKELL__
//-- XXX Why aren't these derived?
//instance Eq () where
//    () == () = True
//    () /= () = False
instance(Eq, Unit, {
    eq: strictEq,
    ne: strictNe
})

//instance Ord () where
//    () <= () = True
//    () <  () = False
//    () >= () = True
//    () >  () = False
//    max () () = ()
//    min () () = ()
//    compare () () = EQ
instance(Ord, Unit, {
    "<=": function(a, b){ return true  },
    "<" : function(a, b){ return false },
    ">=": function(a, b){ return true  },
    ">" : function(a, b){ return false },
    max : function(a, b){ return unit  },
    min : function(a, b){ return unit  },
    compare : function(a, b){ return Ordering.EQ }
})

//TODO: tuple instances

//#ifndef __HADDOCK__
//deriving instance (Eq  a, Eq  b) => Eq  (a, b)
//deriving instance (Ord a, Ord b) => Ord (a, b)
//deriving instance (Eq  a, Eq  b, Eq  c) => Eq  (a, b, c)
//deriving instance (Ord a, Ord b, Ord c) => Ord (a, b, c)
//deriving instance (Eq  a, Eq  b, Eq  c, Eq  d) => Eq  (a, b, c, d)
//deriving instance (Ord a, Ord b, Ord c, Ord d) => Ord (a, b, c, d)
//deriving instance (Eq  a, Eq  b, Eq  c, Eq  d, Eq  e) => Eq  (a, b, c, d, e)
//deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e) => Ord (a, b, c, d, e)
//deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f)
//               => Eq (a, b, c, d, e, f)
//deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f)
//               => Ord (a, b, c, d, e, f)
//deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g)
//               => Eq (a, b, c, d, e, f, g)
//deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g)
//               => Ord (a, b, c, d, e, f, g)
//deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
//                   Eq h)
//               => Eq (a, b, c, d, e, f, g, h)
//deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
//                   Ord h)
//               => Ord (a, b, c, d, e, f, g, h)
//deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
//                   Eq h, Eq i)
//               => Eq (a, b, c, d, e, f, g, h, i)
//deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
//                   Ord h, Ord i)
//               => Ord (a, b, c, d, e, f, g, h, i)
//deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
//                   Eq h, Eq i, Eq j)
//               => Eq (a, b, c, d, e, f, g, h, i, j)
//deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
//                   Ord h, Ord i, Ord j)
//               => Ord (a, b, c, d, e, f, g, h, i, j)
//deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
//                   Eq h, Eq i, Eq j, Eq k)
//               => Eq (a, b, c, d, e, f, g, h, i, j, k)
//deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
//                   Ord h, Ord i, Ord j, Ord k)
//               => Ord (a, b, c, d, e, f, g, h, i, j, k)
//deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
//                   Eq h, Eq i, Eq j, Eq k, Eq l)
//               => Eq (a, b, c, d, e, f, g, h, i, j, k, l)
//deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
//                   Ord h, Ord i, Ord j, Ord k, Ord l)
//               => Ord (a, b, c, d, e, f, g, h, i, j, k, l)
//deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
//                   Eq h, Eq i, Eq j, Eq k, Eq l, Eq m)
//               => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m)
//deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
//                   Ord h, Ord i, Ord j, Ord k, Ord l, Ord m)
//               => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m)
//deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
//                   Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n)
//               => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
//deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
//                   Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n)
//               => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
//deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g,
//                   Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o)
//               => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
//deriving instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
//                   Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n, Ord o)
//               => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
//#endif  /* !__HADDOCK__ */
//#endif  /* __GLASGOW_HASKELL__ */

//-- ---------------------------------------------------------------------------
//-- Standard functions over tuples

//#if !defined(__HUGS__) && !defined(__NHC__)
//-- | Extract the first component of a pair.
//fst                     :: (a,b) -> a
//fst (x,_)               =  x
function fst(tuple){
    return tuple[0];
}


//-- | Extract the second component of a pair.
//snd                     :: (a,b) -> b
//snd (_,y)               =  y
function snd(tuple){
    return tuple[1];
}

//-- | 'curry' converts an uncurried function to a curried function.
//curry                   :: ((a, b) -> c) -> a -> b -> c
//curry f x y             =  f (x, y)

//TODO: revise naming
//`curry` is used for making javascript functions receive one or more arguments
//so this is called `curry_` since it's less frequently used, I think
function curry_(f){
    return function(x, y){
        var t = [x, y];
        t.constructor = Tuple.Tuple2;
        return f(t);
    }
}

//-- | 'uncurry' converts a curried function to a function on pairs.
//uncurry                 :: (a -> b -> c) -> ((a, b) -> c)
//uncurry f p             =  f (fst p) (snd p)
function uncurry(f){
    return function(p){
        return f(p[0], p[1]);
    }
}
//#endif  /* neither __HUGS__ nor __NHC__ */


namespace("Data_Tuple", {
     fst      : fst
    ,snd      : snd 
    ,curry_   : curry_
    ,uncurry  : uncurry
})﻿/// <reference path="../../../jshaskell/src/Haskell/Main.js" local />
/// <reference path="../../../jshaskell/src/Haskell/DataType.js" local />
/// <reference path="../../../base/src/GHC/Base.js" local />

//{-# OPTIONS_GHC -XNoImplicitPrelude #-}
//-----------------------------------------------------------------------------
//-- |
//-- Module      :  Data.Either
//-- Copyright   :  (c) The University of Glasgow 2001
//-- License     :  BSD-style (see the file libraries/base/LICENSE)
//-- 
//-- Maintainer  :  libraries@haskell.org
//-- Stability   :  experimental
//-- Portability :  portable
//--
//-- The Either type, and associated operations.
//--
//-----------------------------------------------------------------------------

//module Data.Either (
//   Either(..),
//   either,           -- :: (a -> c) -> (b -> c) -> Either a b -> c
//   lefts,            -- :: [Either a b] -> [a]
//   rights,           -- :: [Either a b] -> [b]
//   partitionEithers, -- :: [Either a b] -> ([a],[b])
// ) where

//#include "Typeable.h"

//#ifdef __GLASGOW_HASKELL__
//import GHC.Base
//import GHC.Show
//import GHC.Read
//#endif

//import Data.Typeable

//#ifdef __GLASGOW_HASKELL__
//{-
//-- just for testing
//import Test.QuickCheck
//-}

//{-|

//The 'Either' type represents values with two possibilities: a value of
//type @'Either' a b@ is either @'Left' a@ or @'Right' b@.

//The 'Either' type is sometimes used to represent a value which is
//either correct or an error; by convention, the 'Left' constructor is
//used to hold an error value and the 'Right' constructor is used to
//hold a correct value (mnemonic: \"right\" also means \"correct\").
//-}
//data  Either a b  =  Left a | Right b   deriving (Eq, Ord, Read, Show)
function Either(){}
data(Either, [["Left", "a"], ["Right", "b"]]);

//-- | Case analysis for the 'Either' type.
//-- If the value is @'Left' a@, apply the first function to @a@;
//-- if it is @'Right' b@, apply the second function to @b@.
//either                  :: (a -> c) -> (b -> c) -> Either a b -> c
//either f _ (Left x)     =  f x
//either _ g (Right y)    =  g y
//#endif  /* __GLASGOW_HASKELL__ */
function either(f, g, e){
    return  e.Left  ? f(e[0]) :
            e.Right ? g(e[0]) :
            error(either);
}

//INSTANCE_TYPEABLE2(Either,eitherTc,"Either")

//-- | Extracts from a list of 'Either' all the 'Left' elements
//-- All the 'Left' elements are extracted in order.

//lefts   :: [Either a b] -> [a]
//lefts x = [a | Left a <- x]
function lefts(eithers){
    var acc = [];
    for(var i = 0, l = eithers.length; i < l; ++i){
        var either = eithers[i];
        if(either.Left)
            acc.push(either[0]);
    }
    return acc;
}

//-- | Extracts from a list of 'Either' all the 'Right' elements
//-- All the 'Right' elements are extracted in order.

//rights   :: [Either a b] -> [b]
//rights x = [a | Right a <- x]
function rights(eithers){
    var acc = [];
    for(var i = 0, l = eithers.length; i < l; ++i){
        var either = eithers[i];
        if(either.Right)
            acc.push(either[0]);
    }
    return acc;
}

//-- | Partitions a list of 'Either' into two lists
//-- All the 'Left' elements are extracted, in order, to the first
//-- component of the output.  Similarly the 'Right' elements are extracted
//-- to the second component of the output.

//partitionEithers :: [Either a b] -> ([a],[b])
//partitionEithers = foldr (either left right) ([],[])
// where
//  left  a (l, r) = (a:l, r)
//  right a (l, r) = (l, a:r)
function partitionEithers(eithers){
    function left(a){
        return function(t){
            return [cons(a, t[0]), t[1]];
        }
    }
    function right(a){
        return function(t){
            return [t[0], cons(a, t[1])];
        }
    }

    return foldr(function(a, b){ return either(left, right, a)(b) }, [[],[]], eithers);
}

//{-
//{--------------------------------------------------------------------
//  Testing
//--------------------------------------------------------------------}
//prop_partitionEithers :: [Either Int Int] -> Bool
//prop_partitionEithers x =
//  partitionEithers x == (lefts x, rights x)
//-}

namespace("Data_Either", {
     Either : Either
    ,either : either           //:: (a -> c) -> (b -> c) -> Either a b -> c
    ,lefts  : lefts            //:: [Either a b] -> [a]
    ,rights : rights           //:: [Either a b] -> [b]
    ,partitionEithers : partitionEithers   //:: [Either a b] -> ([a],[b]) 
})﻿/// <reference path="../../../jshaskell/src/Haskell/Main.js" local />
/// <reference path="../../../base/src/GHC/Base.js" local />
/// <reference path="../../../base/src/GHC/Unicode.js" local />
/// <reference path="../../../base/src/GHC/Show.js" local />

//{-# OPTIONS_GHC -XNoImplicitPrelude #-}
//-----------------------------------------------------------------------------
//-- |
//-- Module      :  Data.Char
//-- Copyright   :  (c) The University of Glasgow 2001
//-- License     :  BSD-style (see the file libraries/base/LICENSE)
//-- 
//-- Maintainer  :  libraries@haskell.org
//-- Stability   :  stable
//-- Portability :  portable
//--
//-- The Char type and associated operations.
//--
//-----------------------------------------------------------------------------

//module Data.Char
//    (
//      Char

//    , String

//    -- * Character classification
//    -- | Unicode characters are divided into letters, numbers, marks,
//    -- punctuation, symbols, separators (including spaces) and others
//    -- (including control characters).
//    , isControl, isSpace
//    , isLower, isUpper, isAlpha, isAlphaNum, isPrint
//    , isDigit, isOctDigit, isHexDigit
//    , isLetter, isMark, isNumber, isPunctuation, isSymbol, isSeparator

//    -- ** Subranges
//    , isAscii, isLatin1
//    , isAsciiUpper, isAsciiLower

//    -- ** Unicode general categories
//    , GeneralCategory(..), generalCategory

//    -- * Case conversion
//    , toUpper, toLower, toTitle  -- :: Char -> Char

//    -- * Single digit characters
//    , digitToInt        -- :: Char -> Int
//    , intToDigit        -- :: Int  -> Char

//    -- * Numeric representations
//    , ord               -- :: Char -> Int
//    , chr               -- :: Int  -> Char

//    -- * String representations
//    , showLitChar       -- :: Char -> ShowS
//    , lexLitChar        -- :: ReadS String
//    , readLitChar       -- :: ReadS Char 

//     -- Implementation checked wrt. Haskell 98 lib report, 1/99.
//    ) where

//#ifdef __GLASGOW_HASKELL__
//import GHC.Base
//import GHC.Arr (Ix)
//import GHC.Real (fromIntegral)
//import GHC.Show
//import GHC.Read (Read, readLitChar, lexLitChar)
//import GHC.Unicode
//import GHC.Num
//import GHC.Enum
//#endif

//#ifdef __HUGS__
//import Hugs.Prelude (Ix)
//import Hugs.Char
//#endif

//#ifdef __NHC__
//import Prelude
//import Prelude(Char,String)
//import Char
//import Ix
//import NHC.FFI (CInt)
//foreign import ccall unsafe "WCsubst.h u_gencat" wgencat :: CInt -> CInt
//#endif

//-- | Convert a single digit 'Char' to the corresponding 'Int'.  
//-- This function fails unless its argument satisfies 'isHexDigit',
//-- but recognises both upper and lower-case hexadecimal digits
//-- (i.e. @\'0\'@..@\'9\'@, @\'a\'@..@\'f\'@, @\'A\'@..@\'F\'@).
//digitToInt :: Char -> Int
//digitToInt c
// | isDigit c            =  ord c - ord '0'
// | c >= 'a' && c <= 'f' =  ord c - ord 'a' + 10
// | c >= 'A' && c <= 'F' =  ord c - ord 'A' + 10
// | otherwise            =  error ("Char.digitToInt: not a digit " ++ show c) -- sigh
function digitToInt(c){
    var res = parseInt(c, 16);
    return isNaN(res) ? error(digitToInt) : res;
}

//#ifndef __GLASGOW_HASKELL__
//isAsciiUpper, isAsciiLower :: Char -> Bool
//isAsciiLower c          =  c >= 'a' && c <= 'z'
//isAsciiUpper c          =  c >= 'A' && c <= 'Z'
//#endif

//-- | Unicode General Categories (column 2 of the UnicodeData table)
//-- in the order they are listed in the Unicode standard.

//data GeneralCategory
//        = UppercaseLetter       -- ^ Lu: Letter, Uppercase
//        | LowercaseLetter       -- ^ Ll: Letter, Lowercase
//        | TitlecaseLetter       -- ^ Lt: Letter, Titlecase
//        | ModifierLetter        -- ^ Lm: Letter, Modifier
//        | OtherLetter           -- ^ Lo: Letter, Other
//        | NonSpacingMark        -- ^ Mn: Mark, Non-Spacing
//        | SpacingCombiningMark  -- ^ Mc: Mark, Spacing Combining
//        | EnclosingMark         -- ^ Me: Mark, Enclosing
//        | DecimalNumber         -- ^ Nd: Number, Decimal
//        | LetterNumber          -- ^ Nl: Number, Letter
//        | OtherNumber           -- ^ No: Number, Other
//        | ConnectorPunctuation  -- ^ Pc: Punctuation, Connector
//        | DashPunctuation       -- ^ Pd: Punctuation, Dash
//        | OpenPunctuation       -- ^ Ps: Punctuation, Open
//        | ClosePunctuation      -- ^ Pe: Punctuation, Close
//        | InitialQuote          -- ^ Pi: Punctuation, Initial quote
//        | FinalQuote            -- ^ Pf: Punctuation, Final quote
//        | OtherPunctuation      -- ^ Po: Punctuation, Other
//        | MathSymbol            -- ^ Sm: Symbol, Math
//        | CurrencySymbol        -- ^ Sc: Symbol, Currency
//        | ModifierSymbol        -- ^ Sk: Symbol, Modifier
//        | OtherSymbol           -- ^ So: Symbol, Other
//        | Space                 -- ^ Zs: Separator, Space
//        | LineSeparator         -- ^ Zl: Separator, Line
//        | ParagraphSeparator    -- ^ Zp: Separator, Paragraph
//        | Control               -- ^ Cc: Other, Control
//        | Format                -- ^ Cf: Other, Format
//        | Surrogate             -- ^ Cs: Other, Surrogate
//        | PrivateUse            -- ^ Co: Other, Private Use
//        | NotAssigned           -- ^ Cn: Other, Not Assigned
//        deriving (Eq, Ord, Enum, Read, Show, Bounded, Ix)

//-- | The Unicode general category of the character.
//generalCategory :: Char -> GeneralCategory
//#if defined(__GLASGOW_HASKELL__) || defined(__NHC__)
//generalCategory c = toEnum $ fromIntegral $ wgencat $ fromIntegral $ ord c
//#endif
//#ifdef __HUGS__
//generalCategory c = toEnum (primUniGenCat c)
//#endif

//-- derived character classifiers

//-- | Selects alphabetic Unicode characters (lower-case, upper-case and
//-- title-case letters, plus letters of caseless scripts and modifiers letters).
//-- This function is equivalent to 'Data.Char.isAlpha'.
//isLetter :: Char -> Bool
//isLetter c = case generalCategory c of
//        UppercaseLetter         -> True
//        LowercaseLetter         -> True
//        TitlecaseLetter         -> True
//        ModifierLetter          -> True
//        OtherLetter             -> True
//        _                       -> False

//-- | Selects Unicode mark characters, e.g. accents and the like, which
//-- combine with preceding letters.
//isMark :: Char -> Bool
//isMark c = case generalCategory c of
//        NonSpacingMark          -> True
//        SpacingCombiningMark    -> True
//        EnclosingMark           -> True
//        _                       -> False

//-- | Selects Unicode numeric characters, including digits from various
//-- scripts, Roman numerals, etc.
//isNumber :: Char -> Bool
//isNumber c = case generalCategory c of
//        DecimalNumber           -> True
//        LetterNumber            -> True
//        OtherNumber             -> True
//        _                       -> False

//-- | Selects Unicode punctuation characters, including various kinds
//-- of connectors, brackets and quotes.
//isPunctuation :: Char -> Bool
//isPunctuation c = case generalCategory c of
//        ConnectorPunctuation    -> True
//        DashPunctuation         -> True
//        OpenPunctuation         -> True
//        ClosePunctuation        -> True
//        InitialQuote            -> True
//        FinalQuote              -> True
//        OtherPunctuation        -> True
//        _                       -> False

//-- | Selects Unicode symbol characters, including mathematical and
//-- currency symbols.
//isSymbol :: Char -> Bool
//isSymbol c = case generalCategory c of
//        MathSymbol              -> True
//        CurrencySymbol          -> True
//        ModifierSymbol          -> True
//        OtherSymbol             -> True
//        _                       -> False

//-- | Selects Unicode space and separator characters.
//isSeparator :: Char -> Bool
//isSeparator c = case generalCategory c of
//        Space                   -> True
//        LineSeparator           -> True
//        ParagraphSeparator      -> True
//        _                       -> False

//#ifdef __NHC__
//-- dummy implementation
//toTitle :: Char -> Char
//toTitle = toUpper
//#endif

namespace("Data_Char", {
    //from GHC.Base
     String     : String
    ,ord        : ord
    ,chr        : chr

    //from GHC.Unicode
    ,isSpace    : isSpace
    ,isUpper    : isUpper
    ,isLower    : isLower
    ,isAlpha    : isAlpha
    ,isAlphaNum : isAlphaNum
    ,isDigit    : isDigit
    ,isHexDigit : isHexDigit
    ,isOctDigit : isOctDigit
    ,toUpper    : toUpper
    ,toLower    : toLower
    ,toTitle    : toTitle
    
    //from GHC.Show
    //,intToDigit : intToDigit

    ,digitToInt : digitToInt

})
}());;(function(){
;var Bool = NS['Haskell'].Bool, isArray = NS['Haskell'].isArray, isDefined = NS['Haskell'].isDefined, slice = NS['Haskell'].slice, imap = NS['Haskell'].imap, ifilter = NS['Haskell'].ifilter, indexOf = NS['Haskell'].indexOf, lastIndexOf = NS['Haskell'].lastIndexOf, isort = NS['Haskell'].isort, range = NS['Haskell'].range, extend = NS['Haskell'].extend, namespace = NS['Haskell'].namespace, typeOf = NS['Haskell'].typeOf, strictEq = NS['Haskell'].strictEq, strictNe = NS['Haskell'].strictNe, unsafeAdd = NS['Haskell'].unsafeAdd, unsafeSub = NS['Haskell'].unsafeSub, unsafeMul = NS['Haskell'].unsafeMul, unsafeDiv = NS['Haskell'].unsafeDiv, lt = NS['Haskell'].lt, le = NS['Haskell'].le, gt = NS['Haskell'].gt, ge = NS['Haskell'].ge, negate = NS['Haskell'].negate, evalThunks = NS['Haskell'].evalThunks, toArray = NS['Haskell'].toArray, curry = NS['Haskell'].curry, error = NS['Haskell'].error, data = NS['Haskell'].data, ADT = NS['Haskell'].ADT, record = NS['Haskell'].record, accessor = NS['Haskell'].accessor, accessors = NS['Haskell'].accessors, typeclass = NS['Haskell'].typeclass, VARARG = NS['Haskell'].VARARG, instance = NS['Haskell'].instance, getInstance = NS['Haskell'].getInstance, asTypeOf = NS['Haskell'].asTypeOf, operators = NS['Haskell'].operators, infix = NS['Haskell'].infix, infixl = NS['Haskell'].infixl, infixr = NS['Haskell'].infixr, op = NS['Haskell'].op, no = NS['Haskell'].no, resolve = NS['Haskell'].resolve, recurse = NS['Haskell'].recurse, Recurse = NS['Haskell'].Recurse, exl = NS['Haskell'].exl, exs = NS['Haskell'].exs, createDo = NS['Haskell'].createDo, Scope = NS['Haskell'].Scope;;var Either = NS['Data_Either'].Either, either = NS['Data_Either'].either, lefts = NS['Data_Either'].lefts, rights = NS['Data_Either'].rights, partitionEithers = NS['Data_Either'].partitionEithers;;var fst = NS['Data_Tuple'].fst, snd = NS['Data_Tuple'].snd, curry_ = NS['Data_Tuple'].curry_, uncurry = NS['Data_Tuple'].uncurry;;var append = NS['Data_List'].append, head = NS['Data_List'].head, last = NS['Data_List'].last, tail = NS['Data_List'].tail, init = NS['Data_List'].init, null_ = NS['Data_List'].null_, length = NS['Data_List'].length, map = NS['Data_List'].map, reverse = NS['Data_List'].reverse, intersperse = NS['Data_List'].intersperse, intercalate = NS['Data_List'].intercalate, transpose = NS['Data_List'].transpose, subsequences = NS['Data_List'].subsequences, permutations = NS['Data_List'].permutations, foldl = NS['Data_List'].foldl, foldl1 = NS['Data_List'].foldl1, foldr = NS['Data_List'].foldr, foldr1 = NS['Data_List'].foldr1, concat = NS['Data_List'].concat, concatMap = NS['Data_List'].concatMap, and = NS['Data_List'].and, or = NS['Data_List'].or, any = NS['Data_List'].any, all = NS['Data_List'].all, sum = NS['Data_List'].sum, product = NS['Data_List'].product, maximum = NS['Data_List'].maximum, minimum = NS['Data_List'].minimum, scanl = NS['Data_List'].scanl, scanl1 = NS['Data_List'].scanl1, scanr = NS['Data_List'].scanr, scanr1 = NS['Data_List'].scanr1, mapAccumL = NS['Data_List'].mapAccumL, mapAccumR = NS['Data_List'].mapAccumR, replicate = NS['Data_List'].replicate, unfoldr = NS['Data_List'].unfoldr, take = NS['Data_List'].take, drop = NS['Data_List'].drop, splitAt = NS['Data_List'].splitAt, takeWhile = NS['Data_List'].takeWhile, dropWhile = NS['Data_List'].dropWhile, span = NS['Data_List'].span, break_ = NS['Data_List'].break_, stripPrefix = NS['Data_List'].stripPrefix, group = NS['Data_List'].group, inits = NS['Data_List'].inits, tails = NS['Data_List'].tails, isPrefixOf = NS['Data_List'].isPrefixOf, isSuffixOf = NS['Data_List'].isSuffixOf, isInfixOf = NS['Data_List'].isInfixOf, elem = NS['Data_List'].elem, notElem = NS['Data_List'].notElem, lookup = NS['Data_List'].lookup, find = NS['Data_List'].find, filter = NS['Data_List'].filter, partition = NS['Data_List'].partition, index = NS['Data_List'].index, elemIndex = NS['Data_List'].elemIndex, elemIndices = NS['Data_List'].elemIndices, findIndex = NS['Data_List'].findIndex, findIndices = NS['Data_List'].findIndices, zip = NS['Data_List'].zip, zip3 = NS['Data_List'].zip3, zip4 = NS['Data_List'].zip4, zip5 = NS['Data_List'].zip5, zip6 = NS['Data_List'].zip6, zip7 = NS['Data_List'].zip7, zipWith = NS['Data_List'].zipWith, zipWith3 = NS['Data_List'].zipWith3, zipWith4 = NS['Data_List'].zipWith4, zipWith5 = NS['Data_List'].zipWith5, zipWith6 = NS['Data_List'].zipWith6, zipWith7 = NS['Data_List'].zipWith7, unzip = NS['Data_List'].unzip, unzip3 = NS['Data_List'].unzip3, unzip4 = NS['Data_List'].unzip4, unzip5 = NS['Data_List'].unzip5, unzip6 = NS['Data_List'].unzip6, unzip7 = NS['Data_List'].unzip7, lines = NS['Data_List'].lines, words = NS['Data_List'].words, unlines = NS['Data_List'].unlines, unwords = NS['Data_List'].unwords, nub = NS['Data_List'].nub, delete_ = NS['Data_List'].delete_, difference = NS['Data_List'].difference, union = NS['Data_List'].union, intersect = NS['Data_List'].intersect, sort = NS['Data_List'].sort, insert = NS['Data_List'].insert, nubBy = NS['Data_List'].nubBy, deleteBy = NS['Data_List'].deleteBy, deleteFirstsBy = NS['Data_List'].deleteFirstsBy, unionBy = NS['Data_List'].unionBy, intersectBy = NS['Data_List'].intersectBy, groupBy = NS['Data_List'].groupBy, sortBy = NS['Data_List'].sortBy, insertBy = NS['Data_List'].insertBy, maximumBy = NS['Data_List'].maximumBy, minimumBy = NS['Data_List'].minimumBy;;var Maybe = NS['Data_Maybe'].Maybe, maybe = NS['Data_Maybe'].maybe, isJust = NS['Data_Maybe'].isJust, isNothing = NS['Data_Maybe'].isNothing, fromJust = NS['Data_Maybe'].fromJust, fromMaybe = NS['Data_Maybe'].fromMaybe, listToMaybe = NS['Data_Maybe'].listToMaybe, maybeToList = NS['Data_Maybe'].maybeToList, catMaybes = NS['Data_Maybe'].catMaybes, mapMaybe = NS['Data_Maybe'].mapMaybe;;var Unit = NS['GHC_Base'].Unit, Tuple = NS['GHC_Base'].Tuple, Bool = NS['GHC_Base'].Bool, Ordering = NS['GHC_Base'].Ordering, Eq = NS['GHC_Base'].Eq, Ord = NS['GHC_Base'].Ord, andOp = NS['GHC_Base'].andOp, orOp = NS['GHC_Base'].orOp, not = NS['GHC_Base'].not, Functor = NS['GHC_Base'].Functor, Monad = NS['GHC_Base'].Monad, emptyListOf = NS['GHC_Base'].emptyListOf, cons = NS['GHC_Base'].cons, consJoin = NS['GHC_Base'].consJoin, uncons = NS['GHC_Base'].uncons, augment = NS['GHC_Base'].augment, map = NS['GHC_Base'].map, foldr = NS['GHC_Base'].foldr, append = NS['GHC_Base'].append, otherwise = NS['GHC_Base'].otherwise, ord = NS['GHC_Base'].ord, chr = NS['GHC_Base'].chr, eqString = NS['GHC_Base'].eqString, minInt = NS['GHC_Base'].minInt, maxInt = NS['GHC_Base'].maxInt, id = NS['GHC_Base'].id, lazy = NS['GHC_Base'].lazy, assert = NS['GHC_Base'].assert, const_ = NS['GHC_Base'].const_, compose1 = NS['GHC_Base'].compose1, compose = NS['GHC_Base'].compose, flip = NS['GHC_Base'].flip, call = NS['GHC_Base'].call, until = NS['GHC_Base'].until, asTypeOf = NS['GHC_Base'].asTypeOf, elemIndex = NS['GHC_Base'].elemIndex, unsafeCompare = NS['GHC_Base'].unsafeCompare, readHex = NS['GHC_Base'].readHex, readOct = NS['GHC_Base'].readOct, round = NS['GHC_Base'].round, toInteger = NS['GHC_Base'].toInteger, fromInteger = NS['GHC_Base'].fromInteger, fromIntegral = NS['GHC_Base'].fromIntegral;;var Show = NS['GHC_Show'].Show;﻿/// <reference path="../../jshaskell/src/Haskell.js" local />
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
}());;(function(){
;var Bool = NS['Haskell'].Bool, isArray = NS['Haskell'].isArray, isDefined = NS['Haskell'].isDefined, slice = NS['Haskell'].slice, imap = NS['Haskell'].imap, ifilter = NS['Haskell'].ifilter, indexOf = NS['Haskell'].indexOf, lastIndexOf = NS['Haskell'].lastIndexOf, isort = NS['Haskell'].isort, range = NS['Haskell'].range, extend = NS['Haskell'].extend, namespace = NS['Haskell'].namespace, typeOf = NS['Haskell'].typeOf, strictEq = NS['Haskell'].strictEq, strictNe = NS['Haskell'].strictNe, unsafeAdd = NS['Haskell'].unsafeAdd, unsafeSub = NS['Haskell'].unsafeSub, unsafeMul = NS['Haskell'].unsafeMul, unsafeDiv = NS['Haskell'].unsafeDiv, lt = NS['Haskell'].lt, le = NS['Haskell'].le, gt = NS['Haskell'].gt, ge = NS['Haskell'].ge, negate = NS['Haskell'].negate, evalThunks = NS['Haskell'].evalThunks, toArray = NS['Haskell'].toArray, curry = NS['Haskell'].curry, error = NS['Haskell'].error, data = NS['Haskell'].data, ADT = NS['Haskell'].ADT, record = NS['Haskell'].record, accessor = NS['Haskell'].accessor, accessors = NS['Haskell'].accessors, typeclass = NS['Haskell'].typeclass, VARARG = NS['Haskell'].VARARG, instance = NS['Haskell'].instance, getInstance = NS['Haskell'].getInstance, asTypeOf = NS['Haskell'].asTypeOf, operators = NS['Haskell'].operators, infix = NS['Haskell'].infix, infixl = NS['Haskell'].infixl, infixr = NS['Haskell'].infixr, op = NS['Haskell'].op, no = NS['Haskell'].no, resolve = NS['Haskell'].resolve, recurse = NS['Haskell'].recurse, Recurse = NS['Haskell'].Recurse, exl = NS['Haskell'].exl, exs = NS['Haskell'].exs, createDo = NS['Haskell'].createDo, Scope = NS['Haskell'].Scope;;var cons = NS['Prelude'].cons, consJoin = NS['Prelude'].consJoin, uncons = NS['Prelude'].uncons, elemIndex = NS['Prelude'].elemIndex, unsafeCompare = NS['Prelude'].unsafeCompare, readHex = NS['Prelude'].readHex, readOct = NS['Prelude'].readOct, round = NS['Prelude'].round, toInteger = NS['Prelude'].toInteger, fromInteger = NS['Prelude'].fromInteger, fromIntegral = NS['Prelude'].fromIntegral, Bool = NS['Prelude'].Bool, andOp = NS['Prelude'].andOp, orOp = NS['Prelude'].orOp, not = NS['Prelude'].not, otherwise = NS['Prelude'].otherwise, Maybe = NS['Prelude'].Maybe, maybe = NS['Prelude'].maybe, Either = NS['Prelude'].Either, either = NS['Prelude'].either, Ordering = NS['Prelude'].Ordering, String = NS['Prelude'].String, fst = NS['Prelude'].fst, snd = NS['Prelude'].snd, curry = NS['Prelude'].curry, curry_ = NS['Prelude'].curry_, uncurry = NS['Prelude'].uncurry, Eq = NS['Prelude'].Eq, Ord = NS['Prelude'].Ord, Monad = NS['Prelude'].Monad, Functor = NS['Prelude'].Functor, id = NS['Prelude'].id, const_ = NS['Prelude'].const_, compose = NS['Prelude'].compose, compose1 = NS['Prelude'].compose1, flip = NS['Prelude'].flip, call = NS['Prelude'].call, until = NS['Prelude'].until, asTypeOf = NS['Prelude'].asTypeOf, error = NS['Prelude'].error, map = NS['Prelude'].map, append = NS['Prelude'].append, filter = NS['Prelude'].filter, head = NS['Prelude'].head, last = NS['Prelude'].last, tail = NS['Prelude'].tail, init = NS['Prelude'].init, null_ = NS['Prelude'].null_, length = NS['Prelude'].length, index = NS['Prelude'].index, reverse = NS['Prelude'].reverse, foldl = NS['Prelude'].foldl, foldl1 = NS['Prelude'].foldl1, foldr = NS['Prelude'].foldr, foldr1 = NS['Prelude'].foldr1, and = NS['Prelude'].and, or = NS['Prelude'].or, any = NS['Prelude'].any, all = NS['Prelude'].all, sum = NS['Prelude'].sum, product = NS['Prelude'].product, concat = NS['Prelude'].concat, concatMap = NS['Prelude'].concatMap, maximum = NS['Prelude'].maximum, minimum = NS['Prelude'].minimum, scanl = NS['Prelude'].scanl, scanl1 = NS['Prelude'].scanl1, scanr = NS['Prelude'].scanr, scanr1 = NS['Prelude'].scanr1, replicate = NS['Prelude'].replicate, take = NS['Prelude'].take, drop = NS['Prelude'].drop, splitAt = NS['Prelude'].splitAt, takeWhile = NS['Prelude'].takeWhile, dropWhile = NS['Prelude'].dropWhile, span = NS['Prelude'].span, break_ = NS['Prelude'].break_, elem = NS['Prelude'].elem, notElem = NS['Prelude'].notElem, lookup = NS['Prelude'].lookup, zip = NS['Prelude'].zip, zip3 = NS['Prelude'].zip3, zipWith = NS['Prelude'].zipWith, zipWith3 = NS['Prelude'].zipWith3, unzip = NS['Prelude'].unzip, unzip3 = NS['Prelude'].unzip3, lines = NS['Prelude'].lines, words = NS['Prelude'].words, unlines = NS['Prelude'].unlines, unwords = NS['Prelude'].unwords, Show = NS['Prelude'].Show;;var String = NS['Data_Char'].String, ord = NS['Data_Char'].ord, chr = NS['Data_Char'].chr, isSpace = NS['Data_Char'].isSpace, isUpper = NS['Data_Char'].isUpper, isLower = NS['Data_Char'].isLower, isAlpha = NS['Data_Char'].isAlpha, isAlphaNum = NS['Data_Char'].isAlphaNum, isDigit = NS['Data_Char'].isDigit, isHexDigit = NS['Data_Char'].isHexDigit, isOctDigit = NS['Data_Char'].isOctDigit, toUpper = NS['Data_Char'].toUpper, toLower = NS['Data_Char'].toLower, toTitle = NS['Data_Char'].toTitle, digitToInt = NS['Data_Char'].digitToInt;;var append = NS['Data_List'].append, head = NS['Data_List'].head, last = NS['Data_List'].last, tail = NS['Data_List'].tail, init = NS['Data_List'].init, null_ = NS['Data_List'].null_, length = NS['Data_List'].length, map = NS['Data_List'].map, reverse = NS['Data_List'].reverse, intersperse = NS['Data_List'].intersperse, intercalate = NS['Data_List'].intercalate, transpose = NS['Data_List'].transpose, subsequences = NS['Data_List'].subsequences, permutations = NS['Data_List'].permutations, foldl = NS['Data_List'].foldl, foldl1 = NS['Data_List'].foldl1, foldr = NS['Data_List'].foldr, foldr1 = NS['Data_List'].foldr1, concat = NS['Data_List'].concat, concatMap = NS['Data_List'].concatMap, and = NS['Data_List'].and, or = NS['Data_List'].or, any = NS['Data_List'].any, all = NS['Data_List'].all, sum = NS['Data_List'].sum, product = NS['Data_List'].product, maximum = NS['Data_List'].maximum, minimum = NS['Data_List'].minimum, scanl = NS['Data_List'].scanl, scanl1 = NS['Data_List'].scanl1, scanr = NS['Data_List'].scanr, scanr1 = NS['Data_List'].scanr1, mapAccumL = NS['Data_List'].mapAccumL, mapAccumR = NS['Data_List'].mapAccumR, replicate = NS['Data_List'].replicate, unfoldr = NS['Data_List'].unfoldr, take = NS['Data_List'].take, drop = NS['Data_List'].drop, splitAt = NS['Data_List'].splitAt, takeWhile = NS['Data_List'].takeWhile, dropWhile = NS['Data_List'].dropWhile, span = NS['Data_List'].span, break_ = NS['Data_List'].break_, stripPrefix = NS['Data_List'].stripPrefix, group = NS['Data_List'].group, inits = NS['Data_List'].inits, tails = NS['Data_List'].tails, isPrefixOf = NS['Data_List'].isPrefixOf, isSuffixOf = NS['Data_List'].isSuffixOf, isInfixOf = NS['Data_List'].isInfixOf, elem = NS['Data_List'].elem, notElem = NS['Data_List'].notElem, lookup = NS['Data_List'].lookup, find = NS['Data_List'].find, filter = NS['Data_List'].filter, partition = NS['Data_List'].partition, index = NS['Data_List'].index, elemIndex = NS['Data_List'].elemIndex, elemIndices = NS['Data_List'].elemIndices, findIndex = NS['Data_List'].findIndex, findIndices = NS['Data_List'].findIndices, zip = NS['Data_List'].zip, zip3 = NS['Data_List'].zip3, zip4 = NS['Data_List'].zip4, zip5 = NS['Data_List'].zip5, zip6 = NS['Data_List'].zip6, zip7 = NS['Data_List'].zip7, zipWith = NS['Data_List'].zipWith, zipWith3 = NS['Data_List'].zipWith3, zipWith4 = NS['Data_List'].zipWith4, zipWith5 = NS['Data_List'].zipWith5, zipWith6 = NS['Data_List'].zipWith6, zipWith7 = NS['Data_List'].zipWith7, unzip = NS['Data_List'].unzip, unzip3 = NS['Data_List'].unzip3, unzip4 = NS['Data_List'].unzip4, unzip5 = NS['Data_List'].unzip5, unzip6 = NS['Data_List'].unzip6, unzip7 = NS['Data_List'].unzip7, lines = NS['Data_List'].lines, words = NS['Data_List'].words, unlines = NS['Data_List'].unlines, unwords = NS['Data_List'].unwords, nub = NS['Data_List'].nub, delete_ = NS['Data_List'].delete_, difference = NS['Data_List'].difference, union = NS['Data_List'].union, intersect = NS['Data_List'].intersect, sort = NS['Data_List'].sort, insert = NS['Data_List'].insert, nubBy = NS['Data_List'].nubBy, deleteBy = NS['Data_List'].deleteBy, deleteFirstsBy = NS['Data_List'].deleteFirstsBy, unionBy = NS['Data_List'].unionBy, intersectBy = NS['Data_List'].intersectBy, groupBy = NS['Data_List'].groupBy, sortBy = NS['Data_List'].sortBy, insertBy = NS['Data_List'].insertBy, maximumBy = NS['Data_List'].maximumBy, minimumBy = NS['Data_List'].minimumBy;/// <reference path="../../../../jshaskell/src/Haskell.js" local />
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
    evalThunks(p(new Scope(), input, function(result){
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
    }), async);
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
        
        function next(parser0){
            return function(scope, state, k){
                return function(){ return parser0(scope, state, function(result){
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

//char_ :: Char -> Parser
var char_ = tokenPrim(function(c, state, startIndex){
    if(state.length > 0 && state.at(0) == c){
        state.scroll(1);
        return {ast: c, success: true};
    }
    return {success: false, expecting: c};
});


//satisfy :: (Char -> Bool) -> Parser
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
        return k({ast: at, success: true});
    }
    
    return k({success: false, expecting : "anyToken"});
}


//-- | This parser only succeeds at the end of the input. This is not a
//-- primitive parser but it is defined using 'notFollowedBy'.
//--
//-- >  eof  = notFollowedBy anyToken <?> "end of input"
//
//eof :: (Stream s m t, Show t) => ParsecT s u m ()
//eof                 = notFollowedBy anyToken <?> "end of input"
//

function eof(scope, state, k){
    return k({success: !state.length, expecting: state.length ? "end of input" : undef});
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
});/// <reference path="../../../../base/src/Data/Char.js" local />
/// <reference path="Prim.js" />


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

var space = exs(satisfy, isSpace ,"<?>", "space");


// | Skips /zero/ or more white space characters. See also 'skipMany'.

//spaces :: (Stream s m Char) => ParsecT s u m ()
//spaces              = skipMany space        <?> "white space"

var spaces = exs(skipMany, space ,"<?>", "white space");


// | Parses a newline character (\'\\n\'). Returns a newline character. 

//newline :: (Stream s m Char) => ParsecT s u m Char
//newline             = char '\n'             <?> "new-line"

var newline = exs(char_, '\n' ,"<?>", "new-line");

// | Parses a tab character (\'\\t\'). Returns a tab character. 

//tab :: (Stream s m Char) => ParsecT s u m Char
//tab                 = char '\t'             <?> "tab"

var tab = exs(char_, '\t' ,"<?>", "tab");

// | Parses an upper case letter (a character between \'A\' and \'Z\').
// Returns the parsed character. 

//upper :: (Stream s m Char) => ParsecT s u m Char
//upper               = satisfy isUpper       <?> "uppercase letter"

var upper = exs(satisfy, isUpper ,"<?>", "uppercase letter");


// | Parses a lower case character (a character between \'a\' and \'z\').
// Returns the parsed character. 

//lower :: (Stream s m Char) => ParsecT s u m Char
//lower               = satisfy isLower       <?> "lowercase letter"

var lower = exs(satisfy, isLower ,"<?>", "lowercase letter");


// | Parses a letter or digit (a character between \'0\' and \'9\').
// Returns the parsed character. 

//alphaNum :: (Stream s m Char => ParsecT s u m Char)
//alphaNum            = satisfy isAlphaNum    <?> "letter or digit"

var alphaNum = exs(satisfy, isAlphaNum ,"<?>", "letter or digit");


// | Parses a letter (an upper case or lower case character). Returns the
// parsed character. 

//letter :: (Stream s m Char) => ParsecT s u m Char
//letter              = satisfy isAlpha       <?> "letter"

var letter = exs(satisfy, isAlpha ,"<?>", "letter");

// | Parses a digit. Returns the parsed character. 

//digit :: (Stream s m Char) => ParsecT s u m Char
//digit               = satisfy isDigit       <?> "digit"

var digit = exs(satisfy, isDigit ,"<?>", "digit");


// | Parses a hexadecimal digit (a digit or a letter between \'a\' and
// \'f\' or \'A\' and \'F\'). Returns the parsed character. 

//hexDigit :: (Stream s m Char) => ParsecT s u m Char
//hexDigit            = satisfy isHexDigit    <?> "hexadecimal digit"

var hexDigit = exs(satisfy, isHexDigit ,"<?>", "hexadecimal digit");


// | Parses an octal digit (a character between \'0\' and \'7\'). Returns
// the parsed character. 

//octDigit :: (Stream s m Char) => ParsecT s u m Char
//octDigit            = satisfy isOctDigit    <?> "octal digit"

var octDigit = exs(satisfy, isOctDigit ,"<?>", "octal digit");


// | This parser succeeds for any character. Returns the parsed character. 

//anyChar :: (Stream s m Char) => ParsecT s u m Char
//anyChar             = satisfy (const True)

var anyChar = exs(satisfy, const_(true));


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
});/// <reference path="../../../../base/src/Data/Char.js" local />
/// <reference path="../../../../base/src/Data/List.js" local />
/// <reference path="Prim.js" />
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

var whiteSpace = exs(
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

var naturalOrFloat  = exs(lexeme, natFloat   ,"<?>", "number" );

var float_          = exs(lexeme, floating   ,"<?>", "float"  );
var integer         = exs(lexeme, int_       ,"<?>", "integer");
var natural         = exs(lexeme, nat        ,"<?>", "natural");




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
    //var compare = getInstance(Ord, name).compare; //TODO
    var compare = unsafeCompare;
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
               , identLetter    : exs(alphaNum   ,"<|>", oneOf, "_'")
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
;var data = NS['Haskell_DataType'].data, ADT = NS['Haskell_DataType'].ADT, record = NS['Haskell_DataType'].record, accessor = NS['Haskell_DataType'].accessor, accessors = NS['Haskell_DataType'].accessors;;var cons = NS['Prelude'].cons, consJoin = NS['Prelude'].consJoin, uncons = NS['Prelude'].uncons, elemIndex = NS['Prelude'].elemIndex, unsafeCompare = NS['Prelude'].unsafeCompare, readHex = NS['Prelude'].readHex, readOct = NS['Prelude'].readOct, round = NS['Prelude'].round, toInteger = NS['Prelude'].toInteger, fromInteger = NS['Prelude'].fromInteger, fromIntegral = NS['Prelude'].fromIntegral, Bool = NS['Prelude'].Bool, andOp = NS['Prelude'].andOp, orOp = NS['Prelude'].orOp, not = NS['Prelude'].not, otherwise = NS['Prelude'].otherwise, Maybe = NS['Prelude'].Maybe, maybe = NS['Prelude'].maybe, Either = NS['Prelude'].Either, either = NS['Prelude'].either, Ordering = NS['Prelude'].Ordering, String = NS['Prelude'].String, fst = NS['Prelude'].fst, snd = NS['Prelude'].snd, curry = NS['Prelude'].curry, curry_ = NS['Prelude'].curry_, uncurry = NS['Prelude'].uncurry, Eq = NS['Prelude'].Eq, Ord = NS['Prelude'].Ord, Monad = NS['Prelude'].Monad, Functor = NS['Prelude'].Functor, id = NS['Prelude'].id, const_ = NS['Prelude'].const_, compose = NS['Prelude'].compose, compose1 = NS['Prelude'].compose1, flip = NS['Prelude'].flip, call = NS['Prelude'].call, until = NS['Prelude'].until, asTypeOf = NS['Prelude'].asTypeOf, error = NS['Prelude'].error, map = NS['Prelude'].map, append = NS['Prelude'].append, filter = NS['Prelude'].filter, head = NS['Prelude'].head, last = NS['Prelude'].last, tail = NS['Prelude'].tail, init = NS['Prelude'].init, null_ = NS['Prelude'].null_, length = NS['Prelude'].length, index = NS['Prelude'].index, reverse = NS['Prelude'].reverse, foldl = NS['Prelude'].foldl, foldl1 = NS['Prelude'].foldl1, foldr = NS['Prelude'].foldr, foldr1 = NS['Prelude'].foldr1, and = NS['Prelude'].and, or = NS['Prelude'].or, any = NS['Prelude'].any, all = NS['Prelude'].all, sum = NS['Prelude'].sum, product = NS['Prelude'].product, concat = NS['Prelude'].concat, concatMap = NS['Prelude'].concatMap, maximum = NS['Prelude'].maximum, minimum = NS['Prelude'].minimum, scanl = NS['Prelude'].scanl, scanl1 = NS['Prelude'].scanl1, scanr = NS['Prelude'].scanr, scanr1 = NS['Prelude'].scanr1, replicate = NS['Prelude'].replicate, take = NS['Prelude'].take, drop = NS['Prelude'].drop, splitAt = NS['Prelude'].splitAt, takeWhile = NS['Prelude'].takeWhile, dropWhile = NS['Prelude'].dropWhile, span = NS['Prelude'].span, break_ = NS['Prelude'].break_, elem = NS['Prelude'].elem, notElem = NS['Prelude'].notElem, lookup = NS['Prelude'].lookup, zip = NS['Prelude'].zip, zip3 = NS['Prelude'].zip3, zipWith = NS['Prelude'].zipWith, zipWith3 = NS['Prelude'].zipWith3, unzip = NS['Prelude'].unzip, unzip3 = NS['Prelude'].unzip3, lines = NS['Prelude'].lines, words = NS['Prelude'].words, unlines = NS['Prelude'].unlines, unwords = NS['Prelude'].unwords, Show = NS['Prelude'].Show;;var sequence = NS['Text_Parsec'].sequence, run = NS['Text_Parsec'].run, Parser = NS['Text_Parsec'].Parser, ParseState = NS['Text_Parsec'].ParseState, ps = NS['Text_Parsec'].ps, toParser = NS['Text_Parsec'].toParser, unexpected = NS['Text_Parsec'].unexpected, parsecMap = NS['Text_Parsec'].parsecMap, fmap = NS['Text_Parsec'].fmap, liftM = NS['Text_Parsec'].liftM, liftM2 = NS['Text_Parsec'].liftM2, liftM3 = NS['Text_Parsec'].liftM3, liftA = NS['Text_Parsec'].liftA, liftA2 = NS['Text_Parsec'].liftA2, liftA3 = NS['Text_Parsec'].liftA3, ap = NS['Text_Parsec'].ap, parserBind = NS['Text_Parsec'].parserBind, parserReturn = NS['Text_Parsec'].parserReturn, return_ = NS['Text_Parsec'].return_, pure = NS['Text_Parsec'].pure, parserFail = NS['Text_Parsec'].parserFail, fail = NS['Text_Parsec'].fail, parserZero = NS['Text_Parsec'].parserZero, mzero = NS['Text_Parsec'].mzero, empty = NS['Text_Parsec'].empty, parserPlus = NS['Text_Parsec'].parserPlus, parserPlusN = NS['Text_Parsec'].parserPlusN, mplus = NS['Text_Parsec'].mplus, do_ = NS['Text_Parsec'].do_, do$ = NS['Text_Parsec'].do$, do2 = NS['Text_Parsec'].do2, bind = NS['Text_Parsec'].bind, ret = NS['Text_Parsec'].ret, withBound = NS['Text_Parsec'].withBound, returnCall = NS['Text_Parsec'].returnCall, getPosition = NS['Text_Parsec'].getPosition, setPosition = NS['Text_Parsec'].setPosition, getParserState = NS['Text_Parsec'].getParserState, setParserState = NS['Text_Parsec'].setParserState, tokens = NS['Text_Parsec'].tokens, many = NS['Text_Parsec'].many, many1 = NS['Text_Parsec'].many1, string = NS['Text_Parsec'].string, char_ = NS['Text_Parsec'].char_, satisfy = NS['Text_Parsec'].satisfy, label = NS['Text_Parsec'].label, try_ = NS['Text_Parsec'].try_, skipMany = NS['Text_Parsec'].skipMany, match = NS['Text_Parsec'].match, withScope = NS['Text_Parsec'].withScope, ex = NS['Text_Parsec'].ex, oneOf = NS['Text_Parsec'].oneOf, noneOf = NS['Text_Parsec'].noneOf, space = NS['Text_Parsec'].space, spaces = NS['Text_Parsec'].spaces, newline = NS['Text_Parsec'].newline, tab = NS['Text_Parsec'].tab, upper = NS['Text_Parsec'].upper, lower = NS['Text_Parsec'].lower, alphaNum = NS['Text_Parsec'].alphaNum, letter = NS['Text_Parsec'].letter, digit = NS['Text_Parsec'].digit, hexDigit = NS['Text_Parsec'].hexDigit, octDigit = NS['Text_Parsec'].octDigit, anyChar = NS['Text_Parsec'].anyChar, choice = NS['Text_Parsec'].choice, count = NS['Text_Parsec'].count, between = NS['Text_Parsec'].between, option = NS['Text_Parsec'].option, optionMaybe = NS['Text_Parsec'].optionMaybe, optional = NS['Text_Parsec'].optional, skipMany1 = NS['Text_Parsec'].skipMany1, sepBy = NS['Text_Parsec'].sepBy, sepBy1 = NS['Text_Parsec'].sepBy1, endBy = NS['Text_Parsec'].endBy, endBy1 = NS['Text_Parsec'].endBy1, sepEndBy = NS['Text_Parsec'].sepEndBy, sepEndBy1 = NS['Text_Parsec'].sepEndBy1, chainl = NS['Text_Parsec'].chainl, chainl1 = NS['Text_Parsec'].chainl1, chainr = NS['Text_Parsec'].chainr, chainr1 = NS['Text_Parsec'].chainr1, eof = NS['Text_Parsec'].eof, notFollowedBy = NS['Text_Parsec'].notFollowedBy, manyTill = NS['Text_Parsec'].manyTill, lookAhead = NS['Text_Parsec'].lookAhead, anyToken = NS['Text_Parsec'].anyToken, GenLanguageDef = NS['Text_Parsec'].GenLanguageDef, GenTokenParser = NS['Text_Parsec'].GenTokenParser, makeTokenParser = NS['Text_Parsec'].makeTokenParser, emptyDef = NS['Text_Parsec'].emptyDef, haskellStyle = NS['Text_Parsec'].haskellStyle, javaStyle = NS['Text_Parsec'].javaStyle, haskellDef = NS['Text_Parsec'].haskellDef, mondrianDef = NS['Text_Parsec'].mondrianDef, getHaskell = NS['Text_Parsec'].getHaskell, getMondrian = NS['Text_Parsec'].getMondrian, Assoc = NS['Text_Parsec'].Assoc, Operator = NS['Text_Parsec'].Operator, buildExpressionParser = NS['Text_Parsec'].buildExpressionParser;;var Bool = NS['Haskell'].Bool, isArray = NS['Haskell'].isArray, isDefined = NS['Haskell'].isDefined, slice = NS['Haskell'].slice, imap = NS['Haskell'].imap, ifilter = NS['Haskell'].ifilter, indexOf = NS['Haskell'].indexOf, lastIndexOf = NS['Haskell'].lastIndexOf, isort = NS['Haskell'].isort, range = NS['Haskell'].range, extend = NS['Haskell'].extend, namespace = NS['Haskell'].namespace, typeOf = NS['Haskell'].typeOf, strictEq = NS['Haskell'].strictEq, strictNe = NS['Haskell'].strictNe, unsafeAdd = NS['Haskell'].unsafeAdd, unsafeSub = NS['Haskell'].unsafeSub, unsafeMul = NS['Haskell'].unsafeMul, unsafeDiv = NS['Haskell'].unsafeDiv, lt = NS['Haskell'].lt, le = NS['Haskell'].le, gt = NS['Haskell'].gt, ge = NS['Haskell'].ge, negate = NS['Haskell'].negate, evalThunks = NS['Haskell'].evalThunks, toArray = NS['Haskell'].toArray, curry = NS['Haskell'].curry, error = NS['Haskell'].error, data = NS['Haskell'].data, ADT = NS['Haskell'].ADT, record = NS['Haskell'].record, accessor = NS['Haskell'].accessor, accessors = NS['Haskell'].accessors, typeclass = NS['Haskell'].typeclass, VARARG = NS['Haskell'].VARARG, instance = NS['Haskell'].instance, getInstance = NS['Haskell'].getInstance, asTypeOf = NS['Haskell'].asTypeOf, operators = NS['Haskell'].operators, infix = NS['Haskell'].infix, infixl = NS['Haskell'].infixl, infixr = NS['Haskell'].infixr, op = NS['Haskell'].op, no = NS['Haskell'].no, resolve = NS['Haskell'].resolve, recurse = NS['Haskell'].recurse, Recurse = NS['Haskell'].Recurse, exl = NS['Haskell'].exl, exs = NS['Haskell'].exs, createDo = NS['Haskell'].createDo, Scope = NS['Haskell'].Scope;;var String = NS['Data_Char'].String, ord = NS['Data_Char'].ord, chr = NS['Data_Char'].chr, isSpace = NS['Data_Char'].isSpace, isUpper = NS['Data_Char'].isUpper, isLower = NS['Data_Char'].isLower, isAlpha = NS['Data_Char'].isAlpha, isAlphaNum = NS['Data_Char'].isAlphaNum, isDigit = NS['Data_Char'].isDigit, isHexDigit = NS['Data_Char'].isHexDigit, isOctDigit = NS['Data_Char'].isOctDigit, toUpper = NS['Data_Char'].toUpper, toLower = NS['Data_Char'].toLower, toTitle = NS['Data_Char'].toTitle, digitToInt = NS['Data_Char'].digitToInt;
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
/// <reference path="../../../../base/src/Data/Char.js" local />
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


function dropWhile(p, a){ //TODO
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
/// <reference path="../../../jsparsec/src/Text/Parsec.js" />

namespace("BrownPLT_JavaScript")
importSubmodules("BrownPLT_JavaScript",
    ["Lexer"
    ,"Syntax"
    ,"Parser"
    ])


}());
}());
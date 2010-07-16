/// <reference path="Main.js" />

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
})
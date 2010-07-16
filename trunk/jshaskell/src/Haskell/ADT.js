/// <reference path="Main.js" />

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
});
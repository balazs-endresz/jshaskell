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
})
/// <reference path="../../../jshaskell/src/Haskell.js" local />

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
})
/// <reference path="../../../jshaskell/src/Haskell.js" local />
/// <reference path="Base.js" local />

//moved here to simplifiy dependencies (GHC.List)

//data  Maybe a  =  Nothing | Just a
//  deriving (Eq, Ord)
function Maybe(){}
data(Maybe, [["Just", "a"], "Nothing"]);

namespace("GHC_Maybe", {
     Maybe       : Maybe
})
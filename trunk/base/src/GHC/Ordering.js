/// <reference path="../../../jshaskell/src/Haskell.js" local />

function Ordering(){}
data(Ordering, ["LT", "EQ", "GT"]);

namespace("GHC_Ordering", {
    Ordering : Ordering
})
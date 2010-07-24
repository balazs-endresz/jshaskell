/// <reference path="../../jsparsec/src/Text/Parsec/Prim.js" local />
/// <reference path="../../pretty/src/Text/PrettyPrint/HughesPJ.js" />
/// <reference path="../../WebBits/src/BrownPLT/JavaScript.js" local />

/*

The first import brings functions from Text_Parsec_Prim to the local scope.
Only `run` is used, so all the others will be removed by the Closure compiler.

However the second import alone would be enough, because all the dependencies are
resolved during the build, which includes Text_Parsec_Prim. But then the `run` function 
can be accessed only by `NS.Text_Parsec_Prim.run` or `NS.Text_Parsec.run`.

/// <reference path="../../jshaskell/src/Haskell.js" local />
if this is included too, then all the functions from jshaskell
will be available as local variables for all the files in the current folder

*/

/*

Build:
> runHaskell build.hs example

// or by using the exact file name:
// > runHaskell build.hs example/src/parsejs.js

Run:
> d8 example-parsejs.js

// the filename is determined by the path, e.g. 
// jsparsec/src/Text/Parsec/Prim.js -> jsparsec-Text.Parsec.Prim.js

Result:
> ok / failed

// in Firebug you can see the whole AST on the console

*/
var P = NS.Text_PrettyPrint_HughesPJ;
var log = typeof console != "undefined" ? console.log : 
          typeof print != "undefined" ? print : 
          function(a){ alert(a) };

function parseThis(){
    run(parseScript, parseThis.toString(), function(result){
        log(result.state.length ? "failed" : "ok");
        log(result.ast);
        //log(P.render(javaScript(result.ast)))
    })
}

parseThis();
/// <reference path="Expression.js" />

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
    createDo: createDo
})
/*
// This code is inserted at the beginning during the build
// but it's not present in the final file.
// However the following two function should be included in the actual code as well:
function namespace(ns, obj){
    var global = function(){ return this }() || window;
    global.namespace = global.namespace || namespace;
    var NS = global["NS"] || (global["NS"] = {});
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
*/

//-----------------------------------------------

//during the build print is disabled:
var _print_ = print;
print = function(){};


//creates a namespace string, like "var nub = NS.Data_List.nub, ...",
//which is inserted in the final code, so that no eval will be needed there
function _createNS_(moduleStr){
//TODO: filter invalid identifier names
    var names = [], moduleObj = NS[moduleStr];
    for(var name in moduleObj)
        if(name.charAt(0) != "_")
            //names.push(name + " = NS." + moduleStr + "." + name);
            names.push(name + " = NS[\'" + moduleStr + "\']." + name);
    return names.length ? ";var " + names.join(", ") + ";" : ";";
}
var _moduleStrArray_ = eval(arguments[0])
var _locals_ = [];


//inserted at each import with local attribute:
//;var _ns_ = _createNS_(_moduleStrArray_.shift());_locals_.push(_ns_);eval(_ns_);

//inserted at end of the file, this data will be read by Haskell,
//and will replace each _locals_.push... statements
//print(_locals_)

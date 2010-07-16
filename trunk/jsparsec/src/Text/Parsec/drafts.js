/// <reference path="references.js" />


//var parser = do$
//    (char_, "a")
//    (lazy(function(){ return parser }), "<|>", return_, null)

var ParserMonad = getInstance(Monad, Parser);
var do_ = ParserMonad.do_
var do$ = ParserMonad.do$

//var $do = ParserMonad.$do


do_(char_("a"))

do$(char_, "a")
   ("a" ,"<-", string, "asd")

ex$()

//general expression, resolves strictly
ex$("a b c" ,"->", Maybe.Just ,"$", head, "a")
function(a){ return Maybe.Just(head(arr)) }

var ex = ex$.asType(Parser);

ex$.asType(Parser)(Maybe.Just ,"$", head, "a")

curry()

function FromTo(){}
var to = new FromTo();
var __ = to;
function List(args){
    this.args = args;
    this.gap = indexOf(args, __);
    this.elems = [];
    for(var i = 0; i < this.gap; ++i)
        this.elems[i] = args[i];
    
    if(args.length && (args.length > this.gap)){
        this.last = args[args.length];
        //this[0] = this.elems[0];
        //this[1] = this.elems[1];
    }
    
    
    if(this.last !== undefined)
        this.EqInstance = getInstance(Eq, args[0]);
    //TODO: check Bounded instance
    this.lastIndex = Infinity;
}
List.prototype.index = function(index){
    if((index < 0) || (index > this.lastIndex))
        error("Out of bounds");
    if(index in this.elems)
        return this.elems[index];
        
    var i = this.gap || 0,
        elem = this.args[index];
    while(i <= index){
        elem = this.elems[i] || Enum.succ(elem);
        this.elems[i] = elem;
        if((this.last !== undefined) && this.EqInstance.eq(elem, this.last)){
            this.lastIndex = i;
            if(i < index)
                error("Out of bounds");
        }
        i += 1;
    }
    return elem;
}

List.prototype.slice = function(from, to){
    var range = new Range([]);
    
    //cached elements
    var curLength = this.elems.length;
    var until = (to < curLength) ? to : curLength;
    for(var i = from, j = 0; i < until; ++i, ++j)
        list.elems[j] = this.elems[i];
    //Eq instance
    list.EqInstance = this.EqInstance;
    //last item
    list.last = to === undefined ? this.last : to;
    //last index
    list.lastIndex = this.lastIndex;
    //first few elements
    //list[0] = list.elems[0];
    //list[1] = list.elems[1];
    
    return list;
}

List.prototype.toArray = function(){
    if(this.last !== undefined)
        try{this.index(Infinity)}catch(_){}
    else
        error("Can't convert inifinite list to array");
    return slice(this.elems);
}
List.prototype.empty = list;
List.prototype.singleton = list;
List.prototype.singleton._length = 1;

function list(){ return new List(arguments)}

list(1, 2, to, 15)

instance(ListLike, List)
    ("index", function(list, index){ return list.index(list) })

//pointer equality
var PEq = typeclass("PEq", "obj")
instance(PEq, Object)

//value equality


instance(Enum, Number, {
    toEnum: id,
    fromEnum: id
})

/// <reference path="../../../../pretty/src/Text/PrettyPrint/HughesPJ.js" />
/// <reference path="../../../../jshaskell/src/Haskell.js" local />
/// <reference path="../../../../base/src/Prelude.js" local />
/// <reference path="Syntax.js" />
;(function(){
//-- |Pretty-printing JavaScript.
//module BrownPLT.JavaScript.PrettyPrint
//  ( stmt
//  , expr
//  , javaScript
//  , renderStatements
//  , renderExpression
//  ) where

//import Text.PrettyPrint.HughesPJ
//import BrownPLT.JavaScript.Syntax

//currently Text.PrettyPrint.HughesPJ is not imorted locally
var P = NS.Text_PrettyPrint_HughesPJ;
var  Doc            = P.Doc            //-- Abstract
    ,text           = P.text
    ,semi           = P.semi  
    ,comma          = P.comma
    ,colon          = P.colon
    ,equals         = P.equals
    ,lbrace         = P.lbrace
    ,rbrace         = P.rbrace
    ,parens         = P.parens
    ,brackets       = P.brackets
    ,braces         = P.braces
    ,quotes         = P.quotes
    ,doubleQuotes   = P.doubleQuotes
    ,empty          = P.empty
    ,hsep           = P.hsep
    ,vcat           = P.vcat     
    ,cat            = P.cat
    ,nest           = P.nest
    ,punctuate      = P.punctuate
    ,render         = P.render;



//renderStatements :: [Statement a] -> String
//renderStatements ss = render (semiSep ss)
function renderStatements(ss){
    return render(semiSep(ss));
}

//renderExpression :: Expression a -> String
//renderExpression e = render (expr e)
function renderExpression(e){
    return render(expr(e));
}

//-- Displays the statement in { ... }, unless it already is in a block.
//inBlock:: (Statement a) -> Doc
//inBlock s@(BlockStmt _ ss) = stmt s
//inBlock s                 = lbrace $+$ nest 2 (stmt s) $+$ rbrace
function inBlock(s){
    return s.BlockStmt ? stmt(s) : exs(lbrace ,"$+$", nest, 2, stmt(s) ,"$+$", rbrace);
}

//-- Displays the expression in ( ... ), unless it already is in parens.
//inParens:: (Expression a) -> Doc
//inParens e@(ParenExpr _ _) = expr e
//inParens e                 = parens (expr e)
function inParens(e){
    return e.ParenExpr ? expr(e) : parens(expr(e));
}

//semiSep :: [Statement a] -> Doc
//semiSep ss = vcat $ map (\s -> stmt s <> semi) ss
function semiSep(ss){
    return vcat(map(function(s){ return exs(stmt, s ,"<>", semi) }, ss));
}

//pp (Id _ str) = text str
function pp(id){
    return id.Id ? text(id[1]) : error();
}

//forInit :: ForInit a -> Doc
//forInit t = case t of
//  NoInit     -> empty
//  VarInit vs -> text "var" <+> (cat $ punctuate comma $ (map varDecl vs))
//  ExprInit e -> expr e
function forInit(t){
    return  t.NoInit   ? empty :
            t.VarInit  ? exs(text, "var" ,"<+>", [cat ,"$", punctuate, comma ,"$", map(varDecl, t[0])]) :
            t.ExprInit ? expr(t[0]) :
            error();
}

//forInInit :: ForInInit a -> Doc  
//forInInit t = case t of
//  ForInVar id   -> text "var" <+> pp id
//  ForInNoVar id -> pp id
function ForInNoVar(t){
    return  t.ForInVar   ? exs(text, "var" ,"<+>", pp(t[0])) :
            t.ForInNoVar ? pp(t[0]) :
            error();
}

//caseClause :: CaseClause a -> Doc
//caseClause (CaseClause _ e ss) =
//  text "case" $+$ expr e <+> colon $$ (nest 2 (semiSep ss))
//caseClause (CaseDefault _ ss) =
//  text "default:" $$ (nest 2 (semiSep ss))
function caseClause(a){
    return  a.CaseClause  ? exs(text, "case" ,"$+$", expr(a[1]) ,"<+>", colon ,"$$", [nest, 2, semiSep(a[2])]) : 
            a.CaseDefault ? exs(text, "default:" ,"$$", [nest, 2, semiSep(a[1])]) :
            error();
}

//catchClause :: CatchClause a -> Doc
//catchClause (CatchClause _ id s) = text "catch" <+> (parens.pp) id <+> inBlock s
function catchClause(a){
    return a.CatchClause ? exs(text, "catch" ,"<+>", parens(pp(a[1])) ,"<+>", inBlock(a[2])) : 
        error();
}

//varDecl :: VarDecl a -> Doc
//varDecl (VarDecl _ id Nothing) = pp id
//varDecl (VarDecl _ id (Just e)) = pp id <+> equals <+> expr e
function varDecl(a){
    return  a[2].Nothing ? pp(a[1]) :
            a[2].Just    ? exs(pp(a[1]) ,"<+>", equals ,"<+>", expr(a[2][0]) ) :
            error();
}

//stmt :: Statement a -> Doc
//stmt s = case s of
//  BlockStmt _ ss -> lbrace $+$ (nest 2 (semiSep ss)) $$ rbrace
//  EmptyStmt _ -> semi
//  ExprStmt _ e -> expr e
//  IfSingleStmt _ test cons -> text "if" <+> inParens test $$ stmt cons
//  IfStmt _ test cons alt ->
//    text "if" <+> inParens test $$ stmt cons $$ text "else" <+> stmt alt
//  SwitchStmt _ e cases ->
//    text "switch" <+> inParens e $$ 
//    braces (nest 2 (vcat (map caseClause cases)))
//  WhileStmt _ test body -> text "while" <+> inParens test $$ (stmt body)
//  ReturnStmt _ Nothing -> text "return"
//  ReturnStmt _ (Just e) -> text "return" <+> expr e
//  DoWhileStmt _ s e -> text "do" $$ (stmt s <+> text "while" <+> inParens e)
//  BreakStmt _ Nothing ->  text "break"
//  BreakStmt _ (Just label) -> text "break" <+> pp label
//  ContinueStmt _ Nothing -> text "continue"
//  ContinueStmt _ (Just label) -> text"continue" <+> pp label
//  LabelledStmt _ label s -> pp label <> colon $$ stmt s
//  ForInStmt p init e body -> 
//    text "for" <+> 
//    parens (forInInit init <+> text "in" <+> expr e) $+$ stmt body
//  ForStmt _ init incr test body ->
//    text "for" <+> 
//    parens (forInit init <> semi <+> mexpr incr <> semi <+> mexpr test) $$ 
//    stmt body
//  TryStmt _ stmt catches finally ->
//    text "try" $$ inBlock stmt $$ (vcat (map catchClause catches)) $$
//    ppFinally where 
//       ppFinally = case finally of
//        Nothing -> empty
//        Just stmt -> text "finally" <> inBlock stmt
//  ThrowStmt _ e -> text "throw" <+> expr e
//  WithStmt _ expr s ->  text "with" <+> inParens expr $$ stmt s
//  VarDeclStmt _ decls ->
//    text "var" <+> (cat $ punctuate comma (map varDecl decls))
//  FunctionStmt _ name args s ->
//    text "function" <+> pp name <> 
//    (parens $ cat $ punctuate comma (map pp args)) $$ 
//    inBlock s
function stmt(s){
    return s.BlockStmt    ? exs(lbrace ,"$+$", nest(2, semiSep(s[1])) ,"$$", rbrace)
    : s.EmptyStmt    ? semi
    : s.ExprStmt     ? expr(s[1])
    : s.IfSingleStmt ? exs(text, "if"     ,"<+>", inParens(s[1]) ,"$$", stmt(s[2]))
    : s.IfStmt       ? exs(text, "if"     ,"<+>", inParens(s[1]) ,"$$", stmt(s[2]) 
                                                  ,"$$", text, "else" ,"<+>", stmt(s[3]))
    : s.SwitchStmt   ? exs(text, "switch" ,"<+>", inParens(s[1]) 
                                                  ,"$$", braces(nest(2, vcat(map(caseClause, s[2])))))
    : s.WhileStmt    ? exs(text, "while"  ,"<+>", inParens(s[1]) ,"$$", stmt(s[2]))
    : s.ReturnStmt   ? s[1].Nothing ? text("return")   : exs(text, "return" ,"<+>", expr(s[1][0]))
    : s.DoWhileStmt  ? exs(text, "do" ,"$$", [stmt(s[1]) ,"<+>", text, "while" ,"<+>", inParens(s[2])])
    : s.BreakStmt    ? s[1].Nothing ? text("break")    : exs(text, "break"  ,"<+>", pp(s[1][0]))
    : s.ContinueStmt ? s[1].Nothing ? text("continue") : exs(text, "continue" ,"<+>", pp(s[1][0]))
    : s.LabelledStmt ? exs(pp(s[1]) ,"<>", colon ,"$$", stmt(s[2]))
    : s.ForInStmt    ? exs(text, "for"
                            ,"<+>", parens, [forInInit(s[1]) ,"<+>", text, "in" ,"<+>", expr(s[2])]
                            ,"$+$", stmt(s[3]))
    : s.ForStmt      ? exs(text, "for" ,"<+>", parens, 
                            [forInit(s[1]) ,"<>", semi ,"<+>", mexpr(s[2]) ,"<>", semi ,"<+>", mexpr(s[3])]
                            ,"$$", stmt(s[4]))
    : s.TryStmt      ? exs(text, "try" 
                            ,"$$", inBlock(s[1])
                            ,"$$", vcat(map(catchClause, s[2]))
                            ,"$$", ppFinally.Nothing ? empty : 
                                        finally_.Just ?
                                            exs(text, "finally" ,"<>", inBlock(finally_[0])) :
                                            error())
    : s.ThrowStmt    ? exs(text, "throw"    ,"<+>", expr(s[1]))
    : s.WithStmt     ? exs(text, "with"     ,"<+>", inParens(s[1]) ,"$$", stmt(s[2]))
    : s.VarDeclStmt  ? exs(text, "var"      ,"<+>", cat(punctuate(comma, map(varDecl, s[1]))))
    : s.FunctionStmt ? exs(text, "function" ,"<+>", pp(s[1])
                            ,"<>", parens(cat(punctuate(comma, map(pp, s[2]))))
                            ,"$$", inBlock(s[3]))
    : error();
}

//prop :: Prop a -> Doc
//prop p = case p of
//  PropId _ id -> pp id
//  PropString _ str -> doubleQuotes (text (jsEscape str))
//  PropNum _ n -> text (show n)
function prop(p){
    return  p.PropId     ? pp(p[1]) :
            p.PropString ? doubleQuotes(text(jsEscape(p[1]))) :
            p.PropNum    ? text(show(p[1])) :
            error();
}

//infixOp op = text $ case op of
//  OpMul -> "*"
//  OpDiv -> "/"
//  OpMod -> "%" 
//  OpAdd -> "+" 
//  OpSub -> "-"
//  OpLShift -> "<<"
//  OpSpRShift -> ">>"
//  OpZfRShift -> ">>>"
//  OpLT -> "<"
//  OpLEq -> "<="
//  OpGT -> ">"
//  OpGEq -> ">="
//  OpIn -> "in"
//  OpInstanceof -> "instanceof"
//  OpEq -> "=="
//  OpNEq -> "!="
//  OpStrictEq -> "==="
//  OpStrictNEq -> "!=="
//  OpBAnd -> "&"
//  OpBXor -> "^"
//  OpBOr -> "|"
//  OpLAnd -> "&&"
//  OpLOr -> "||"
var infixOpCtrToStr = {
     OpMul : "*"
    ,OpDiv : "/"
    ,OpMod : "%" 
    ,OpAdd : "+" 
    ,OpSub : "-"
    ,OpLShift : "<<"
    ,OpSpRShift : ">>"
    ,OpZfRShift : ">>>"
    ,OpLT : "<"
    ,OpLEq : "<="
    ,OpGT : ">"
    ,OpGEq : ">="
    ,OpIn : "in"
    ,OpInstanceof : "instanceof"
    ,OpEq : "=="
    ,OpNEq : "!="
    ,OpStrictEq : "==="
    ,OpStrictNEq : "!=="
    ,OpBAnd : "&"
    ,OpBXor : "^"
    ,OpBOr : "|"
    ,OpLAnd : "&&"
    ,OpLOr : "||"
};
function infixOp(op){
    return text(infixOpCtrToStr[showConstrOf(op)]);
}


//prefixOp op = text $ case op of
//  PrefixLNot -> "!"
//  PrefixBNot -> "~"
//  PrefixPlus -> "+"
//  PrefixMinus -> "-"
//  PrefixTypeof -> "typeof"
//  PrefixVoid -> "void"
//  PrefixDelete -> "delete"
var prefixOpCtrToStr = {
     PrefixLNot : "!"
    ,PrefixBNot : "~"
    ,PrefixPlus : "+"
    ,PrefixMinus : "-"
    ,PrefixTypeof : "typeof"
    ,PrefixVoid : "void"
    ,PrefixDelete : "delete"
};

function prefixOp(op){
    return text(prefixOpCtrToStr[showConstrOf(op)]);
}

//assignOp op = text $ case op of
//  OpAssign -> "="
//  OpAssignAdd -> "+="
//  OpAssignSub -> "-="
//  OpAssignMul -> "*="
//  OpAssignDiv -> "/="
//  OpAssignMod -> "%="
//  OpAssignLShift -> "<<="
//  OpAssignSpRShift -> ">>="
//  OpAssignZfRShift -> ">>>="
//  OpAssignBAnd -> "&="
//  OpAssignBXor -> "^="
//  OpAssignBOr -> "|="
var assignOpCtrToStr = {
     OpAssign : "="
    ,OpAssignAdd : "+="
    ,OpAssignSub : "-="
    ,OpAssignMul : "*="
    ,OpAssignDiv : "/="
    ,OpAssignMod : "%="
    ,OpAssignLShift : "<<="
    ,OpAssignSpRShift : ">>="
    ,OpAssignZfRShift : ">>>="
    ,OpAssignBAnd : "&="
    ,OpAssignBXor : "^="
    ,OpAssignBOr : "|="
};
function assignOp(op){
    return text(assignOpCtrToStr[showConstrOf(op)]);
}

//-- Based on:
//--   http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Guide:Literals
//jsEscape:: String -> String
//jsEscape "" = ""
//jsEscape (ch:chs) = (sel ch) ++ jsEscape chs where
//    sel '\b' = "\\b"
//    sel '\f' = "\\f"
//    sel '\n' = "\\n"
//    sel '\r' = "\\r"
//    sel '\t' = "\\t"
//    sel '\v' = "\\v"
//    sel '\'' = "\\'"
//    sel '\"' = "\\\""
//    sel '\\' = "\\\\"
//    sel x    = [x]
//    -- We don't have to do anything about \X, \x and \u escape sequences.
var escapeMap = {
     //"\\b"  : /\b/g  //this inerts \\b after every character
     "\\f"  : /\f/g
    ,"\\n"  : /\n/g
    ,"\\r"  : /\r/g
    ,"\\t"  : /\t/g
    ,"\\v"  : /\v/g
    ,"\\'"  : /\'/g
    ,"\\\"" : /\"/g
    ,"\\\\" : /\\/g
};

function jsEscape(str){
	for(var key in escapeMap )
		str = str.replace(escapeMap[key], key);
	return str;
}

//lvalue :: LValue a -> Doc
//lvalue (LVar _ x) = text x
//lvalue (LDot _ e x) = expr e <> text "." <> text x
//lvalue (LBracket _ e1 e2) = expr e1 <> brackets (expr e2)
function lvalue(lval){
    return  lval.LVar     ? text(lval[1]) :
            lval.LDot     ? exs(expr(lval[1]) ,"<>", text(".") ,"<>", text(lval[2])) :
            lval.LBracket ? exs(expr(lval[1]) ,"<>", brackets, expr(lval[2])) :
            error();
}

//expr :: Expression a -> Doc
//expr e = case e of 
//  StringLit _ str ->  doubleQuotes (text (jsEscape str))
//  RegexpLit _ re global ci -> 
//    text "/" <> text re <> text "/" <> g <> i where
//      g = if global then text "g" else empty
//      i = if ci then text "i" else empty
//  NumLit _ n ->  text (show n)
//  IntLit _ n ->  text (show n)
//  BoolLit _ True ->  text "true"
//  BoolLit _ False ->  text "false"
//  NullLit _ ->  text "null"
//  ArrayLit _ es ->  brackets $ cat $ punctuate comma (map expr es)
//  ObjectLit _ xs ->  
//    braces (hsep (punctuate comma (map pp' xs))) where
//      pp' (n,v) =  prop n <> colon <+> expr v
//  ThisRef _ ->  text "this"
//  VarRef _ id ->  pp id
//  DotRef _ e' id -> expr e' <> text "." <> pp id
//  BracketRef _ container key -> expr container <> brackets (expr key)
//  NewExpr _ constr args -> 
//    text "new" <+> expr constr <> 
//    (parens $ cat $ punctuate comma (map expr args))
//  PrefixExpr _ op e' -> prefixOp op <+> expr e'
//  InfixExpr _ op left right -> expr left <+> infixOp op <+> expr right
//  CondExpr _ test cons alt -> 
//    expr test <+> text "?" <+> expr cons <+> colon <+> expr alt
//  AssignExpr _ op l r ->  lvalue l <+> assignOp op <+> expr r
//  UnaryAssignExpr _ op e' -> case op of
//    PrefixInc -> text "++" <> lvalue e'
//    PrefixDec -> text "--" <> lvalue e'
//    PostfixInc -> lvalue e' <> text "++"
//    PostfixDec -> lvalue e' <> text "--"
//  ParenExpr _ e' ->  parens (expr e')
//  ListExpr _ es ->  cat $ punctuate comma (map expr es)
//  CallExpr _ f args -> 
//    expr f <> (parens $ cat $ punctuate comma (map expr args))
//  FuncExpr _ name args body -> 
//    text "function" <+> text (maybe "" unId name) <+>
//    (parens $ cat $ punctuate comma (map pp args)) $$ 
//    inBlock body
function expr(e){
    return e.StringLit  ? doubleQuotes(text(jsEscape(e[1])))
    : e.RegexpLit  ? exs(text("/") ,"<>", text(e[1]) ,"<>", text("/") ,"<>", e[2] ? text("g") : empty ,"<>", e[3] ? text("i") : empty)
    : e.NumLit     ? text(Show.show(e[1]))
    : e.IntLit     ? text(Show.show(e[1]))
    : e.BoolLit    ? text(e[1] ? "true" : "false")
    : e.NullLit    ? text("null")
    : e.ArrayLit   ? brackets(cat(punctuate(comma, map(expr, e[1]))))
    : e.ObjectLit  ? braces(hsep(punctuate(comma, map(function(t){
                        return exs(prop(t[0]) ,"<>", colon ,"<+>", expr(t[1]))
                     }, e[1]))))
    : e.ThisRef    ? text("this")
    : e.VarRef     ? pp(e[1])
    : e.DotRef     ? exs(expr(e[1]) ,"<>", text(".") ,"<>", pp(e[2]))
    : e.BracketRef ? exs(expr(e[1]) ,"<>", brackets(expr(e[2])))
    : e.NewExpr    ? exs(text, "new" ,"<+>", expr(e[1]) ,"<>", parens(cat(punctuate(comma, map(expr, e[2])))))
    : e.PrefixExpr ? exs(prefixOp(e[1]) ,"<+>", expr(e[2]))
    : e.InfixExpr  ? exs(expr(e[2]) ,"<+>", infixOp(e[1]) ,"<+>", expr(e[3]))
    : e.CondExpr   ? exs(expr(e[1]) ,"<+>", text("?") ,"<+>", expr(e[2]) ,"<+>", colon ,"<+>", expr(e[3]))
    : e.AssignExpr ? exs(lvalue(e[2]) ,"<+>", assignOp(e[1]) ,"<+>", expr(e[3]))
    : e.UnaryAssignExpr ? e[1].PrefixInc  ? exs(text("++") ,"<>", lvalue(e[2]))
                        : e[1].PrefixDec  ? exs(text("--") ,"<>", lvalue(e[2]))
                        : e[1].PostfixInc ? exs(lvalue(e[2]) ,"<>", text("++"))
                        : e[1].PostfixDec ? exs(lvalue(e[2]) ,"<>", text("--")) : error()
    : e.ParenExpr    ? parens(expr(e[1]))
    : e.ListExpr     ? cat(punctuate(comma, map(expr, e[1])))
    : e.CallExpr     ? exs(expr(e[1]) ,"<>", parens(cat(punctuate(comma, map(expr, e[2])))))
    : e.FuncExpr     ? exs(text, "function" ,"<+>", text, maybe("", unId, e[1]) ,"<+>",
                     parens(cat(punctuate(comma, map(pp, e[2])))) ,"$$", 
                     inBlock(e[3]))
    : error()
}

//mexpr :: Maybe (Expression a) -> Doc
//mexpr Nothing = empty
//mexpr (Just e) = expr e
function mexpr(m){
    return  m.Nothing ? empty :
            m.Just    ? expr(m[0]) :
            error();
}

//javaScript :: JavaScript a -> Doc
//javaScript (Script _ ss) = semiSep ss
function javaScript(js){
    return js.Script ? semiSep(js[1]) : error();
}

namespace("BrownPLT_JavaScript_PrettyPrint", {
     stmt             : stmt
    ,expr             : expr
    ,javaScript       : javaScript
    ,renderStatements : renderStatements
    ,renderExpression : renderExpression
})

}());

-- This Happy file is an example of a monadic parser for a C-like grammar
-- langauge. It is based on an abstract syntax and a set of rules (slightly
-- modified) written by Marco Comini, and adapted in order to word with a
-- monadic lexer.
--
-- Feel free to point out any mistake in my personal comments. Please,
-- refer to the official documentation for any further information.
--
-- Last Modified: 04/23/2015
-- by Federico Igne

{

module Main where

import AbstractSyntaxTree
import Lexer

}

  -- The '%name' statement declares the name of the parsing function that Happy
  -- will generate; you can also specify the top non-terminal symbol the parsing
  -- function will try to reach.
%name parser Program

  -- The '%monad' statement adds monadic support to the parser; simply specify
  -- the monad you are going to use.
%monad { MyMonad }

  -- The '%lexer' statement adds support for a monadic lexer. You have to give a
  -- function with the following type signature:
  --    <lexer> :: (Token -> MyMonad a) -> MyMonad a
  -- The second parameter is the EOF Token; Happy needs this in order to end
  -- the parsing process.
%lexer { lexer } { Token undefined T_EOF }

  -- The '%error' statement specify the function Happy will call whenever there's
  -- an error in the parsing process.
  -- N.B. in this case, we can wrap errors into a monadinc computation. This will
  -- allow either the lexer or the parser to throw contestual and meaninful errors.
%error { parseError }

  -- Token definition
%tokentype { Token }
%token 
 '('    { Token _ (T_RSymb "(") }
 ')'    { Token _ (T_RSymb ")") }
 '||'   { Token _ (T_RSymb "||") }
 '^^'   { Token _ (T_RSymb "^^") }
 '&&'   { Token _ (T_RSymb "&&") }
 '!'    { Token _ (T_RSymb "!") }
 '=='   { Token _ (T_RSymb "==") }
 '!='   { Token _ (T_RSymb "!=") }
 '<'    { Token _ (T_RSymb "<") }
 '<='   { Token _ (T_RSymb "<=") }
 '>'    { Token _ (T_RSymb ">") }
 '>='   { Token _ (T_RSymb ">=") }
 '+'    { Token _ (T_RSymb "+") }
 '-'    { Token _ (T_RSymb "-") }
 '*'    { Token _ (T_RSymb "*") }
 '/'    { Token _ (T_RSymb "/") }
 '%'    { Token _ (T_RSymb "%") }
 '**'   { Token _ (T_RSymb "**") }
 '++'   { Token _ (T_RSymb "++") }
 '--'   { Token _ (T_RSymb "--") }
 '['    { Token _ (T_RSymb "[") }
 ']'    { Token _ (T_RSymb "]") }
 ','    { Token _ (T_RSymb ",") }
 ';'    { Token _ (T_RSymb ";") }
 '='    { Token _ (T_RSymb "=") }
 '{'    { Token _ (T_RSymb "{") }
 '}'    { Token _ (T_RSymb "}") }
 '*='   { Token _ (T_RSymb "*=") }
 '+='   { Token _ (T_RSymb "+=") }
 '/='   { Token _ (T_RSymb "/=") }
 '-='   { Token _ (T_RSymb "-=") }
 '&='   { Token _ (T_RSymb "&=") }
 '^='   { Token _ (T_RSymb "^=") }
 '|='   { Token _ (T_RSymb "|=") }

 'for'          { Token _ (T_RWrds "for") }
 'if'           { Token _ (T_RWrds "if") }
 'else'         { Token _ (T_RWrds "else") }
 'while'        { Token _ (T_RWrds "while") }
 'repeat'       { Token _ (T_RWrds "repeat") }
 'until'        { Token _ (T_RWrds "until") }
 'break'        { Token _ (T_RWrds "break") }
 'continue'     { Token _ (T_RWrds "continue") }
 'return'       { Token _ (T_RWrds "return") }
 'bool'         { Token _ (T_RWrds "bool") }
 'char'         { Token _ (T_RWrds "char") }
 'float'        { Token _ (T_RWrds "float") }
 'int'          { Token _ (T_RWrds "int") }
 'void'         { Token _ (T_RWrds "void") }
 'val'          { Token _ (T_RWrds "val") }
 'valres'       { Token _ (T_RWrds "valres") }
 'ref'          { Token _ (T_RWrds "ref") }
 'const'        { Token _ (T_RWrds "const") }
 'res'          { Token _ (T_RWrds "res") }
 'name'         { Token _ (T_RWrds "name") }

 Ident    { Token _ (T_Ident  _) }
 Int      { Token _ (T_Int    _) }
 Double   { Token _ (T_Double _) }
 Char     { Token _ (T_Char   _) }
 String   { Token _ (T_String _) }

  -- Precedences specifications

%right THEN 'else'
%left '||' '^^'
%left '&&'
%left '!'
%nonassoc '==' '!=' '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/' '%'
%right '**'
%left NEG

%%

Id :: { Ident }
    : Ident                     { let T_Ident s = tok $1 in Ident s }

RExpr :: { RExpr }
       : RExpr '||' RExpr                   { InfixOp (BoolOp Or) $1 $3 }
       | RExpr '^^' RExpr                   { InfixOp (BoolOp Xor) $1 $3 }
       | RExpr '&&' RExpr                   { InfixOp (BoolOp And) $1 $3 }
       | '!' RExpr                          { UnaryOp Not $2 }
       | RExpr '==' RExpr                   { InfixOp (RelOp Eq) $1 $3 }
       | RExpr '!=' RExpr                   { InfixOp (RelOp Neq) $1 $3 }
       | RExpr '<'  RExpr                   { InfixOp (RelOp Lt) $1 $3 }
       | RExpr '<=' RExpr                   { InfixOp (RelOp LtE) $1 $3 }
       | RExpr '>'  RExpr                   { InfixOp (RelOp Gt) $1 $3 }
       | RExpr '>=' RExpr                   { InfixOp (RelOp GtE) $1 $3 }
       | RExpr '+'  RExpr                   { InfixOp (ArithOp Add) $1 $3 }
       | RExpr '-'  RExpr                   { InfixOp (ArithOp Sub) $1 $3 }
       | RExpr '*'  RExpr                   { InfixOp (ArithOp Mul) $1 $3 }
       | RExpr '/'  RExpr                   { InfixOp (ArithOp Div) $1 $3 }
       | RExpr '%'  RExpr                   { InfixOp (ArithOp Mod) $1 $3 }
       | RExpr '**' RExpr                   { InfixOp (ArithOp Pow) $1 $3 }
       | '-' RExpr %prec NEG                { UnaryOp Neg $2 }
       | Id '(' sep0(RExpr,',') ')'         { FCall $1 $3 }
       | Int                                { let T_Int n = tok $1 in Const (BasicType BasicTypeInt) (Int n) }
       | Char                               { let T_Char c = tok $1 in Const (BasicType BasicTypeChar) (Char c) }
       | String                             { let T_String s = tok $1 in Const (ArrayType (Just $ Const (BasicType BasicTypeInt) (Int (length s))) (BasicType BasicTypeChar)) $ Array $ str2list s }
       | '(' RExpr ')'                      { $2 }
       | LExpr                              { LasRExpr $1 }

LExpr :: { LExpr }
       : LExpr1                          { $1 } 
       | '++' LExpr1                     { PrePostIncDecr Pre Inc $2 }
       | '--' LExpr1                     { PrePostIncDecr Pre Decr $2 }

LExpr1 :: { LExpr }
        : LExpr2                         { $1 } 
        | LExpr2 '++'                    { PrePostIncDecr Post Inc $1 }
        | LExpr2 '--'                    { PrePostIncDecr Post Decr $1 }

LExpr2 :: { LExpr }
        : LExpr3                         { $1 }
        | '(' LExpr ')'                  { $2 } -- shift/reduce due to parens;

LExpr3 :: { LExpr }
        : Id                             { VarIdent $1 }
        | LExpr3 '[' RExpr ']'           { ArrayElem $1 $3 }

Program :: { Program }
         : ListDecl                     { Prog $1 } 

ListDecl :: { [Decl] }
          : {- empty -}                 { [] } 
          | ListDecl Decl               { $1 ++ $2 }

Decl :: { [Decl] }
      : TypeSpec ListVarDecl ';'                           { varDecl2Dvar $1 $2 } 
      | TypeSpec Id '(' sep0(Param,',') ')' Stmt           { [Dfun $1 $2 $4 $6] }

TypeSpec :: { TypeSpec }
          : BasicType                       { BasicType $1 }
          | TypeSpec '[' ']'                { ArrayType Nothing $1}
          | TypeSpec '[' RExpr ']'          { ArrayType (Just $3) $1}

BasicType :: { BasicType }
           : 'bool'                 { BasicTypeBool } 
           | 'char'                 { BasicTypeChar }
           | 'float'                { BasicTypeFloat }
           | 'int'                  { BasicTypeInt }
           | 'void'                 { BasicTypeVoid }

ListVarDecl :: { [VarDecl] }
             : sep1(VarDecl,',')             { $1 [] }

VarDecl :: { VarDecl }
         : Id                                { ($1, Nothing) }
         | Id '=' RExpr                      { ($1, Just $3) } 

Param :: { FormalParam }
           : TypeSpec Id                     { FormalParam Value $1 $2 } 
           | Modality TypeSpec Id            { FormalParam $1 $2 $3 } 

Modality :: { Modality }
          : 'val'                            { Value }
          | 'ref'                            { Reference }
          | 'const'                          { Constant }
          | 'res'                            { Result }
          | 'valres'                         { ValueResult }
          | 'name'                           { Name }

Stmt :: { Stmt }
      : SimpleStmt ';'                           { $1 }
      | '{' ListDecl lst0(Stmt) '}'              { Block $2 $3 }
      | SelectionStmt                            { $1 }
      | IterStmt                                 { $1 } 
      | JumpStmt                                 { $1 }

SimpleStmt :: { Stmt }
      : LExpr                                    { LExprStmt $1 }       -- ma solo se PrePostIncDecr
      | LExpr AssignOp RExpr                     { Assgn $1 $2 $3 }
      | Id '(' sep0(RExpr,',') ')'               { PCall $1 $3 }

AssignOp :: { AssignmentOp }
          : '='                 { Assign } 
          | '*='                { AssgnArith Mul }
          | '+='                { AssgnArith Add }
          | '/='                { AssgnArith Div }
          | '-='                { AssgnArith Sub }
          | '&='                { AssgnBool And }
          | '^='                { AssgnBool Xor }
          | '|='                { AssgnBool Or }

JumpStmt :: { Stmt }
          : 'break' ';'                   { Break } 
          | 'continue' ';'                { Continue }
          | 'return' ';'                  { RetExp Nothing }
          | 'return' '(' ')' ';'          { RetExp Nothing }
          | 'return' '(' RExpr ')' ';'    { RetExp $ Just $3 }
 
SelectionStmt :: { Stmt }
               : 'if' '(' RExpr ')' Stmt %prec THEN  { If $3 $5 Nothing } 
               | 'if' '(' RExpr ')' Stmt 'else' Stmt { If $3 $5 $ Just $7 }
 
IterStmt :: { Stmt }
          : 'for' '(' SimpleStmt ';' RExpr ';' SimpleStmt ')' Stmt       { For ($3,$5,$7) $ Just $9 }
          | 'for' '(' SimpleStmt ';' RExpr ';' SimpleStmt ')' ';'        { For ($3,$5,$7) Nothing }
          | 'while' '(' RExpr ')' Stmt                                   { While $3 $5 } 
          | 'repeat' Stmt 'until' '(' RExpr ')'                          { Repeat $2 $5 }


-- Parametric Productions

lst1(p) :: { [a] -> [a] }
           : p                    { ($1:) }
           | lst1(p) p            { $1 . ($2:)}

lst0(p) :: { [a] }
           : {-empty-}            { [] }
           | lst1(p)              { $1 [] }

sep1(p,q) :: { [a] -> [a] }
           : p                    { ($1:) }
           | sep1(p,q) q p        { $1 . ($3:)}

sep0(p,q) :: { [a] }
           : {-empty-}            { [] }
           | sep1(p,q)            { $1 [] }

ter1(p,q) :: { [a] -> [a] }
           : p q                  { ($1:) }
           | ter1(p,q) p q        { $1 . ($3:)}

ter0(p,q) :: { [a] }
           : {-empty-}            { [] }
           | ter1(p,q)            { $1 [] }

{

-- tokenPos function definition
-- Prints the token position in a readable way (better error handling purpose).
tokenPos Token{ pos = undefined } = "the end of file."
tokenPos Token{ pos = (Pos l c) } = "line " ++ show l ++ " column " ++ show c ++ "."

-- parseError function definition
-- The function is automatucally called by Happy if there's an error in the parsing
-- process. You can also manually call the function in the rules body.
-- parseError :: Token -> Alex a
parseError ts = fail $ "syntax error at " ++ tokenPos ts

-- lexer function definition
-- The 'lexer' function is one of the main function on the parser side. In order to
-- use it with a monadic lexer we have to change its type signature, that is:
--    lexer :: (Token -> MyMonad a) -> MyMonad a
-- It will take a continuation function that will be applied to the token,
-- returned by the lexing process. The continuation function will then return a
-- monadic value.
-- Remember that Happy will ask for a new token only when needed. What the parser is
-- going to build is a huge statefull computation that will eventually return an
-- AST (according to the production rules). This means that the lexer and the parser
-- are bound together and will work simultaneusly.

-- We will try to represent a simplified parser computation with a tree, with the
-- main non-terminal we are tring to reach as root. The leaves are the terminal tokens
-- the lexer will gives us. Asking for a new token (shift action) will cover an
-- additional leaf; Using an inverse production to build a non-terminal (reduce action)
-- will cover a new internal node (and eventually the root).

{-
         _               _               _               _               _         |
        /_\             /_\             /_\             /_\             /_\        |
       /___\           /___\           /___\           /___\           /___\       |
      /_____\  ---->  /_____\  ---->  /_____\  ---->  /_____\  ---->  /+ ___\      |
     /_______\       /_______\       /_______\       /+ _____\       /+ + ___\     |
    /_________\     /_________\     /+ _______\     /+ + _____\     /+ + + ___\    |
   /___________\   /+ _________\   /+ + _______\   /+ + + _____\   /+ + + + ___\   |
                    ^                 ^                 ^                 ^        |
-}

-- Whenever you add a new leaf, the parser is internally calling the 'lexer' function.
-- N.B. the parsing function defined in the '%name' statement will return a statefull
-- computation. Then you need to run the computation with a starting state in order
-- to have the AST back.

-- Here are three equivalent versions of a simple 'lexer' functions. You can do
-- fancy thing in there (basically you have the chance to control the computation
-- between the lexer returning the next token and the parser pushing it into the stack).

-- lexer cont = alexMonadScan >>= \token -> cont token

-- lexer cont = do 
--              token <- alexMonadScan
--              {- do fancy stuff here -}
--              cont t

lexer = (alexMonadScan >>=)

-- parse function definition
-- 'parse' is the main function. It simply runs the computation with a starting state.
-- The 's' parameter is the input string

parse s = run s parser

-- Useful functions related to grammar rules

str2list :: [Char] -> [Const]
str2list [] = []
str2list (c:ss) = (Char c) : str2list ss

varDecl2Dvar :: TypeSpec -> [VarDecl] -> [Decl]
varDecl2Dvar t = map go
  where
    go x = Dvar t (fst x) (snd x)


 -- ---------- --
 -- -- MAIN -- --
 -- ---------- --

main = do 
    x <- getContents
    case parse x of
        Left msg -> putStr msg
        Right ast -> putStr $ show ast

}

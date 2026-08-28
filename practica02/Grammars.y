{
module Grammars where

import Lexer (Token(..), lexer)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
      nat             { TokenNum $$ }
      bool            { TokenBool $$ }
      '+'             { TokenSuma }
      '-'             { TokenResta }
      '*'             { TokenMul }
      '/'             { TokenDiv }
      "and"           { TokenAnd }
      "or"            { TokenOr }
      "not"           { TokenNot }
      "add1"          { TokenAdd1 }
      "sub1"          { TokenSub1 }
      "zero?"         { TokenZeroP }
      "expt"          { TokenExpt }
      '<'             { TokenLT }
      '>'             { TokenGT }
      "<="            { TokenLE }
      ">="            { TokenGE }
      "eq"            { TokenEq }
      '('             { TokenPA }
      ')'             { TokenPC }

%%

ASA : nat                       { Num $1 }
    | bool                      { Boolean $1 }
    | '(' '+' listaDeASAs ')'   {Add $3} 
    | '(' '-' listaDeASAs ')'   {Sub $3}
    | '(' '*' listaDeASAs ')'   {Mul $3}
    | '(' '/' listaDeASAs ')'   {Div $3}
    | '(' "and" listaDeASAs ')' {And $3}
    | '(' "or" listaDeASAs ')'  {Or $3}
    | '(' "not" ASA ')'         {Not $3}
    | '(' "add1" ASA ')'        {Add1 $3}
    | '(' "sub1" ASA ')'        {Sub1 $3}
    | '(' "zero?" ASA ')'       {ZeroP $3}
    | '(' "expt" ASA ASA ')'    {Expt $3 $4}
    | '(' '<' ASA ASA ')'       {Lt $3 $4}
    | '(' '>' ASA ASA ')'       {Gt $3 $4}
    | '(' "<=" ASA ASA ')'      {Le $3 $4}
    | '(' ">=" ASA ASA ')'      {Ge $3 $4}
    | '(' "eq" ASA ASA ')'      {Eq $3 $4}

ListaDeASAs : ASA ASA           { [$1,$2] }
            | ASA listaDeASAs   { $1 : $2}

-- RETO 2:
-- Agrega las producciones para:
--   * operadores n-arios con al menos dos argumentos;
--   * operadores estrictamente binarios: expt y eq;
--   * operadores unarios: not, add1, sub1, zero?.

-- RETO 3:
-- Agrega un no terminal para representar dos o mas argumentos.
-- El resultado debe ser una lista de ASA.


{
parseError :: [Token] -> a
parseError toks = error ("Parse error: " ++ show toks)

data ASA
  = Num Int
  | Boolean Bool
  | And [ASA]
  | Or [ASA]
  | Add [ASA]
  | Sub [ASA]
  | Mul [ASA]
  | Div [ASA]
  | Lt [ASA]
  | Gt [ASA]
  | Le [ASA]
  | Ge [ASA]
  | Expt ASA ASA
  | EqP ASA ASA
  | Not ASA
  | Add1 ASA
  | Sub1 ASA
  | ZeroP ASA
  deriving (Eq, Show)
}

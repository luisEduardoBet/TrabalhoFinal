{
module Parser where

import System.IO
import Token
import qualified Lex as L
import DataTree
}

%name calc
%tokentype { Token }
%error { parseError }
%token 
  '+' {TADD}
  '-' {TSUB}
  '*' {TMUL}
  '/' {TDIV}
  '(' {TLPAR}
  ')' {TRPAR}
  '/=' {TDIF} 
  '&&' {TAND} 
  '||' {TOR} 
  '!' {TNOT}
  '==' {TEQUAL} 
  '>=' {TGET} 
  '>' {TGT} 
  '<=' {TLET} 
  '<' {TLT}
  'void' {TVOID}
  'int'  {TINT}
  'float' {TFLOAT}
  'string' {TSTRING}
  '{'  {TLB}
  '}'  {TRB}
  ','  {TCOMMA}
  ';'  {TSEMI}
  'return' {TRET}
  'if' {TIF}
  'else' {TELSE}             
  'while' {TWHILE}     
  '='  {TATRIB}
  'print' {TPRINT}
  'read' {TREAD}
  
  Int {CINT $$}
  Float {CFLOAT $$}
  Literal {CLITERAL $$}     
  Id   {CID $$}

%%

Bloco: '{' ListaCmd '}'           {[$2]}


ListaCmd: Comando    {$1}

Comando: CmdSe  {$1}
       | CmdEscrita {$1}

CmdSe: 'if' '(' ExpressaoLogica ')' Bloco                            {If $3 $5 $5}
     | 'if'  '(' ExpressaoLogica ')' Bloco  'else'  Bloco            {If $3 $5 $7} 



CmdEscrita: 'print' '(' ExpressaoAritmetica ')' ';'     {Imp $3} 


ExpressaoLogica: ExpressaoLogica '&&' LTermo       {And $1 $3}
               | ExpressaoLogica '||' LTermo       {Or $1 $3}
               | LTermo                                 {$1}

LTermo : '!' LFator         {Not $2}
       | LFator             {$1}

LFator : '(' ExpressaoLogica ')'   {$2}
       | ExpressaoRelacional       {Rel $1}

ExpressaoRelacional : ExpressaoAritmetica '==' ExpressaoAritmetica {Req $1 $3}
                    | ExpressaoAritmetica '>=' ExpressaoAritmetica {Rge $1 $3}
                    | ExpressaoAritmetica '>' ExpressaoAritmetica  {Rgt $1 $3}
                    | ExpressaoAritmetica '<' ExpressaoAritmetica  {Rlt $1 $3}
                    | ExpressaoAritmetica '<=' ExpressaoAritmetica {Rle $1 $3}
                    | ExpressaoAritmetica '/=' ExpressaoAritmetica {Rdif $1 $3}


ExpressaoAritmetica: ExpressaoAritmetica '+' Term  {Add $1 $3}
                   | ExpressaoAritmetica '-' Term  {Sub $1 $3}
                   | Term {$1}

Term  : Term  '*' Factor    {Mul $1 $3}
      | Term '/' Factor     {Div $1 $3}
      | Factor              {$1}

Factor : Int                               {Const (CInt $1)}
       | Float                             {Const (CDouble $1)}
       | Id                                {IdVar $1}
       | '(' ExpressaoAritmetica ')'       {$2}      


{
parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)

main = do
       handle <- openFile "texto.txt" ReadMode
       contents <- hGetContents handle
       print(calc(L.alexScanTokens contents)) 
       hClose handle
}
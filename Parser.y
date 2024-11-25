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
Programa : ListaFuncoes BlocoPrincipal {Prog (map fst $1) (map t1 $1) (fst $2) (snd $2) }
       | BlocoPrincipal  {Prog[] [] (fst $1) (snd $1)}

ListaFuncoes : ListaFuncoes Funcao        {$1 ++ [$2]}
             | Funcao                     {[$1]}

Funcao : TipoRetorno Id '(' DeclParametros ')' BlocoPrincipal {($2:->:($4,$1), $6)}   
       | TipoRetorno Id '(' ')' BlocoPrincipal {($2 :->:([],$1), $5)}                       

TipoRetorno: Tipo           {$1}
           | 'void'         {TVoid}

DeclParametros: DeclParametros ',' Parametro     {$1 ++ [$3]}
              | Parametro                        {[$1]}

Parametro:  Tipo Id         {$2:#:($1, 0)}


BlocoPrincipal: '{' Declaracoes ListaCmd'}'      {($2, $3)}
              | '{' ListaCmd '}'                 {([], $2)}


Declaracoes: Declaracoes Declaracao {$1++$2}
           | Declaracao {$1}

Declaracao : Tipo ListaId ';'      {map (\x -> x:#: ($1,0)) $2}

Tipo: 'int'                 {TInt}
    | 'string'              {TString} 
    | 'float'               {TDouble}

ListaId: ListaId ',' Id            {$1 ++ [$3]}
       | Id                        {[$1]}


Bloco: '{' ListaCmd '}'           {$2}


ListaCmd: ListaCmd Comando    {$1++[$2]}
       | Comando {[$1]}     

Comando: CmdSe  {$1}
       | CmdEnquanto {$1}
       | CmdAtrib {$1}
       | CmdEscrita {$1}
       | CmdLeitura {$1}
       | ChamadaProc {$1}
       | Retorno {$1}


CmdSe: 'if' '(' ExpressaoLogica ')' Bloco                            {If $3 $5 []}
     | 'if'  '(' ExpressaoLogica ')' Bloco  'else'  Bloco            {If $3 $5 $7} 


CmdEnquanto:  'while' '(' ExpressaoLogica ')' Bloco  {While $3 $5}


CmdAtrib: Id '=' ExpressaoAritmetica ';'         {Atrib $1 $3}
        | Id '=' Literal ';'                     {Atrib $1 (Lit $3)}


CmdEscrita: 'print' '(' ExpressaoAritmetica ')' ';'     {Imp $3} 
       |    'print' '(' Literal ')' ';'                 {Imp (Lit $3)}  

CmdLeitura: 'read' '(' Id ')' ';'                {Leitura $3}


ChamadaProc: ChamadaFuncao ';'                   {Proc (fst $1) (snd $1)}

ChamadaFuncao: Id '(' ListaParametros ')'        {($1,$3)}
             | Id '(' ')'                        {($1,[])}


ListaParametros: ListaParametros ',' ExpressaoAritmetica       {$1 ++ [$3]}
               | ListaParametros ',' Literal                   {$1 ++ [Lit $3]} 
               | Literal                                        {[(Lit $1)]} 
               | ExpressaoAritmetica                            {[$1]}
               

Retorno: 'return' ExpressaoAritmetica ';'        {Ret (Just $2)}
       | 'return' Literal ';'                    {Ret (Just (Lit $2))}
       | 'return' ';'                            {Ret (Nothing)}

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



ExpressaoAritmetica: '-' ExpressaoAritmetica {Neg $2}
                     | Expressao {$1}

Expressao : Expressao '+' Term     {Add $1 $3}
       | Expressao '-' Term        {Sub $1 $3}
       | Term                      {$1}

Term  : Term  '*' Factor    {Mul $1 $3}
      | Term '/' Factor     {Div $1 $3}
      | Factor              {$1}

Factor : Int                               {Const (CInt $1)}
       | Float                             {Const (CDouble $1)}
       | Id                                {IdVar $1}
       | ChamadaFuncao                     {Chama (fst $1) (snd $1)}
       | '(' ExpressaoAritmetica ')'       {$2}      

{
parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)

main = do
       content <- readFile "texto.txt"
       let parse = calc(L.alexScanTokens content)
       writeFile "Parsed1.txt" (show $ parse ) 
}
{
module Parser where

import System.IO
import Token
import qualified Lex as L
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

Programa : ListaFuncoes BlocoPrincipal    {}
         | BlocoPrincipal                 {}

ListaFuncoes : ListaFuncoes Funcao        {}
             | Funcao                     {}

Funcao : TipoRetorno Id '(' DeclParametros ')' BlocoPrincipal         {}
       | TipoRetorno Id '(' ')' BlocoPrincipal                        {}

TipoRetorno: Tipo           {}
           | 'void'         {}

DeclParametros: DeclParametros ',' Parametro     {}
              | Parametro                        {}

Parametro:  Tipo Id         {}

BlocoPrincipal: '{' Declaracoes ListaCmd '}'     {}
              | '{' ListaCmd '}'                 {}

Declaracoes: Declaracoes Declaracao       {}
           | Declaracao                   {}

Declaracao : Tipo ListaId ';'      {}

Tipo: 'int'                 {}
    | 'string'              {} 
    | 'float'               {}

ListaId: ListaId ',' Id            {}
       | Id                        {}

Bloco: '{' ListaCmd '}'            {}

ListaCmd: ListaCmd Comando         {}
        | Comando                  {}

Comando: CmdSe              {}
       | CmdEnquanto        {}
       | CmdAtrib           {}
       | CmdEscrita         {}
       | CmdLeitura         {}
       | ChamadaProc        {}
       | Retorno            {}

Retorno: 'return' ExpressaoAritmetica ';'        {}
       | 'return' Literal ';'                    {}
       | 'return' ';'                            {}

CmdSe: 'if' '(' ExpressaoLogica ')' Bloco                      {}
     | 'if'  '(' ExpressaoLogica ')' Bloco 'else' Bloco         {} 

CmdEnquanto: 'while' '(' ExpressaoLogica ')' Bloco             {}

CmdAtrib: Id '=' ExpressaoAritmetica ';'         {}
        | Id '=' Literal ';'                     {}

CmdEscrita: 'print' '(' ExpressaoAritmetica ')' ';'     {}
          | 'print' '(' Literal ')' ';'                 {}   

CmdLeitura: 'read' '(' Id ')' ';'                {}

ChamadaProc: ChamadaFuncao ';'                   {}

ChamadaFuncao: Id '(' ListaParametros ')'        {}
             | Id '(' ')'                        {}

ListaParametros: ListaParametros ',' ExpressaoAritmetica       {}
               | ListaParametros ',' Literal                   {} 
               | ExpressaoAritmetica                           {} 
               | Literal                                       {}


ExpressaoLogica: ExpressaoLogica '&&' LTermo      {}
               | ExpressaoLogica '||' LTermo      {}
               | LTermo                                 {}

LTermo : '!' LFator         {}
       | LFator             {}

LFator : '(' ExpressaoLogica ')'   {}
       | ExpressaoRelacional       {}

ExpressaoRelacional : ExpressaoAritmetica '==' ExpressaoAritmetica {}
                    | ExpressaoAritmetica '>=' ExpressaoAritmetica {}
                    | ExpressaoAritmetica '>' ExpressaoAritmetica  {}
                    | ExpressaoAritmetica '<' ExpressaoAritmetica  {}
                    | ExpressaoAritmetica '<=' ExpressaoAritmetica {}
                    | ExpressaoAritmetica '/=' ExpressaoAritmetica {}


ExpressaoAritmetica: ExpressaoAritmetica '+' Term  {}
                   | ExpressaoAritmetica '-' Term  {}
                   | Term {}

Term  : Term  '*' Factor    {}
      | Term '/' Factor     {}
      | Factor              {}

Factor : Int                               {}
       | Float                             {}
       | Id                                {}
       | '(' ExpressaoAritmetica ')'       {}      


{
parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)

main = do
       handle <- openFile "texto.txt" ReadMode
       contents <- hGetContents handle
       print(calc(L.alexScanTokens contents)) 
       hClose handle
}
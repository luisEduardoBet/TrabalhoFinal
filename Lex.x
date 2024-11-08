{
module Lex where

import Token
}

%wrapper "basic"

$digit = [0-9]          
$alpha = [A-Za-z]

@int = $digit+
@float = $digit+(\.$digit+)
@id = [$alpha][$alpha | $digit]*
@literal = \" ($printable # \") \"  

tokens :-

<0> $white+ ;
<0> @id {\s -> CID (read s)}
<0> @literal {\s -> CLITERAL (read s)}
<0> @float {\s -> CFLOAT (read s)}
<0> @int {\s -> CINT (read s)}
<0> "+" {\s -> TADD}  
<0> "-" {\s -> TSUB}  
<0> "*" {\s -> TMUL}
<0> "/="{\s -> TDIF}  
<0> "/" {\s -> TDIV}  
<0> "(" {\s -> TLPAR}  
<0> ")" {\s -> TRPAR}
<0> "&&" {\s -> TAND} 
<0> "||" {\s-> TOR}
<0> "!" {\s -> TNOT}
<0> "==" {\s -> TEQUAL} 
<0> ">" {\s -> TGT}
<0> ">=" {\s -> TGET} 
<0> "<" {\s -> TLT}    
<0> "<=" {\s -> TLET} 
<0> "void" {\s -> TVOID} 
<0> "int" {\s -> TINT} 
<0> "string" {\s -> TSTRING} 
<0> "float" {\s -> TFLOAT}
<0> "{" {\s -> TLB}
<0> "}" {\s -> TRB}
<0> "," {\s -> TCOMMA}
<0> ";" {\s -> TSEMI}
<0> "return" {\s -> TRET}
<0> "if" {\s -> TIF}
<0> "else" {\s -> TELSE}
<0> "while" {\s -> TWHILE}
<0> "=" {\s -> TATRIB}
<0> "print" {\s -> TPRINT}
<0> "read" {\s -> TREAD}

{
-- As acoes tem tipo :: String -> Token

testLex = do s <- getLine
             print (alexScanTokens s)
}

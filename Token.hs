module Token where

data Token
  = CFLOAT Double
  | CLITERAL ByteString
  | CID ByteString
  | CINT Integer
  | ADD
  | SUB
  | MUL
  | DIV
  | TLPAR
  | TRPAR
  | TAND
  | TOR
  | TNOT
  | TEQUAL
  | TGT
  | TGET
  | TLT
  | TLET
  | TVOID
  | TINT
  | TSTRING
  | TFLOAT
  | TLB
  | TRB
  | TCOMMA
  | TSEMI
  | TRET
  | TIF
  | TELSE
  | TWHILE
  | TATRIB
  | TPRINT
  | TREAD
  deriving (Eq, Show)
  

module Token where

data Token
  = CFLOAT Double
  | CLITERAL String
  | CID String
  | CINT Int
  | TADD
  | TSUB
  | TMUL
  | TDIF
  | TDIV
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
  

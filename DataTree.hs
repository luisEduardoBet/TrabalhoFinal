module DataTree where

type Id =  String 

data Tipo = TDouble | TInt | TString | TVoid deriving (Read,Show, Eq)

data TCons = CDouble Double | CInt Int deriving (Read, Show)

data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Neg Expr | Const TCons | IdVar String | Chama Id [Expr] | Lit String | IntDouble Expr | DoubleInt Expr deriving (Read, Show)

data ExprR = Req Expr Expr | Rdif Expr Expr | Rlt Expr Expr | Rgt Expr Expr | Rle Expr Expr | Rge Expr Expr deriving (Read, Show)

data ExprL = And ExprL ExprL | Or ExprL ExprL | Not ExprL | Rel ExprR deriving (Read, Show)

data Var = Id :#: (Tipo, Int) deriving (Read, Show)

data Funcao =  Id :->: ([Var], Tipo) deriving (Read, Show)

data Programa = Prog[Funcao] [(Id, [Var], Bloco)] [Var] Bloco deriving (Read, Show)

type Bloco = [Comando]

data Comando = 
    If ExprL Bloco Bloco
    | While ExprL Bloco
    | Atrib Id Expr
    | Leitura Id 
    | Imp Expr
    | Ret (Maybe Expr)
    | Proc Id [Expr]
        deriving (Read, Show)

t1:: (Funcao, ([Var], Bloco)) -> (Id, [Var], Bloco) 
t1 (a:->:as,(b,c)) = (a, (fst as) ++ b,c)  
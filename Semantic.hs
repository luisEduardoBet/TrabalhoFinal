module Semantic where

import DataTree
import System.IO
import Data.Typeable

-- Monada para gerenciar mensagens de erro e advertência
data M a = MS (String, a) deriving Show

instance Functor M where
    fmap f (MS (s, a)) = MS (s, f a)

instance Applicative M where
    pure a = MS ("", a)
    MS (s1, f) <*> MS (s2, x) = MS (s1 ++ s2, f x)

instance Monad M where
    MS (s, a) >>= f =
        let MS (s', b) = f a
        in MS (s ++ s', b)

-----------------------------------------------------------------

erro :: String -> M ()
erro s = MS ("Erro: " ++ s ++ "\n", ())

adv :: String -> M ()
adv s = MS ("Advertência: " ++ s ++ "\n", ())

-------------------------------------------------------------

test:: Programa -> IO()
test (Prog funcoes blocoFuncoes vars bloco) = do
    print(funcoes)



main2 = do 
    handle <- openFile "Parsed1.txt" ReadMode
    contents <- hGetContents handle
    test(read contents )
    hClose handle
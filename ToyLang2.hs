{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module ToyLang2 where
{- See https://stackoverflow.com/questions/57680170/tagless-final-dsl-with-rvalue-lvalue-problems for details

-}

import           Control.Monad.Writer.Lazy
import           Data.Monoid               ((<>))

data L -- empty
data R -- empty

data Expr lr t where
    Var :: String -> t -> Expr L t
    Num :: t -> Expr R t

class Monad m => ToyLang m t | m -> t where
    comment :: String -> m ()
    int :: String -> m (Expr L t)
    (.=) :: Expr L t -> Expr lr t -> m (Expr L t)
    (.+) :: Expr lr t -> Expr lr' t -> m (Expr R t)
    ret :: Expr lr t -> m t
    end :: m ()

infix 1 .=
infixl 6 .+

{-
toVal :: Expr lr -> Maybe Int
toVal (Var n v) = v
toVal (Num i)   = i
-}

toString :: (Show t) => Expr lr t -> String
toString (Var n v) = n
toString (Num v)   = show v


instance ToyLang (Writer String) String where
    comment s = void $ tell ("# " <> s <> "\n")
    int n = do
        tell $ "var " <> n <> "\n"
        return $ Var n ("var " <> n <> "\n")
    (.=) (Var n _) e = do
        tell $ n <> " = " <> toString e <> "\n"
        return $ Var n (n <> " = " <> toString e <> "\n")
    (.+) e1 e2 = do
        tell $ toString e1 <> " + " <> toString e2
        return $ Num (toString e1 <> " + " <> toString e2)
    ret (Var n v) = do
        tell $ "ret " <> n <> "\n"
        return v
    ret (Num i) = do
        tell $ "ret " <> show (Just i)
        return i
    end = void $ tell "end"


toy :: ToyLang m t => m t
toy = do
    comment "set a = 1"
    a <- int "a"
    a .= Num 1
--    a .= (a .+ num 1)  
    ret a


render :: Writer String a -> String
render = execWriter

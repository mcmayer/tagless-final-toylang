
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ToyLang where
{- Details in -}

import           Control.Monad.Writer.Lazy
import           Data.Monoid               ((<>))

data L -- empty
data R -- empty

data Expr lr where
    Var :: String -> Maybe Int -> Expr L
    Num :: Maybe Int -> Expr R

class Monad m => ToyLang m where
    comment :: String -> m ()
    int :: String -> m (Expr L)
    (.=) :: Expr L -> Expr lr -> m (Expr L)
    (.+) :: Expr lr -> Expr lr' -> Expr R
    ret :: Expr lr -> m (Maybe Int)
    end :: m ()

infix 1 .=
infixl 6 .+

num :: Int -> Expr R
num = Num . Just

toVal :: Expr lr -> Maybe Int
toVal (Var n mi) = mi
toVal (Num i)    = i

toString :: Expr lr -> String
toString (Var n mi) = n
toString (Num i)    = show i


instance ToyLang (Writer String) where
    comment s = void $ tell ("# " <> s <> "\n")
    int n = do
        tell $ "var " <> n <> "\n"
        return (Var n Nothing)
    (.=) (Var n _) e = do
        tell $ n <> " = " <> toString e <> "\n"
        return $ Var n (toVal e)
    (.+) e1 e2 = Num ((+) <$> toVal e1 <*> toVal e2)
    ret (Var n mi) = do
        tell $ "ret " <> n <> "\n"
        return mi
    ret (Num i) = do
        tell $ "ret " <> show (Just i)
        return i
    end = void $ tell "end"


toy :: ToyLang m => m (Maybe Int)
toy = do
    comment "set a = 1"
    a <- int "a"
    a .= num 1
    a .= a .+ num 1
    ret a


render :: Writer String a -> String
render = execWriter

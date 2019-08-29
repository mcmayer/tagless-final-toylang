{-# LANGUAGE OverloadedStrings #-}

module Main (main)
where

import           Data.Monoid ((<>))
import           ToyLang2

main :: IO ()
main = putStrLn $ render toy

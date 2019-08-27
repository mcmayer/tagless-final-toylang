{-# LANGUAGE OverloadedStrings #-}

module Main (main)
where

import           Data.Monoid ((<>))
import           ToyLang

main :: IO ()
main = putStrLn $ render toy

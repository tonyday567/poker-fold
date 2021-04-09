{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import NumHask.Prelude
import Test.DocTest

main :: IO ()
main =
  doctest
  [ "src/Poker/Types.hs",
    "src/Poker/Random.hs",
    "src/Poker.hs"
  ]

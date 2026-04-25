module Main where

import Test.Tasty
import LexerSpec
import ParserSpec

main :: IO ()
main =
  defaultMain $
    testGroup "SL Compiler Tests"
      [ lexerTests
      , parserTests
      ]

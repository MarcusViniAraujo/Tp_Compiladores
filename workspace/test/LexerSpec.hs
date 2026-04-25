module LexerSpec (lexerTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Frontend.Lexer.SlLexer
import Frontend.Lexer.Token

lexerTests :: TestTree
lexerTests = testGroup "Lexer tests"
    [ testCase "integer literal" $
        lexer "123"
        @?= Right [ Token (1,1) (TokInt 123)
                , Token (1,4) TokEOF
                ]

    , testCase "identifier" $
        lexer "abc"
            @?= Right [ Token (1,1) (TokId "abc")
                , Token (1,4) TokEOF
                ]

    , testCase "keyword vs id" $
        lexer "if"
            @?= Right [ Token (1,1) KW_If
                , Token (1,3) TokEOF
                ]
  
    , testCase "float and arithmetic operators" $
        lexer "3.14 + 5.0 * x"
            @?= Right [ Token (1,1) (TokFloat 3.14)
                  , Token (1,6) TokPlus
                  , Token (1,8) (TokFloat 5.0)
                  , Token (1,12) TokTimes
                  , Token (1,14) (TokId "x")
                  , Token (1,15) TokEOF
                  ]

    , testCase "string and comparison operators" $
        lexer "\"hello\" == \"world\""
            @?= Right [ Token (1,1) (TokString "hello")
                  , Token (1,9) TokEq
                  , Token (1,12) (TokString "world")
                  , Token (1,19) TokEOF
                  ]

    , testCase "logical operators and symbols" $
        lexer "x < 10 || y >= 2"
            @?= Right [ Token (1,1) (TokId "x")
                  , Token (1,3) TokLt
                  , Token (1,5) (TokInt 10)
                  , Token (1,8) TokOr
                  , Token (1,11) (TokId "y")
                  , Token (1,13) TokGeq
                  , Token (1,16) (TokInt 2)
                  , Token (1,17) TokEOF
                  ]

    , testCase "nested multiline comment" $
        lexer "/* start /* inner */ end */ code"
            @?= Right [ Token (1,29) (TokId "code")
                  , Token (1,33) TokEOF
                  ]

    , testCase "new, size, and function type arrow" $
        lexer "new int[] -> void"
            @?= Right [ Token (1,1) KW_New
                , Token (1,5) KW_Int
                , Token (1,8) TokLBracket
                , Token (1,9) TokRBracket
                , Token (1,11) TokArrow
                , Token (1,14) KW_Void
                , Token (1,18) TokEOF
                ]

    , testCase "generic forall keyword" $
        lexer "forall t . func"
            @?= Right [ Token (1,1) KW_Forall
                , Token (1,8) (TokId "t")
                , Token (1,10) TokDot
                , Token (1,12) KW_Func
                , Token (1,16) TokEOF
                ]

    , testCase "read, print, and increment operators" $
        lexer "read(x); print(y); i++"
            @?= Right [ Token (1,1) KW_Read
                , Token (1,5) TokLParen
                , Token (1,6) (TokId "x")
                , Token (1,7) TokRParen
                , Token (1,8) TokSemi
                , Token (1,10) KW_Print
                , Token (1,15) TokLParen
                , Token (1,16) (TokId "y")
                , Token (1,17) TokRParen
                , Token (1,18) TokSemi
                , Token (1,20) (TokId "i")
                , Token (1,21) TokIncrement
                , Token (1,23) TokEOF
                ]

    , testCase "structural keywords and separators" $
        lexer "struct { name: string; }"
            @?= Right [ Token (1,1) KW_Struct
                , Token (1,8) TokLBrace
                , Token (1,10) (TokId "name")
                , Token (1,14) TokColon
                , Token (1,16) KW_String
                , Token (1,22) TokSemi
                , Token (1,24) TokRBrace
                , Token (1,25) TokEOF
                ]
  ]
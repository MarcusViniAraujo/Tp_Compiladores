module ParserSpec (parserTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Frontend.Parser.SlParser (parseSL)

import Frontend.Syntax.SlSyntax
  ( Sl(Sl)
  , Definition(Dfunc, DStruct)
  , Param(Param)
  , Field(Field)

  , Type(TInt, TFloat, TBool, TStruct, TVar, TFunc, TVoid, TVector)

  , Stmt
      ( SReturn, SAssign, SIf, SWhile, SFor
      , SRead, SPrint, SLet, SLetInfer, SExpr
      )

  , Value(VInt, VFloat, VBool, VString)

  , Exp
      ( EVar, EValue, EIndex, ECall, EIncrement
      , EField, ENew, EStruct, EVector
      , (:+:), (:-:), (:*:), (:/:), (:%:)
      , (:&&:), (:||:), (:==:), (:!=:)
      , (:<:), (:>:), (:<=:), (:>=:)
      , ENot, EMinus
      )
  )



parserTests :: TestTree
parserTests = testGroup "Parser tests"
    [ testCase "empty program" $
        parseSL ""
            @?= Right (Sl [])

    , testCase "simple function" $
        parseSL "func main() { }"
            @?= Right
                (Sl
                    [ Dfunc "main" [] [] Nothing [] ])


    , testCase "struct definition with fields" $
        parseSL "struct Point { x: float; y: int; }"
            @?= Right
                (Sl
                    [ DStruct "Point" 
                        [ Field "x" TFloat, Field "y" TInt ]
                ])

    , testCase "let declaration with explicit type and value" $
        parseSL "func f() { let count : int = 5; }"
            @?= Right
                (Sl
                    [ Dfunc "f" [] [] Nothing 
                        [ SLet "count" TInt (Just (EValue (VInt 5))) ] 
                    ])

    , testCase "generic function definition (forall)" $
        parseSL "forall t . func identity(x: t): t { return x; }"
            @?= Right
                (Sl
                    [ Dfunc "identity" ["t"] [Param "x" (Just (TVar "t"))] (Just (TVar "t"))
                        [ SReturn (EVar "x") ]
                    ])
                
    , testCase "function with vector and function types" $
        parseSL "func map(f: (int) -> int, arr: int[]): void { }"
            @?= Right
                (Sl
                    [ Dfunc "map" [] 
                        [ Param "f" (Just (TFunc [TInt] TInt))
                        , Param "arr" (Just (TVector TInt))
                        ] 
                        (Just TVoid)
                        []
                    ])


    , testCase "if-else structure with blocks" $
        parseSL "func check(x: int) { if (x > 0) { print(1); } else { print(0); } }"
            @?= Right
                (Sl
                    [ Dfunc "check" [] [Param "x" (Just TInt)] Nothing 
                        [ SIf (EVar "x" :>: EValue (VInt 0))
                            [ SPrint (EValue (VInt 1)) ]
                            [ SPrint (EValue (VInt 0)) ] 
                        ]
                    ])

    , testCase "while loop with complex condition" $
        parseSL "func loop(i: int) { while (i < 10 && i != 0) { i = i - 1; } }"
            @?= Right
              (Sl
                [ Dfunc "loop" [] [Param "i" (Just TInt)] Nothing
                    [ SWhile ((EVar "i" :<: EValue (VInt 10)) :&&: (EVar "i" :!=: EValue (VInt 0)))
                             [ SAssign (EVar "i") (EVar "i" :-: EValue (VInt 1)) ]
                    ]
                ])

    , testCase "for loop initialization and increment" $
        parseSL "func f() { for (let i = 0; i < 10; i++) { } }"
            @?= Right
              (Sl
                [ Dfunc "f" [] [] Nothing
                    [ SFor 
                        (SLetInfer "i" (EValue (VInt 0)))
                        (EVar "i" :<: EValue (VInt 10))
                        (SExpr (EIncrement (EVar "i")))
                        []
                    ]
                ])
                
  
    , testCase "struct instantiation and field access" $
        parseSL "func f(p: Point) { p.x = Point { 10, 20 }; }" 
            @?= Right
                (Sl
                    [ Dfunc "f" [] [Param "p" (Just (TStruct "Point"))] Nothing
                        [ SAssign (EField (EVar "p") "x")
                            (EStruct "Point" [EValue (VInt 10), EValue (VInt 20)]) 
                        ]
                    ])

    , testCase "vector indexing and assignment" $
        parseSL "func f(arr: int[]) { arr[0] = 10 * y; }"
            @?= Right
                (Sl
                    [ Dfunc "f" [] [Param "arr" (Just (TVector TInt))] Nothing
                        [ SAssign (EIndex (EVar "arr") (EValue (VInt 0)))
                            (EValue (VInt 10) :*: EVar "y") 
                        ]
                    ])

  ]
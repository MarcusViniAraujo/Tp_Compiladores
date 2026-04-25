{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Frontend.Lexer.SlLexer where

import Control.Monad
import Frontend.Lexer.Token
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]
@int    = $digit+
@float  = $digit+ \. $digit+
@id     = $alpha [$alpha $digit _]* 
@string = \" ([^\"\\]|\\.)* \"

tokens :-

  <0> $white+             ;

  <0> "//" .* ;

  <0> "/*"                { nestComment `andBegin` state_comment }
  <0> "*/"                {\ _ _ -> alexError "Error! Unexpected close comment!" }
  <state_comment> "/*"    { nestComment }
  <state_comment> "*/"    { unnestComment }
  <state_comment> .       ;
  <state_comment> \n      ;

  <0> "func"              { simpleToken KW_Func }
  <0> "struct"            { simpleToken KW_Struct }
  <0> "let"               { simpleToken KW_Let }
  <0> "return"            { simpleToken KW_Return }
  <0> "if"                { simpleToken KW_If }
  <0> "else"              { simpleToken KW_Else }
  <0> "while"             { simpleToken KW_While }
  <0> "for"               { simpleToken KW_For }
  <0> "new"               { simpleToken KW_New }
  <0> "forall"            { simpleToken KW_Forall }
  <0> "void"              { simpleToken KW_Void }
  <0> "read"              { simpleToken KW_Read }
  <0> "print"             { simpleToken KW_Print }

  <0> "int"               { simpleToken KW_Int }
  <0> "float"             { simpleToken KW_Float }
  <0> "string"            { simpleToken KW_String }
  <0> "bool"              { simpleToken KW_Bool }
  
  <0> "true"              { simpleToken KW_True }
  <0> "false"             { simpleToken KW_False }


  <0> "->"                { simpleToken TokArrow }
  <0> "=="                { simpleToken TokEq }
  <0> "!="                { simpleToken TokNeq }
  <0> "<="                { simpleToken TokLeq }
  <0> ">="                { simpleToken TokGeq }
  <0> "&&"                { simpleToken TokAnd }
  <0> "||"                { simpleToken TokOr }
  <0> "="                 { simpleToken TokAssign }
  <0> "++"                { simpleToken TokIncrement }
  <0> "+"                 { simpleToken TokPlus }
  <0> "-"                 { simpleToken TokMinus }
  <0> "*"                 { simpleToken TokTimes }
  <0> "/"                 { simpleToken TokDiv }
  <0> "%"                 { simpleToken TokMod }
  <0> "!"                 { simpleToken TokNot }
  <0> "<"                 { simpleToken TokLt }
  <0> ">"                 { simpleToken TokGt }
  <0> "."                 { simpleToken TokDot }

  <0> "("                 { simpleToken TokLParen }
  <0> ")"                 { simpleToken TokRParen }
  <0> "{"                 { simpleToken TokLBrace }
  <0> "}"                 { simpleToken TokRBrace }
  <0> "["                 { simpleToken TokLBracket }
  <0> "]"                 { simpleToken TokRBracket }
  <0> ";"                 { simpleToken TokSemi }
  <0> ":"                 { simpleToken TokColon }
  <0> ","                 { simpleToken TokComma }

  <0> @float              { mkFloat }
  <0> @int                { mkInt }
  <0> @string             { mkString }
  <0> @id                 { mkId }

{


data AlexUserState = AlexUserState
  { nestLevel :: Int }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

alexEOF :: Alex Token
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  startCode <- alexGetStartCode
  when (startCode == state_comment) $
    alexError "Error: unclosed comment"
  pure $ Token (position pos) TokEOF


nestComment :: AlexAction Token
nestComment input len = do
  modify $ \s -> s{nestLevel = nestLevel s + 1}
  skip input len

unnestComment :: AlexAction Token
unnestComment input len = do
  s <- get
  let level = (nestLevel s) - 1
  put s{nestLevel = level}
  when (level == 0) $
    alexSetStartCode 0
  skip input len

position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x, y)

simpleToken :: Lexeme -> AlexAction Token
simpleToken lx (st, _, _, _) _ = return $ Token (position st) lx


mkId :: AlexAction Token
mkId (st, _, _, str) len =
  pure $ Token (position st) (TokId (take len str))

mkInt :: AlexAction Token
mkInt (st, _, _, str) len =
  pure $ Token (position st) (TokInt (read $ take len str))

mkFloat :: AlexAction Token
mkFloat (st, _, _, str) len =
  pure $ Token (position st) (TokFloat (read $ take len str))

mkString :: AlexAction Token
mkString (st, _, _, str) len =

  let s = take len str
      content = init (tail s) 
  in pure $ Token (position st) (TokString content)


alexLexer :: (Token -> Alex a) -> Alex a
alexLexer cont = do
    tok <- alexMonadScan
    cont tok

lexer :: String -> Either String [Token]
lexer s = runAlex s go
  where
    go = do
      output <- alexMonadScan
      if lexeme output == TokEOF
        then pure [output]
        else (output :) <$> go
}
module Frontend.Lexer.Token where

data Token
  = Token {
      pos :: (Int, Int) 
    , lexeme :: Lexeme
    }
  deriving (Eq, Show)

data Lexeme
  = KW_Func | KW_Struct | KW_Let | KW_Return
  | KW_If | KW_Else | KW_While | KW_For
  | KW_New | KW_Forall | KW_Void | KW_Read | KW_Print
  | KW_Int | KW_Float | KW_String | KW_Bool
  | KW_True | KW_False
  
  | TokId String 
  | TokInt Int
  | TokFloat Double
  | TokString String

  | TokIncrement    -- ++
  | TokAssign       -- =
  | TokPlus         -- +
  | TokMinus        -- -
  | TokTimes        -- *
  | TokDiv          -- /
  | TokMod          -- %
  | TokAnd          -- &&
  | TokOr           -- ||
  | TokNot          -- !
  | TokEq           -- ==
  | TokNeq          -- !=
  | TokLt           -- <
  | TokGt           -- >
  | TokLeq          -- <=
  | TokGeq          -- >=
  | TokArrow        -- -> 
  | TokDot          -- .  

  -- Delimitadores
  | TokLParen       -- (
  | TokRParen       -- )
  | TokLBrace       -- {
  | TokRBrace       -- }
  | TokLBracket     -- [
  | TokRBracket     -- ]
  | TokSemi         -- ;
  | TokColon        -- :
  | TokComma        -- ,
  
  | TokEOF
  deriving (Eq, Show)
{
module Frontend.Parser.SlParser where

import Frontend.Lexer.Token
import Frontend.Lexer.SlLexer
import Frontend.Syntax.SlSyntax
}

%name slParser Sl
%monad { Alex } { (>>=) } { return }
%tokentype { Token }
%error { parseError }
%lexer { alexLexer } { Token _ TokEOF }

%token
    func    { Token _ KW_Func }
    struct  { Token _ KW_Struct }
    let     { Token _ KW_Let }
    return  { Token _ KW_Return }
    if      { Token _ KW_If }
    else    { Token _ KW_Else }
    while   { Token _ KW_While }
    for     { Token _ KW_For }
    new     { Token _ KW_New }
    forall  { Token _ KW_Forall }
    void    { Token _ KW_Void }
    read    { Token _ KW_Read }
    print   { Token _ KW_Print }

    int     { Token _ KW_Int }
    float   { Token _ KW_Float }
    string  { Token _ KW_String }
    bool    { Token _ KW_Bool }
    
    true    { Token _ KW_True }
    false   { Token _ KW_False }

    id      { Token _ (TokId $$) }
    int_lit { Token _ (TokInt $$) }
    float_lit { Token _ (TokFloat $$) }
    str_lit { Token _ (TokString $$) }

    '='     { Token _ TokAssign }
    '+'     { Token _ TokPlus }
    '-'     { Token _ TokMinus }
    '*'     { Token _ TokTimes }
    '/'     { Token _ TokDiv }
    '%'     { Token _ TokMod }
    '&&'    { Token _ TokAnd }
    '||'    { Token _ TokOr }
    '!'     { Token _ TokNot }
    '++'    { Token _ TokIncrement }
    '=='    { Token _ TokEq }
    '!='    { Token _ TokNeq }
    '<'     { Token _ TokLt }
    '>'     { Token _ TokGt }
    '<='    { Token _ TokLeq }
    '>='    { Token _ TokGeq }
    '->'    { Token _ TokArrow }
    '.'     { Token _ TokDot }

    '('     { Token _ TokLParen }
    ')'     { Token _ TokRParen }
    '{'     { Token _ TokLBrace }
    '}'     { Token _ TokRBrace }
    '['     { Token _ TokLBracket }
    ']'     { Token _ TokRBracket }
    ';'     { Token _ TokSemi }
    ':'     { Token _ TokColon }
    ','     { Token _ TokComma }


%right '->'
%left '||'
%left '&&'
%nonassoc '==' '!=' '<' '>' '<=' '>=' 
%left '+' '-'
%left '*' '/' '%'
%left '!'           
%left NEG           
%nonassoc '++'
%nonassoc PREC_INTLIT
%left '[' ']' '.'   
%left '(' ')'       

%%


Sl : Definitions { Sl (reverse $1) }

Definitions 
    : Definitions Definition { $2 : $1 }
    | {- empty -}           { [] }

Definition
    : StructDef { $1 }
    | FuncDef   { $1 }


StructDef 
    : struct id '{' Fields '}' { DStruct $2 (reverse $4) }

Fields
    : Fields Field ';' { $2 : $1 }
    | {- empty -}      { [] }

Field 
    : id ':' Type { Field $1 $3 }


FuncDef
    : func id '(' Params ')' OptRetType '{' Block '}' 
      { Dfunc $2 [] (reverse $4) $6 $8 } 

    | forall TypeVars '.' func id '(' Params ')' OptRetType '{' Block '}'
      { Dfunc $5 (reverse $2) (reverse $7) $9 $11 } 

OptRetType
    : ':' Type      { Just $2 }  
    | {- empty -}   { Nothing }  


TypeVars
    : TypeVars id { $2 : $1 }
    | id          { [$1] }

Params
    : ParamsList { $1 }
    | {- empty -} { [] }

ParamsList
    : ParamsList ',' Param { $3 : $1 }
    | Param                { [$1] }

Param
    : id ':' Type { Param $1 (Just $3) } 
    | id          { Param $1 Nothing }   



Type
    : int         { TInt }
    | float       { TFloat }
    | string      { TString }
    | bool        { TBool }
    | void        { TVoid }
    | id          { if isTypeVar $1 then TVar $1 else TStruct $1 } 
    | Type '[' ']' { TVector $1 }          
    | Type '[' int_lit ']' %prec ']' { TVectorN $1 $3 }
    | '(' TypeList ')' '->' Type { TFunc (reverse $2) $5 }

TypeList
    : TypeList ',' Type { $3 : $1 }
    | Type              { [$1] }



Block 
    : Stmts { reverse $1 }

Stmts
    : Stmts Stmt { $2 : $1 }
    | {- empty -} { [] }

Stmt
    
    : let id ':' Type '=' Exp ';'  { SLet $2 $4 (Just $6) }

    | let id ':' Type ';'          { SLet $2 $4 Nothing }
    
    | let id '=' Exp ';'           { SLetInfer $2 $4 }
    
    | Exp '=' Exp ';'              { SAssign $1 $3 }

    | read '(' Exp ')' ';'              { SRead $3 }
    | print '(' Exp ')' ';'             {SPrint $3} 

    
    | if '(' Exp ')' '{' Block '}' else '{' Block '}' { SIf $3 $6 $10 }
    | if '(' Exp ')' '{' Block '}'                    { SIf $3 $6 [] }
    | while '(' Exp ')' '{' Block '}'                 { SWhile $3 $6 }
    | for '(' Stmt Exp ';' StmtNoSemi ')' '{' Block '}'     { SFor $3 $4 $6 $9 } 
    
    | return Exp ';'               { SReturn $2 }
    | Exp ';'                      { SExpr $1 } 

StmtNoSemi
    : Exp '++'                { SExpr (EIncrement $1) } -- i++



Exp
   
    : int_lit %prec PREC_INTLIT  { EValue (VInt $1) }
    | float_lit     { EValue (VFloat $1) }
    | str_lit       { EValue (VString $1) }
    | true          { EValue (VBool True) }
    | false         { EValue (VBool False) }
    | id            { EVar $1 }
    | id '{' ExpList '}'  { EStruct $1 (reverse $3) }

 
    | '[' ExpList ']'      { EVector (reverse $2) } 
    | new Type '[' Exp ']' { ENew $2 $4 }           
    
    | Exp '[' Exp ']'      { EIndex $1 $3 }         
    | Exp '.' id           { EField $1 $3 }         

    | Exp '++'             { EIncrement $1 }   
   
    | Exp '(' ExpList ')'  { ECall $1 (reverse $3) }
    | Exp '(' ')'          { ECall $1 [] }

    | Exp '+' Exp   { $1 :+: $3 }
    | Exp '-' Exp   { $1 :-: $3 }
    | Exp '*' Exp   { $1 :*: $3 }
    | Exp '/' Exp   { $1 :/: $3 }
    | Exp '%' Exp   { $1 :%: $3 }
    | '-' Exp %prec NEG { EMinus $2 } 
    
    | Exp '&&' Exp  { $1 :&&: $3 }
    | Exp '||' Exp  { $1 :||: $3 }
    | '!' Exp       { ENot $2 }
    
    | Exp '==' Exp  { $1 :==: $3 }
    | Exp '!=' Exp  { $1 :!=: $3 }
    | Exp '<' Exp   { $1 :<: $3 }
    | Exp '>' Exp   { $1 :>: $3 }
    | Exp '<=' Exp  { $1 :<=: $3 }
    | Exp '>=' Exp  { $1 :>=: $3 }
    
    | '(' Exp ')'   { $2 }

ExpList
    : ExpList ',' Exp { $3 : $1 }
    | Exp             { [$1] }

{
parseError :: Token -> Alex a
parseError (Token (l, c) lexeme) = 
    alexError $ "Syntax error on line " ++ show l ++ ", column " ++ show c ++ 
            ". Unexpected token: " ++ show lexeme

isTypeVar :: String -> Bool
isTypeVar (c:cs) = null cs && c `elem` ['a'..'z'] 
isTypeVar _ = False


parseSL :: String -> Either String Sl
parseSL input = runAlex input $ do
  ast <- slParser
  return ast
}


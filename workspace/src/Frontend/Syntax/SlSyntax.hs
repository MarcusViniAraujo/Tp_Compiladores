module Frontend.Syntax.SlSyntax where

data Sl 
    = Sl [Definition]
  deriving (Eq, Ord, Show)

type Var = String
type Block =  [Stmt]
type TypeVar = String

data Field 
    = Field String Type
  deriving (Eq, Ord, Show)

data Param 
    = Param String (Maybe Type)
  deriving (Eq, Ord, Show)

data Definition 
    = DStruct String [Field]
    | Dfunc String [TypeVar] [Param] (Maybe Type) Block
   
  deriving (Eq, Ord, Show)
  

data Type 
    = TInt | TFloat | TString | TBool | TVoid
    | TVector Type 
    | TVectorN Type Int 
    | TStruct String
    | TVar TypeVar 
    | TFunc [Type] Type
    deriving (Eq, Ord, Show)

data Stmt
    = SAssign Exp Exp 
    | SLet Var Type (Maybe Exp) 
    | SLetInfer Var Exp 
    | SRead Exp 
    | SPrint Exp 
    | SIf Exp Block Block 
    | SFor Stmt Exp Stmt Block 
    | SWhile Exp Block 
    | SReturn Exp 
    | SExpr Exp           
    deriving (Eq, Ord, Show)

data Exp
    = EValue Value 
    | EVector [Exp]
    | EStruct String [Exp]
    | EIndex Exp Exp 
    | EField Exp String 
    | EArraySize Exp 
    | ENew Type Exp
    | ECall Exp [Exp] 
    | EIncrement Exp
    | EVar Var 
    | ENot Exp 
    | EMinus Exp
    | Exp :+: Exp
    | Exp :-: Exp
    | Exp :*: Exp
    | Exp :/: Exp
    | Exp :%: Exp
    | Exp :&&: Exp
    | Exp :||: Exp
    | Exp :<:  Exp
    | Exp :>:  Exp
    | Exp :<=: Exp
    | Exp :>=: Exp
    | Exp :==: Exp
    | Exp :!=: Exp
    deriving (Eq, Ord, Show)

data Value
    = VInt Int
    | VFloat Double
    | VString String
    | VBool Bool
  deriving (Eq, Ord, Show)
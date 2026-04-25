module Semantics.SlTypeEnv where

import Frontend.Syntax.SlSyntax
import qualified Data.Map as Map

-- | Representa os possíveis erros durante a verificação de tipos
data TypeError
    = TypeMismatch Type Type      
    | UndefinedVar Var            
    | UndefinedStruct String      
    | FieldNotFound String String 
    | NotAFunction Type           
    | ArgCountMismatch Int Int    
    | InvalidIndex Type           
    | NotAStruct Type             
    | UnificationFail Type Type   
    deriving (Show, Eq)

-- [(String, Type)] (Preserva ordem: x vem antes de y)
type StructInfo = [(String, Type)]

-- | O Ambiente de Tipos (Gamma)
data TypeEnv = TypeEnv
    { varEnv    :: Map.Map Var Type
    , funcEnv   :: Map.Map Var Type
    , structEnv :: Map.Map String StructInfo 
    } deriving (Show)

-- | Cria um ambiente vazio
emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty Map.empty Map.empty

-- | Busca variável (local ou global)
lookupVar :: Var -> TypeEnv -> Maybe Type
lookupVar v env = 
    case Map.lookup v (varEnv env) of
        Just t -> Just t
        Nothing -> Map.lookup v (funcEnv env)

-- | Busca definição de struct
lookupStruct :: String -> TypeEnv -> Maybe StructInfo
lookupStruct name env = Map.lookup name (structEnv env)
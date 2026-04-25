module Semantics.SlTypeChecker where

import Semantics.SlTypeEnv
import Frontend.Syntax.SlSyntax
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad (zipWithM_)
import qualified Data.Map as Map

-- | Estado do TypeChecker
data CheckState = CheckState
    { count :: Int 
    }

initialState :: CheckState
initialState = CheckState 0

type CheckM a = ExceptT TypeError (ReaderT TypeEnv (State CheckState)) a

-- | Função principal para rodar o verificador
runCheck :: CheckM a -> TypeEnv -> Either TypeError a
runCheck action env = 
    evalState (runReaderT (runExceptT action) env) initialState

-- === FUNÇÕES AUXILIARES ===

throwError' :: TypeError -> CheckM a
throwError' err = throwError err

getVarType :: Var -> CheckM Type
getVarType var = do
    env <- ask
    case lookupVar var env of
        Just t -> return t
        Nothing -> throwError' (UndefinedVar var)

-- Verifica compatibilidade (com suporte básico a Generics)
unify :: Type -> Type -> CheckM ()
unify t1 t2 = 
    if t1 == t2 
    then return ()
    else case (t1, t2) of
        (TVar _, _) -> return () 
        (_, TVar _) -> return ()
        
        -- Erro real
        _ -> throwError' (TypeMismatch t1 t2)

-- === VERIFICAÇÃO DE EXPRESSÕES ===

typeCheckExp :: Exp -> CheckM Type
-- 1. Valores Literais
typeCheckExp (EValue (VInt _))    = return TInt
typeCheckExp (EValue (VFloat _))  = return TFloat
typeCheckExp (EValue (VString _)) = return TString
typeCheckExp (EValue (VBool _))   = return TBool

-- 2. Variáveis
typeCheckExp (EVar v) = getVarType v

-- 3. Operações Lógicas
typeCheckExp (e1 :&&: e2) = do
    t1 <- typeCheckExp e1
    t2 <- typeCheckExp e2
    unify t1 TBool
    unify t2 TBool
    return TBool

typeCheckExp (e1 :||: e2) = do
    t1 <- typeCheckExp e1
    t2 <- typeCheckExp e2
    unify t1 TBool
    unify t2 TBool
    return TBool

typeCheckExp (ENot e) = do
    t <- typeCheckExp e
    unify t TBool
    return TBool

-- 4. Operações Aritméticas
typeCheckExp (e1 :+: e2) = checkArithmetic e1 e2
typeCheckExp (e1 :-: e2) = checkArithmetic e1 e2
typeCheckExp (e1 :*: e2) = checkArithmetic e1 e2
typeCheckExp (e1 :/: e2) = checkArithmetic e1 e2

-- 5. Comparações
typeCheckExp (e1 :==: e2) = do
    t1 <- typeCheckExp e1
    t2 <- typeCheckExp e2
    unify t1 t2
    return TBool

typeCheckExp (e1 :!=: e2) = do
    t1 <- typeCheckExp e1
    t2 <- typeCheckExp e2
    unify t1 t2
    return TBool

typeCheckExp (e1 :<: e2) = checkOrder e1 e2
typeCheckExp (e1 :>: e2) = checkOrder e1 e2
typeCheckExp (e1 :<=: e2) = checkOrder e1 e2
typeCheckExp (e1 :>=: e2) = checkOrder e1 e2

-- 7. Construção de Struct (ATUALIZADO PARA LISTA)
typeCheckExp (EStruct nomeStruct exprs) = do
    env <- ask
    case lookupStruct nomeStruct env of
        Nothing -> throwError' (UndefinedStruct nomeStruct)
        Just fieldsList -> do
            -- Pegamos apenas os tipos, na ordem correta.
            let tiposEsperados = map snd fieldsList
            
            tiposPassados <- mapM typeCheckExp exprs
            
            if length tiposEsperados /= length tiposPassados
                then throwError' (ArgCountMismatch (length tiposEsperados) (length tiposPassados))
                else do
                    zipWithM_ unify tiposPassados tiposEsperados
                    return (TStruct nomeStruct)

-- 8. Acesso a Campo (ATUALIZADO PARA SUPORTAR ARRAY.SIZE)
typeCheckExp (EField expr nomeCampo) = do
    tipoObj <- typeCheckExp expr
    case tipoObj of
        -- Caso 1: É um Struct Normal
        TStruct nomeStruct -> do
            env <- ask
            case lookupStruct nomeStruct env of
                Nothing -> throwError' (UndefinedStruct nomeStruct)
                Just fieldsList -> 
                    case lookup nomeCampo fieldsList of
                        Just tipoCampo -> return tipoCampo
                        Nothing -> throwError' (FieldNotFound nomeCampo nomeStruct)
        
        -- Caso 2: É um Array e estamos pedindo ".size"
        TVector _ -> 
            if nomeCampo == "size"
            then return TInt
            else throwError' (FieldNotFound nomeCampo "Array")

        -- Erro: Não é nem Struct nem Array
        _ -> throwError' (NotAStruct tipoObj)

-- 9. Chamada de Função
typeCheckExp (ECall exprFun args) = do
    tipoFun <- typeCheckExp exprFun
    case tipoFun of
        TFunc tiposParams tipoRetorno -> do
            if length args /= length tiposParams
                then throwError' (ArgCountMismatch (length tiposParams) (length args))
                else do
                    tiposArgs <- mapM typeCheckExp args
                    zipWithM_ unify tiposArgs tiposParams
                    return tipoRetorno
        _ -> throwError' (NotAFunction tipoFun)

-- 10. Criação de Array (new int[5])
typeCheckExp (ENew tipoTamanho exprTamanho) = do
    tSize <- typeCheckExp exprTamanho
    unify tSize TInt -- O tamanho DEVE ser inteiro
    return (TVector tipoTamanho)

-- 11. Literal de Array ([1, 2, 3])
typeCheckExp (EVector exprs) = do
    if null exprs
    then return (TVector TVoid) -- Array vazio (caso de borda)
    else do
        -- Verifica o tipo de todos os elementos
        tipos <- mapM typeCheckExp exprs
        let primeiroTipo = head tipos
        
        -- Garante que todos são iguais ao primeiro (ex: todos Int)
        mapM_ (unify primeiroTipo) tipos
        
        return (TVector primeiroTipo)

-- 12. Acesso a Índice (arr[i])
typeCheckExp (EIndex exprArr exprIdx) = do
    tArr <- typeCheckExp exprArr
    tIdx <- typeCheckExp exprIdx
    
    unify tIdx TInt -- O índice deve ser int
    
    case tArr of
        TVector tElem -> return tElem -- Retorna o tipo do elemento
        _ -> throwError' (InvalidIndex tArr)

-- 13. Tamanho do Array (arr.size)
typeCheckExp (EArraySize exprArr) = do
    tArr <- typeCheckExp exprArr
    case tArr of
        TVector _ -> return TInt
        _ -> throwError' (TypeMismatch (TVector TVoid) tArr)

typeCheckExp _ = return TVoid

-- === VERIFICAÇÃO DE STATEMENTS ===

checkStmt :: Stmt -> CheckM Type
checkStmt (SLet var tipo (Just expr)) = do
    tipoExpr <- typeCheckExp expr
    unify tipo tipoExpr
    return TVoid 

checkStmt (SAssign (EVar var) expr) = do
    tipoVar <- getVarType var
    tipoExpr <- typeCheckExp expr
    unify tipoVar tipoExpr
    return TVoid

checkStmt (SIf cond blocoThen blocoElse) = do
    tipoCond <- typeCheckExp cond
    unify tipoCond TBool
    t1 <- checkBlock blocoThen
    t2 <- checkBlock blocoElse
    
    -- Se ambos os blocos retornarem o mesmo tipo e NAO for Void,
    -- entao o IF inteiro tem esse tipo.
    if t1 == t2 && t1 /= TVoid
        then return t1
        else return TVoid -- Se houver discrepancia ou for apenas um efeito colateral, retorna TVoid

checkStmt (SWhile cond bloco) = do
    tipoCond <- typeCheckExp cond
    unify tipoCond TBool
    tipoDoBloco <- checkBlock bloco
    return tipoDoBloco

checkStmt (SReturn expr) = do
    t <- typeCheckExp expr
    return t

checkStmt (SExpr expr) = do
    _ <- typeCheckExp expr
    return TVoid

checkStmt _ = return TVoid

checkBlock :: [Stmt] -> CheckM Type
checkBlock [] = return TVoid
checkBlock (stmt:resto) = 
    case stmt of
        -- Casos de Definição (Let): Continuam estendendo o escopo local
        SLet var tipo (Just expr) -> do
            tExpr <- typeCheckExp expr
            unify tipo tExpr
            local (extendVar var tipo) (checkBlock resto)
            
        SLet var tipo Nothing -> 
            local (extendVar var tipo) (checkBlock resto)

        SLetInfer var expr -> do
            tExpr <- typeCheckExp expr
            local (extendVar var tExpr) (checkBlock resto)

        -- Caso do Return: Ele determina o tipo do bloco, 
        -- mas precisamos validar o resto por questões de sintaxe (opcional)
        SReturn expr -> typeCheckExp expr

        -- Comandos de Fluxo e Atribuição (If, While, Assign)
        _ -> do
            tipoStmt <- checkStmt stmt
            if tipoStmt /= TVoid
                then do
                    -- Se houver um retorno dentro de um If/While, 
                    -- ele é o tipo do bloco, mas o resto deve ser ignorado.
                    return tipoStmt
                else checkBlock resto


-- === FUNÇÃO PRINCIPAL (CHECK PROGRAM) ===

checkProgram :: Sl -> Either TypeError ()
checkProgram (Sl defs) = 
    let 
        envInicial = foldl registrarDefinicao emptyEnv defs
    in
        runCheck (mapM_ checkDefBody defs) envInicial

registrarDefinicao :: TypeEnv -> Definition -> TypeEnv
registrarDefinicao env (DStruct nome campos) = 
    -- MUDANÇA: Guardamos como LISTA [(String, Type)]
    let infos = [ (n, t) | Field n t <- campos ]
    in env { structEnv = Map.insert nome infos (structEnv env) }

registrarDefinicao env (Dfunc nome _ params tipoRetorno _) = 
    let 
        tiposParams = [ t | Param _ (Just t) <- params ]
        ret = case tipoRetorno of Just t -> t; Nothing -> TVoid
        tipoFun = TFunc tiposParams ret
    in env { funcEnv = Map.insert nome tipoFun (funcEnv env) }

checkDefBody :: Definition -> CheckM ()
checkDefBody (DStruct _ _) = return ()
checkDefBody (Dfunc _ _ params tipoRetorno bloco) = do
    let paramsComTipo = [ (n, t) | Param n (Just t) <- params ]
    
    local (addParams paramsComTipo) $ do
        tipoEncontrado <- checkBlock bloco
        let retEsperado = case tipoRetorno of Just t -> t; Nothing -> TVoid
        
        if retEsperado /= TVoid && tipoEncontrado == TVoid 
            then throwError' (TypeMismatch retEsperado TVoid)
            else unify retEsperado tipoEncontrado

-- === AUXILIARES ===

checkArithmetic :: Exp -> Exp -> CheckM Type
checkArithmetic e1 e2 = do
    t1 <- typeCheckExp e1
    t2 <- typeCheckExp e2
    unify t1 t2
    case t1 of
        TInt   -> return TInt
        TFloat -> return TFloat
        _      -> throwError' (TypeMismatch TInt t1)

checkOrder :: Exp -> Exp -> CheckM Type
checkOrder e1 e2 = do
    t1 <- typeCheckExp e1
    t2 <- typeCheckExp e2
    unify t1 t2
    case t1 of
        TInt   -> return TBool
        TFloat -> return TBool
        _      -> throwError' (TypeMismatch TInt t1)

extendVar :: Var -> Type -> TypeEnv -> TypeEnv
extendVar var tipo env = 
    env { varEnv = Map.insert var tipo (varEnv env) }

addParams :: [(Var, Type)] -> TypeEnv -> TypeEnv
addParams ps env = foldl (\e (n,t) -> extendVar n t e) env ps
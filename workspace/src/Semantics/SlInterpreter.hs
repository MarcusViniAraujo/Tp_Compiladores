module Semantics.SlInterpreter where

import Frontend.Syntax.SlSyntax
import Control.Monad.State
import Control.Monad.Except
import Control.Monad (void)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- === 1. ESTRUTURAS DE DADOS ===

-- | Valor em Tempo de Execução (Memória)
-- A AST só tem VInt, VBool... Aqui precisamos de mais poder (Arrays, Structs)
data RValue
    = RInt Int
    | RFloat Double
    | RString String
    | RBool Bool
    | RStruct String (Map.Map String RValue) -- NomeDoStruct + Mapa de Campos {x: 10, y: 20}
    | RArray [RValue]                        -- Vetores [1, 2, 3]
    | RVoid                                  -- Para funções que não retornam nada
    | RNull                                  -- Para variáveis não inicializadas
    deriving (Show, Eq)

-- | O Ambiente de Execução (Memória + Definições)
data Env = Env
    { vars  :: Map.Map Var RValue        -- Variáveis: x = 10
    , funcs :: Map.Map Var Definition    -- Funções: main() { ... }
    , structs :: Map.Map String [Field]  -- Moldes de Structs
    , retVal :: Maybe RValue             -- Para guardar o valor de retorno de uma função
    } deriving (Show)

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty Map.empty Nothing

-- | Nosso Monad: Erro + Estado + IO
type InterpM a = ExceptT String (StateT Env IO) a

-- === 2. FUNÇÕES AUXILIARES ===

-- | Roda o interpretador
runInterp :: InterpM a -> Env -> IO (Either String a)
runInterp action env = evalStateT (runExceptT action) env

-- | Lança erro
err :: String -> InterpM a
err msg = throwError $ "[Erro de Execucao] " ++ msg

-- | Busca variável
getVar :: Var -> InterpM RValue
getVar v = do
    env <- get
    case Map.lookup v (vars env) of
        Just val -> return val
        Nothing -> err $ "Variavel nao inicializada ou inexistente: " ++ v

-- | Atualiza/Cria variável
setVar :: Var -> RValue -> InterpM ()
setVar v val = do
    env <- get
    put $ env { vars = Map.insert v val (vars env) }

-- === 3. AVALIAÇÃO DE EXPRESSÕES ===

eval :: Exp -> InterpM RValue
-- Literais
eval (EValue (VInt i))    = return (RInt i)
eval (EValue (VFloat f))  = return (RFloat f)
eval (EValue (VBool b))   = return (RBool b)
eval (EValue (VString s)) = return (RString s)

-- Variável
eval (EVar v) = getVar v

-- Operações Aritméticas
eval (e1 :+: e2) = opNum (+) (+) e1 e2
eval (e1 :-: e2) = opNum (-) (-) e1 e2
eval (e1 :*: e2) = opNum (*) (*) e1 e2
-- Divisão precisa checar zero!
eval (e1 :/: e2) = do 
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (RInt a, RInt b) -> if b == 0 then err "Divisao por zero" else return (RInt (div a b))
        (RFloat a, RFloat b) -> if b == 0 then err "Divisao por zero" else return (RFloat (a / b))
        _ -> err "Tipos invalidos para divisao"

-- Operações Lógicas e Comparação
eval (e1 :&&: e2) = opBool (&&) e1 e2
eval (e1 :||: e2) = opBool (||) e1 e2
eval (e1 :==: e2) = do
    v1 <- eval e1
    v2 <- eval e2
    return $ RBool (v1 == v2)

eval (e1 :<: e2)  = opCmp (<) (<) e1 e2
eval (e1 :>: e2)  = opCmp (>) (>) e1 e2
eval (e1 :<=: e2)  = opCmp (<=) (<=) e1 e2
eval (e1 :>=: e2)  = opCmp (>=) (>=) e1 e2


-- Structs (Instanciação)
eval (EStruct nome exprs) = do
    vals <- mapM eval exprs
    env <- get
    case Map.lookup nome (structs env) of
        Nothing -> err $ "Struct nao definido: " ++ nome
        Just fields -> do
            let nomesCampos = [n | Field n _ <- fields]
            if length vals /= length nomesCampos 
                then err "Numero incorreto de argumentos para struct"
                else return $ RStruct nome (Map.fromList (zip nomesCampos vals))

-- Acesso a Campo (Struct ou Array.size)
eval (EField expr nomeCampo) = do
    obj <- eval expr
    case obj of
        -- Caso 1: Struct
        RStruct _ campos -> case Map.lookup nomeCampo campos of
            Just val -> return val
            Nothing -> err $ "Campo nao encontrado: " ++ nomeCampo
            
        -- Caso 2: Array.size
        RArray lista -> 
            if nomeCampo == "size"
            then return (RInt (length lista))
            else err "Arrays possuem apenas o campo .size"
            
        _ -> err "Tentativa de acessar campo de algo que nao e struct nem array"

-- Chamada de Função (Simplificada para recursão básica)
eval (ECall (EVar nomeFun) args) = callFunc nomeFun args
eval (ECall _ _) = err "Chamadas de funcao complexas nao suportadas ainda"

-- Criação de Array (new int[size])
eval (ENew tipoExpr sizeExpr) = do
    sizeVal <- eval sizeExpr
    case sizeVal of
        RInt n -> do
            if n < 0 
            then err "Tamanho do array nao pode ser negativo"
            else do
                -- Cria uma lista de tamanho N preenchida com o valor padrão do tipo
                let lista = replicate n (defaultValue tipoExpr)
                return (RArray lista)
        _ -> err "Tamanho do array deve ser um inteiro"

-- Literal de Array ([1, 2, 3])
eval (EVector exprs) = do
    vals <- mapM eval exprs
    return (RArray vals)

-- Acesso a Índice (arr[i])
eval (EIndex exprArr exprIdx) = do
    valArr <- eval exprArr
    valIdx <- eval exprIdx
    
    case (valArr, valIdx) of
        (RArray lista, RInt i) -> 
            if i >= 0 && i < length lista
            then return (lista !! i) -- O operador (!!) do Haskell pega o elemento i
            else err $ "Indice fora dos limites: " ++ show i
        _ -> err "Tentativa de indexar algo que nao e array ou indice nao inteiro"

-- Tamanho do Array (arr.size ou similar)
eval (EArraySize expr) = do
    val <- eval expr
    case val of
        RArray lista -> return (RInt (length lista))
        _ -> err "Operacao size aplicada em nao-array"

eval _ = err "Expressao nao implementada no interpretador"

-- === 4. HELPERS DE OPERAÇÃO ===

opNum :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> Exp -> Exp -> InterpM RValue
opNum opI opF e1 e2 = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (RInt a, RInt b)     -> return $ RInt (opI a b)
        (RFloat a, RFloat b) -> return $ RFloat (opF a b)
        _ -> err "Operacao aritmetica invalida (tipos misturados?)"

opBool :: (Bool -> Bool -> Bool) -> Exp -> Exp -> InterpM RValue
opBool op e1 e2 = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (RBool a, RBool b) -> return $ RBool (op a b)
        _ -> err "Operacao logica requer booleanos"

opCmp :: (Int -> Int -> Bool) -> (Double -> Double -> Bool) -> Exp -> Exp -> InterpM RValue
opCmp opI opF e1 e2 = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (RInt a, RInt b)     -> return $ RBool (opI a b)
        (RFloat a, RFloat b) -> return $ RBool (opF a b)
        _ -> err "Comparacao invalida"

-- === 5. EXECUÇÃO DE STATEMENTS ===

exec :: Stmt -> InterpM ()
exec (SExpr e) = void (eval e)

exec (SPrint e) = do
    val <- eval e
    liftIO $ printRValue val -- Usamos liftIO para imprimir no console real

-- Declaração e Atribuição
exec (SLet var _ (Just expr)) = do
    val <- eval expr
    setVar var val

exec (SLetInfer var expr) = do
    val <- eval expr
    setVar var val

-- Atribuição em Array (v[i] = expr)
exec (SAssign (EIndex (EVar nomeVar) exprIdx) exprVal) = do
    -- 1. Avalia o índice e o novo valor
    idxVal <- eval exprIdx
    novoVal <- eval exprVal
    
    -- 2. Busca o array atual na memória
    arrAtual <- getVar nomeVar
    
    case (arrAtual, idxVal) of
        (RArray lista, RInt i) -> do
            -- 3. Verifica limites
            if i < 0 || i >= length lista
            then err $ "Indice de atribuicao fora dos limites: " ++ show i
            else do
                -- 4. Cria a nova lista modificada
                let novaLista = replaceAt i novoVal lista
                -- 5. Salva de volta na variável
                setVar nomeVar (RArray novaLista)
        
        (_, RInt _) -> err $ "Variavel " ++ nomeVar ++ " nao e um array"
        _ -> err "Indice de array deve ser inteiro"

exec (SAssign (EVar var) expr) = do
    val <- eval expr
    -- Verifica se var existe (Opcional, pois setVar sobrescreve)
    setVar var val

exec (SAssign (EField exprObj campo) exprVal) = do
    -- ler o struct, mudar o campo, e salvar o struct de volta.
    valNovo <- eval exprVal
    obj <- eval exprObj
    case (exprObj, obj) of
        (EVar nomeVar, RStruct nomeS campos) -> do
            let novosCampos = Map.insert campo valNovo campos
            setVar nomeVar (RStruct nomeS novosCampos)
        _ -> err "Atribuicao em campo aninhado nao suportada nesta versao simples"

-- Controle de Fluxo
exec (SIf cond thenBlock elseBlock) = do
    vCond <- eval cond
    case vCond of
        RBool True  -> execBlock thenBlock
        RBool False -> execBlock elseBlock
        _ -> err "Condicao do IF deve ser booleana"

exec (SWhile cond block) = loop
  where
    loop = do
        vCond <- eval cond
        case vCond of
            RBool True -> do
                execBlock block
                -- Verifica se houve return dentro do while
                env <- get
                case retVal env of
                    Nothing -> loop -- Continua
                    Just _  -> return () -- Para (sai do loop e da função)
            RBool False -> return ()
            _ -> err "Condicao do WHILE deve ser booleana"

exec (SReturn expr) = do
    val <- eval expr
    env <- get
    put $ env { retVal = Just val }

exec _ = return () -- Ignora outros por enquanto

-- Executa uma lista de comandos
execBlock :: [Stmt] -> InterpM ()
execBlock [] = return ()
execBlock (s:ss) = do
    exec s
    -- Se encontrou um return, para de executar o bloco!
    env <- get
    case retVal env of
        Nothing -> execBlock ss
        Just _  -> return ()

-- === 6. CHAMADA DE FUNÇÃO ===

callFunc :: Var -> [Exp] -> InterpM RValue
callFunc nome args = do
    env <- get
    case Map.lookup nome (funcs env) of
        Nothing -> err $ "Funcao nao definida: " ++ nome
        Just (Dfunc _ _ params _ bloco) -> do
            -- 1. Avaliar argumentos
            valsArgs <- mapM eval args
            
            if length valsArgs /= length params
                then err "Numero incorreto de argumentos"
                else do
                    -- 2. Salvar escopo atual (Backup)
                    let varsAntigas = vars env
                    let retAntigo = retVal env
                    
                    -- 3. Definir novos parâmetros no escopo
                    let nomesParams = [n | Param n _ <- params]
                    let novosVars = foldl (\m (k, v) -> Map.insert k v m) varsAntigas (zip nomesParams valsArgs)
                    
                    put $ env { vars = novosVars, retVal = Nothing }
                    
                    -- 4. Executar corpo
                    execBlock bloco
                    
                    -- 5. Pegar retorno
                    envFinal <- get
                    let resultado = fromMaybe RVoid (retVal envFinal)
                    
                    -- 6. Restaurar escopo original
                    put $ envFinal { vars = varsAntigas, retVal = retAntigo }
                    
                    return resultado

        _ -> err "Tentativa de chamar algo que nao e DFunc"

-- === 7. ENTRADA/SAÍDA AUXILIAR ===

printRValue :: RValue -> IO ()
printRValue (RInt i) = print i
printRValue (RFloat f) = print f
printRValue (RString s) = putStrLn s
printRValue (RBool b) = print b
printRValue (RStruct nome _) = putStrLn $ "<Struct " ++ nome ++ ">"
printRValue (RArray _) = putStrLn "<Array>"
printRValue RVoid = return ()
printRValue RNull = putStrLn "null"

-- === 8. BOOTSTRAP (INICIALIZAÇÃO) ===

interpret :: Sl -> IO ()
interpret (Sl defs) = do
    -- 1. Carregar definições (Structs e Funções) no ambiente
    let envInicial = foldl loadDef emptyEnv defs
    
    -- 2. Procurar Main
    case Map.lookup "main" (funcs envInicial) of
        Nothing -> putStrLn "Erro: Funcao main nao encontrada."
        Just _ -> do
            -- 3. Rodar Main
            res <- runInterp (callFunc "main" []) envInicial
            case res of
                Left err -> putStrLn err
                Right _  -> return ()

loadDef :: Env -> Definition -> Env
loadDef env d@(Dfunc nome _ _ _ _) = env { funcs = Map.insert nome d (funcs env) }
loadDef env (DStruct nome campos) = env { structs = Map.insert nome campos (structs env) }

-- === 9. AUXILIARES DE ARRAYS ===

-- | Gera um valor padrão para inicializar arrays
-- int -> 0, bool -> false, string -> "", etc.
defaultValue :: Type -> RValue
defaultValue TInt = RInt 0
defaultValue TFloat = RFloat 0.0
defaultValue TBool = RBool False
defaultValue TString = RString ""
defaultValue (TVector _) = RArray [] -- Array de Arrays começa vazio
defaultValue (TStruct _) = RNull     -- Structs começam nulos
defaultValue _ = RNull

-- | Substitui o elemento no índice 'i' por 'newVal'
replaceAt :: Int -> RValue -> [RValue] -> [RValue]
replaceAt i newVal list
    | i < 0 || i >= length list = list -- Índice inválido (não deve acontecer se verificado antes)
    | otherwise = take i list ++ [newVal] ++ drop (i + 1) list
module Backend.SlWatGenerator where

import Frontend.Syntax.SlSyntax
import Control.Monad.Writer
import Control.Monad.State
-- import qualified Data.Map as Map
-- import Data.List (intercalate)

-- === 1. ESTRUTURAS ===

-- O WebAssembly tem tipos primitivos específicos
data WatType = I32 | F32 | Void
    deriving (Eq, Show)

-- Estado do Gerador (para gerar labels únicos de if/loop se precisar)
data GenState = GenState { labelCount :: Int }

-- Monad de Geração: Writer (acumula linhas de código) + State (conta labels)
type WatM a = WriterT [String] (State GenState) a

-- === 2. FUNÇÕES AUXILIARES ===

-- Adiciona uma linha ao código gerado
emit :: String -> WatM ()
emit line = tell [line]

-- Adiciona linha com indentação (estética)
emitI :: String -> WatM ()
emitI line = tell ["    " ++ line]

-- Converte tipos SL para tipos WAT
toWatType :: Type -> String
toWatType TInt   = "i32"
toWatType TBool  = "i32" -- Bool em WASM é i32 (0 ou 1)
toWatType TFloat = "f32"
toWatType TVoid  = ""
-- Arrays e Structs são ponteiros (i32), mas veremos isso na Parte 2
toWatType _      = "i32" 

-- === 3. GERAÇÃO DE EXPRESSÕES ===

genExp :: Exp -> WatM ()
genExp (EValue (VInt i))   = emitI $ "i32.const " ++ show i
genExp (EValue (VBool b))  = emitI $ "i32.const " ++ (if b then "1" else "0")
genExp (EValue (VFloat f)) = emitI $ "f32.const " ++ show f

genExp (EVar nome) = emitI $ "local.get $" ++ nome

-- Operações Aritméticas (Assumindo Inteiros por enquanto - O Fatorial usa Int)
genExp (e1 :+: e2) = do
    genExp e1
    genExp e2
    emitI "i32.add"

genExp (e1 :-: e2) = do
    genExp e1
    genExp e2
    emitI "i32.sub"

genExp (e1 :*: e2) = do
    genExp e1
    genExp e2
    emitI "i32.mul"

-- Divisão (Signed)
genExp (e1 :/: e2) = do
    genExp e1
    genExp e2
    emitI "i32.div_s" 

-- Comparações
genExp (e1 :<: e2) = do
    genExp e1
    genExp e2
    emitI "i32.lt_s" -- Signed less than

genExp (e1 :>: e2) = do
    genExp e1
    genExp e2
    emitI "i32.gt_s"

genExp (e1 :<=: e2) = do
    genExp e1
    genExp e2
    emitI "i32.le_s"

genExp (e1 :>=: e2) = do
    genExp e1
    genExp e2
    emitI "i32.ge_s"

genExp (e1 :==: e2) = do
    genExp e1
    genExp e2
    emitI "i32.eq"

-- Chamada de Função
-- Ex: (call $factorial (arg1) (arg2))
genExp (ECall (EVar nomeFun) args) = do
    mapM_ genExp args -- Gera código para empilhar cada argumento
    emitI $ "call $" ++ nomeFun

-- === IMPLEMENTAÇÃO DE ARRAYS (MEMÓRIA) ===

-- 1. Criação: new int[N]
-- Lógica: 
--    ptr = $heap_pointer
--    $heap_pointer = $heap_pointer + (N * 4 bytes)
--    return ptr
genExp (ENew _ sizeExpr) = do
    -- Pega o ponteiro atual (será o endereço do array)
    emitI "global.get $heap_pointer"
    
    -- Calcula o novo topo da memória: ptr + (size * 4)
    -- (Assumindo que int/float/ptr usam 4 bytes)
    emitI "global.get $heap_pointer" -- push ptr
    genExp sizeExpr                  -- push N
    emitI "i32.const 4"              -- push 4
    emitI "i32.mul"                  -- N * 4
    emitI "i32.add"                  -- ptr + (N*4)
    
    -- Salva o novo ponteiro globalmente
    emitI "global.set $heap_pointer"
    
    -- O 'global.get' inicial ficou na pilha, esse é o retorno do 'new' (o endereço)

-- 2. Acesso a Leitura: arr[i]
-- Endereço = arr_ptr + (i * 4)
-- Valor = load(Endereço)
genExp (EIndex exprArr exprIdx) = do
    genExp exprArr    -- Coloca o endereço base na pilha
    genExp exprIdx    -- Coloca o índice na pilha
    emitI "i32.const 4"
    emitI "i32.mul"   -- i * 4
    emitI "i32.add"   -- base + offset
    
    -- Lê da memória (i32.load)
    emitI "i32.load"

-- 3. Acesso a Tamanho (.size) - HACK SIMPLIFICADO
-- O WebAssembly "cru" não sabe o tamanho dos arrays a menos que a gente salve.
-- Uma implementação real salvaria o tamanho no endereço [ptr - 4].
-- Para este trabalho, se o professor não exigir rigor nisso, podemos omitir ou retornar 0.
-- Se precisar implementar, me avise que fazemos a lógica do "Header".
genExp (EArraySize _) = emitI "i32.const 0" -- Placeholder

-- === ADICIONE ISTO DENTRO DE genExp ===

-- Caso: Literal de Array ([10, 20, 30])
genExp (EVector exprs) = do
    -- 1. Deixa o endereço base na pilha (será o valor de retorno da expressão)
    emitI "global.get $heap_pointer"

    -- 2. Percorre a lista e salva cada elemento na memória
    --    (Zipamos com [0..] para saber o índice/offset)
    let storeElement (idx, expr) = do
            emitI "global.get $heap_pointer"       -- Endereço Base
            emitI $ "i32.const " ++ show (idx * 4) -- Offset (idx * 4 bytes)
            emitI "i32.add"                        -- Endereço Final (Base + Offset)
            genExp expr                            -- Gera o valor (ex: 10)
            emitI "i32.store"                      -- Salva na RAM
    
    mapM_ storeElement (zip [0 :: Int .. ] exprs)

    -- 3. Atualiza o ponteiro global do Heap (Alocação)
    let size = length exprs
    emitI "global.get $heap_pointer"
    emitI $ "i32.const " ++ show (size * 4) -- Tamanho total em bytes
    emitI "i32.add"
    emitI "global.set $heap_pointer"        -- heap_pointer += size*4

-- Caso padrão
genExp _ = return ()

-- === 4. GERAÇÃO DE STATEMENTS ===

genStmt :: Stmt -> WatM ()
genStmt (SReturn expr) = do
    genExp expr
    emitI "return"

-- IF / ELSE
-- WebAssembly If:
-- (if (condicao) (then ...) (else ...))
genStmt (SIf cond thenBlock elseBlock) = do
    genExp cond -- O resultado fica na pilha
    emitI "if"
    
    -- Bloco Then
    -- Nota: Precisamos mapear genStmt sobre a lista de statements do bloco
    mapM_ genStmt thenBlock
    
    -- Bloco Else (Opcional no WAT, mas nossa AST sempre tem)
    if not (null elseBlock) 
    then do
        emitI "else"
        mapM_ genStmt elseBlock
    else return ()
        
    emitI "end" -- Fim do If

genStmt (SExpr expr) = do
    genExp expr
    emitI "drop" 

-- Declaração/Atribuição (Assumindo que já declaramos os locals no topo da função)
genStmt (SLet var _ (Just expr)) = do
    genExp expr
    emitI $ "local.set $" ++ var

genStmt (SAssign (EVar var) expr) = do
    genExp expr
    emitI $ "local.set $" ++ var

-- Implementação do WHILE
genStmt (SWhile cond bloco) = do
    -- Precisamos de labels únicos para loops aninhados não se confundirem
    -- Vamos usar o contador do estado (GenState)
    st <- get
    let id = labelCount st
    put $ st { labelCount = id + 1 }
    
    let loopLabel = "$loop_" ++ show id
    let exitLabel = "$exit_" ++ show id

    emitI $ "block " ++ exitLabel
    emitI $ "  loop " ++ loopLabel
    
    -- 1. Gera código da condição
    genExp cond
    
    -- 2. Se a condição for FALSA (0), pula para fora (exit)
    -- i32.eqz retorna 1 se o valor for 0.
    emitI "    i32.eqz"
    emitI $ "    br_if " ++ exitLabel
    
    -- 3. Gera o corpo do loop
    mapM_ genStmt bloco
    
    -- 4. Volta para o início para testar a condição de novo
    emitI $ "    br " ++ loopLabel
    
    emitI "  end" -- Fim do loop
    emitI "end"   -- Fim do block

-- Atribuição em Array: arr[i] = val
-- Operação: i32.store (endereço) (valor)
genStmt (SAssign (EIndex exprArr exprIdx) exprVal) = do
    -- 1. Calcula o endereço: arr + (i * 4)
    genExp exprArr
    genExp exprIdx
    emitI "i32.const 4"
    emitI "i32.mul"
    emitI "i32.add" 
    
    -- 2. Gera o valor a ser salvo
    genExp exprVal
    
    -- 3. Grava na memória
    emitI "i32.store"

genStmt _ = return ()

-- === 5. GERAÇÃO DE FUNÇÕES E PROGRAMA ===

genFunc :: Definition -> WatM ()
genFunc (Dfunc nome _ params tipoRetorno bloco) = do
    let watRet = case tipoRetorno of
            Just t  -> "(result " ++ toWatType t ++ ")"
            Nothing -> ""
            
    -- Gera parâmetros: (param $n i32)
    let genParam (Param pName pType) = 
            let pT = case pType of Just t -> toWatType t; Nothing -> "i32"
            in "(param $" ++ pName ++ " " ++ pT ++ ")"
            
    let paramsStr = unwords (map genParam params)
    
    emit ""
    emit $ "(func $" ++ nome ++ " " ++ paramsStr ++ " " ++ watRet
    
    -- HACK: O WebAssembly exige que declaremos todas as variáveis locais no topo da função.
    -- (local $x i32) (local $y f32)
    -- Para fazer isso direito, precisaríamos escanear o 'bloco' procurando SLet.
    -- Vamos fazer uma varredura simples aqui.
    let locals = scanLocals bloco
    mapM_ emitI locals

    -- Gera o corpo
    mapM_ genStmt bloco
    
    -- Fecha a função (algumas funções precisam de valor padrão se não tiver return explícito)
    case tipoRetorno of
        Just TInt -> emitI "i32.const 0" -- Retorno default se faltar
        _ -> return ()
        
    emit ")" -- Fecha func

genFunc _ = return () -- Ignora Structs por enquanto

-- Varre o bloco procurando declarações de variáveis para declarar no topo do WASM
scanLocals :: [Stmt] -> [String]
scanLocals [] = []
scanLocals (SLet var tipo _:rest) = 
    ("(local $" ++ var ++ " " ++ toWatType tipo ++ ")") : scanLocals rest
scanLocals (SLetInfer var _:rest) = 
    -- Se for inferência, assumimos i32 por enquanto (limitação do MVP)
    ("(local $" ++ var ++ " i32)") : scanLocals rest 
scanLocals (SIf _ b1 b2:rest) = scanLocals b1 ++ scanLocals b2 ++ scanLocals rest
scanLocals (SWhile _ b:rest) = scanLocals b ++ scanLocals rest
scanLocals (_:rest) = scanLocals rest

-- Função Principal que gera o módulo inteiro
genModule :: Sl -> String
genModule (Sl defs) = 
    let initialSt = GenState 0
        action = do
            emit "(module"
            emitI "(memory $0 1)"      -- 1 Page (64kb) de RAM
            emitI "(export \"memory\" (memory $0))"
            
            -- Começa no endereço 4 (deixamos o 0 null por segurança)
            emitI "(global $heap_pointer (mut i32) (i32.const 4))"

            mapM_ genFunc defs
            
            -- Exporta a Main se existir
            emitI "(export \"main\" (func $main))"
            emit ")"
        
        (output, _) = runState (execWriterT action) initialSt
    in unlines output
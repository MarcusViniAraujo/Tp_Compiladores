module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Data.Tree (drawTree)

import Frontend.Lexer.SlLexer (lexer, runAlex) 
import Frontend.Parser.SlParser (slParser)
import Frontend.Pretty.SlPretty (prettyPrint) 
import Frontend.Parser.AstToTree (astToTree)

import Semantics.SlTypeChecker (checkProgram)
import Semantics.SlInterpreter (interpret)

import Backend.SlWatGenerator (genModule)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--lexer", file]  -> runLexer file
        ["--parser", file] -> runParser file
        ["--pretty", file] -> runPretty file
        ["--check", file]  -> runCheck file 
        ["--run", file]    -> runInterpreter file
        ["--wat", file]    -> runWat file  
        _ -> printUsage

printUsage :: IO ()
printUsage = do
    putStrLn "Uso: slc [opcao] <arquivo>"
    putStrLn "Opcoes:"
    putStrLn "  --lexer   : Executa apenas a analise lexica"
    putStrLn "  --parser  : Executa a analise sintatica"
    putStrLn "  --pretty  : Formata o codigo original"
    putStrLn "  --check   : Executa a verificacao de tipos (Semantica)" 
    putStrLn "  --run     : Executa o programa" 
    putStrLn "  --wat     : Gera o codigo em WebAssembly"
    exitFailure

runLexer :: FilePath -> IO ()
runLexer file = do
    content <- readFile file
    case lexer content of
        Left err -> do
            putStrLn $ "Erro Lexico: " ++ err
            exitFailure
        Right tokens -> mapM_ print tokens 

runParser :: FilePath -> IO ()
runParser file = do
    content <- readFile file
    case runAlex content slParser of
        Left err -> do
            putStrLn $ "Erro: " ++ err
            exitFailure
        Right ast -> do
            let astStringTree = astToTree ast 
            putStrLn (drawTree astStringTree)

runPretty :: FilePath -> IO ()
runPretty file = do
    content <- readFile file
    case runAlex content slParser of
        Left err -> do
            putStrLn $ "Erro: " ++ err
            exitFailure
        Right ast -> putStrLn (prettyPrint ast)

-- Função para rodar a Análise Semântica
runCheck :: FilePath -> IO ()
runCheck file = do
    content <- readFile file
    -- 1. Primeiro rodamos o Parser para obter a AST
    case runAlex content slParser of
        Left err -> do
            putStrLn $ "Erro Sintatico: " ++ err
            exitFailure
        Right ast -> 
            -- 2. Agora passamos a AST para o TypeChecker
            case checkProgram ast of
                Left typeErr -> do
                    putStrLn "\n[ERRO] SEMANTICO ENCONTRADO:"
                    print typeErr -- Vai usar o 'deriving Show' do TypeError
                    exitFailure
                Right _ -> do
                    putStrLn "\n[SUCESSO] O programa esta semanticamente correto."
                    exitSuccess

runInterpreter :: FilePath -> IO ()
runInterpreter file = do
    content <- readFile file
    case runAlex content slParser of
        Left err -> do
            putStrLn $ "Erro Sintatico: " ++ err
            exitFailure
        Right ast -> do
            -- Opcional: Rodar o TypeChecker antes para garantir
            case checkProgram ast of
                Left err -> putStrLn "Erro de Tipo detectado antes da execucao:" >> print err >> exitFailure
                Right _ -> interpret ast 

runWat :: FilePath -> IO ()
runWat file = do
    content <- readFile file
    case runAlex content slParser of
        Left err -> putStrLn ("Erro Sintatico: " ++ err) >> exitFailure
        Right ast -> do
            -- Opcional: Verificar tipos antes
            -- case checkProgram ast of
            --     Left err -> putStrLn "Erro de Tipo detectado antes da execucao:" >> print err >> exitFailure
            --     Right _ -> interpret ast 
            
            let watCode = genModule ast
            putStrLn watCode
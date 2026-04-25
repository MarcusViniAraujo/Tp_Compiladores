# Trabalho prático de BCC328 - Construção de Compiladores I

Este repositório contém a implementação completa do compilador para a linguagem **SL**, desenvolvido como projeto prático da disciplina de **Construção de Compiladores I (BCC328)** do DECOM/UFOP.

O compilador foi desenvolvido em **Haskell** e evoluiu em três etapas distintas, culminando na geração de código WebAssembly (Wasm) executável.

---

## 🏗️ Arquitetura do Projeto

O compilador segue um pipeline clássico de tradução, estruturado da seguinte forma:

1.  **Frontend (Lexer & Parser):** Analisadores léxico e sintático construídos com `Alex` e `Happy`.
2.  **Middle-end (Análise Semântica):** Verificador de tipos estático baseado em algoritmos de unificação e controle de escopo via mônada `Reader`.
3.  **Backend (Geração de Código):** Tradutor da Árvore de Sintaxe Abstrata (AST) para o formato textual do WebAssembly (`.wat`).

---

## 🚀 Etapas de Desenvolvimento

### Etapa 1: Análise Léxica e Sintática

- Construção da gramática da linguagem SL utilizando `Happy`.
- Implementação do analisador léxico com `Alex`.
- Geração da Árvore de Sintaxe Abstrata (AST).

### Etapa 2: Análise Semântica e Interpretador

- **SlTypeChecker.hs:** Implementação de verificação de tipos estática, unificação automática e detecção de erros semânticos.
- **SlInterpreter.hs:** Motor de execução capaz de simular memória mutável (através de Mônadas de Estado) para validar algoritmos como o Problema da Mochila.

### Etapa 3: Geração de Código WebAssembly

- Tradução da AST para _S-Expressions_ (`.wat`).
- Mapeamento de variáveis para registradores locais/globais do Wasm.
- Implementação de memória linear para vetores e gerenciamento de fluxo de controle (loops e blocos nativos).

---

## 🛠️ Como Utilizar

Certifique-se de ter o `GHC` e `Cabal` instalados.

### 1. Compilação

```bash
cabal build
## Instruções

- Iniciando container
docker-compose up -d
docker-compose exec sl bash
```
## Membros
[Gabriel Vilas](https://github.com/vilas000)
[Marcus Vinicius](https://github.com/MarcusViniAraujo)

```


```

# **Documentação do Analisador Léxico**

Este repositório contém a implementação de um **Analisador Léxico (Lexer)** para uma linguagem de programação personalizada. O objetivo principal do lexer é identificar **tokens** no código-fonte, classificá-los e rastrear sua posição (linha e coluna) no texto. Esta documentação detalha o funcionamento do código, os métodos disponíveis e as principais funcionalidades implementadas.

---

## **📋 Índice**
- [📚 Introdução](#📚-introdução)
- [🎯 Objetivo](#🎯-objetivo)
- [📦 Estrutura do Projeto](#📦-estrutura-do-projeto)
- [📚 Classes e Estruturas](#📚-classes-e-estruturas)
    - [1️⃣ Classe Token](#1️⃣-classe-token)
    - [2️⃣ Classe Lexer](#2️⃣-classe-lexer)
- [⚙️ Funcionalidades](#⚙️-funcionalidades)
- [🚀 Como Executar](#🚀-como-executar)
- [📖 Exemplos de Uso](#📖-exemplos-de-uso)
- [🚩 Possíveis Melhorias](#🚩-possíveis-melhorias)
- [📄 Licença](#📄-licença)

---

## **📚 Introdução**

O **Analisador Léxico** (ou **Lexer**) é a primeira etapa no processo de compilação de um programa. Ele recebe o código-fonte como entrada, identifica **tokens** e os classifica conforme as regras da linguagem. Um token pode ser uma palavra-chave (`IF`, `VAR`), um operador (`+`, `-`, `*`), um literal (`"texto"`, `1234`), entre outros.

---

## **🎯 Objetivo**

- **Analisar o código-fonte** e identificar tokens.
- **Rastrear a posição** (linha e coluna) de cada token.
- **Distinguir palavras-chave, identificadores, números, literais e operadores.**
- **Ignorar espaços em branco e comentários.**

---

## **📦 Estrutura do Projeto**

```
📁 Projeto
├── 📄 main.cpp        # Código-fonte principal contendo o lexer
└── 📄 README.md       # Documentação do projeto
```

---

## **📚 Classes e Estruturas**

A implementação é composta por duas principais classes:

### **1️⃣ Classe `Token`**
A classe **`Token`** representa cada unidade léxica identificada no código. Cada token possui as seguintes informações:

#### **Atributos**
| Atributo       | Tipo         | Descrição                                      |
|----------------|--------------|-----------------------------------------------|
| **id**         | `TokenId`    | O identificador do tipo de token (como `TOK_VAR`, `TOK_IF`, `TOK_IDENTIFIER`). |
| **value**      | `std::string`| O valor do token (por exemplo, o nome de uma variável). |
| **line**       | `int`        | Linha do código onde o token foi encontrado.  |
| **column**     | `int`        | Coluna onde o token foi encontrado.           |

#### **Métodos**
| Método         | Descrição                                     |
|----------------|-----------------------------------------------|
| **Construtor** | Inicializa o token com seus atributos (`id`, `value`, `line`, `column`). |
| **to_string()**| Converte o token em uma string legível para depuração. |

---

### **2️⃣ Classe `Lexer`**
A classe **`Lexer`** é responsável por identificar e classificar os tokens. Ela recebe o código-fonte e processa cada caractere até identificar um token.

#### **Atributos**
| Atributo           | Tipo           | Descrição                                      |
|--------------------|----------------|-----------------------------------------------|
| **input**          | `std::string`  | O código-fonte a ser analisado.              |
| **pos**            | `size_t`       | Posição atual no texto.                      |
| **current_line**   | `int`          | Linha atual no código.                       |
| **current_column** | `int`          | Coluna atual no código.                      |

#### **Métodos**
| Método              | Descrição                                      |
|---------------------|-----------------------------------------------|
| **peek()**           | Retorna o caractere atual sem avançar no texto. |
| **advance()**        | Move o cursor para o próximo caractere.       |
| **skip_whitespace()**| Ignora espaços, quebras de linha e tabulações. |
| **skip_comment()**   | Ignora comentários no formato `(* comentário *)`. |
| **parse_identifier()**| Identifica palavras-chave e identificadores. |
| **classify_keyword()**| Verifica se uma palavra é uma palavra-chave. |

---

## **⚙️ Funcionalidades**

1. **Identificação de Tokens**:
    - Palavras-chave (`IF`, `VAR`, `ELSE`, etc.).
    - Identificadores (nomes de variáveis, funções, etc.).
    - Números inteiros e reais (`123`, `45.67`).
    - Operadores (`+`, `-`, `*`, `/`, `=`, `!=`, `>=`, etc.).
    - Símbolos (`(`, `)`, `[`, `]`, `{`, `}`, `;`, `:`).

2. **Rastreamento de Linha e Coluna**:
    - Cada token identificado possui a linha e a coluna em que foi encontrado no código.

3. **Ignorar Comentários**:
    - Comentários no formato `(* comentário *)` são ignorados.

4. **Ignorar Espaços em Branco**:
    - Todos os espaços, tabulações e quebras de linha são ignorados.

---

## **🚀 Como Executar**

1. **Compilar o código:**
   ```bash
   g++ -o lexer main.cpp
   ```

2. **Executar o lexer com um arquivo de entrada:**
   ```bash
   ./lexer < arquivo.txt
   ```

O programa exibirá os tokens identificados e suas posições (linha e coluna) no código-fonte.

---

## **📖 Exemplos de Uso**

**Exemplo de Código de Entrada (`arquivo.txt`):**
```txt
VAR x : INT;
IF x > 10 THEN
    x := x + 1;
END_IF
```

**Saída do Lexer:**
```
Token(VAR, line: 1, col: 1)
Token(IDENTIFIER: x, line: 1, col: 5)
Token(COLON, line: 1, col: 7)
Token(INT, line: 1, col: 9)
Token(SEMICOLON, line: 1, col: 12)
Token(IF, line: 2, col: 1)
Token(IDENTIFIER: x, line: 2, col: 4)
Token(GREATER_THAN, line: 2, col: 6)
Token(INT_LITERAL: 10, line: 2, col: 8)
Token(THEN, line: 2, col: 11)
Token(IDENTIFIER: x, line: 3, col: 5)
Token(ASSIGN, line: 3, col: 8)
Token(IDENTIFIER: x, line: 3, col: 11)
Token(PLUS, line: 3, col: 13)
Token(INT_LITERAL: 1, line: 3, col: 15)
Token(END_IF, line: 4, col: 1)
```
# **Documentação do Analisador Sintático (Parser)**

Este repositório contém a implementação de um **Analisador Sintático (Parser)** para uma linguagem de programação personalizada. O objetivo principal do **Parser** é analisar uma sequência de **tokens** gerados pelo **Analisador Léxico** e verificar se eles estão de acordo com a **gramática da linguagem**. Caso estejam, uma **Árvore de Sintaxe Abstrata (AST)** é construída, permitindo análises e execuções posteriores.

---

## **📋 Índice**
- [📚 Introdução](#📚-introdução)
- [🎯 Objetivo](#🎯-objetivo)
- [📦 Estrutura do Projeto](#📦-estrutura-do-projeto)
- [📚 Classes e Estruturas](#📚-classes-e-estruturas)
    - [1️⃣ Classe ASTNode](#1️⃣-classe-astnode)
    - [2️⃣ Classe Parser](#2️⃣-classe-parser)
- [⚙️ Funcionalidades](#⚙️-funcionalidades)
- [🚀 Como Executar](#🚀-como-executar)
- [📖 Exemplos de Uso](#📖-exemplos-de-uso)
- [🚩 Possíveis Melhorias](#🚩-possíveis-melhorias)
- [📄 Licença](#📄-licença)

---

## **📚 Introdução**

O **Analisador Sintático** (ou **Parser**) é a **segunda etapa** do processo de compilação. Após o **Lexer** gerar a lista de **tokens**, o **Parser** verifica se a sequência dos tokens obedece à **gramática** da linguagem de programação.  
Caso seja válida, o **Parser** constrói uma **Árvore de Sintaxe Abstrata (AST)**, que será utilizada nas próximas etapas de análise semântica ou geração de código.  
Se a sequência de tokens não for válida, o Parser **gera mensagens de erro** apontando a posição do erro.

---

## **🎯 Objetivo**

- **Validar a sintaxe** do código-fonte, verificando se obedece à gramática.
- **Gerar uma Árvore de Sintaxe Abstrata (AST)**.
- **Fornecer mensagens de erro** indicando o local do erro e o motivo.

---

## **📦 Estrutura do Projeto**

```
📁 Projeto
├── 📄 main.cpp        # Código-fonte principal contendo o parser
└── 📄 README.md       # Documentação do projeto
```

---

## **📚 Classes e Estruturas**

A implementação é composta por duas principais classes:

---

### **1️⃣ Classe `ASTNode`**
A classe **`ASTNode`** (nó da árvore de sintaxe abstrata) é usada para representar os elementos da AST. Cada nó contém informações sobre o tipo de expressão, os filhos e o valor do nó.

#### **Atributos**
| Atributo       | Tipo          | Descrição                                      |
|----------------|---------------|-----------------------------------------------|
| **type**       | `NodeType`    | O tipo do nó (expressão, operação, literal, etc.). |
| **value**      | `std::string` | O valor associado ao nó (nome de variável, valor numérico, etc.). |
| **children**   | `std::vector<ASTNode*>` | Lista de nós filhos, permitindo criar árvores aninhadas. |

#### **Métodos**
| Método         | Descrição                                     |
|----------------|-----------------------------------------------|
| **Construtor** | Inicializa o nó com o tipo, valor e filhos.   |
| **add_child()**| Adiciona um filho à lista de filhos do nó.    |
| **to_string()**| Converte o nó e seus filhos em uma string legível para depuração. |

---

### **2️⃣ Classe `Parser`**
A classe **`Parser`** é responsável por processar a sequência de **tokens** e verificar se está de acordo com a **gramática**. Ela constrói a **Árvore de Sintaxe Abstrata (AST)**.

#### **Atributos**
| Atributo           | Tipo               | Descrição                                      |
|--------------------|-------------------|-----------------------------------------------|
| **tokens**         | `std::vector<Token>`| Lista de tokens gerados pelo lexer.           |
| **current_pos**    | `size_t`           | Posição atual no vetor de tokens.             |

#### **Métodos**
| Método             | Descrição                                              |
|---------------------|------------------------------------------------------|
| **parse()**         | Inicia o processo de análise sintática.                |
| **current_token()** | Retorna o token atual, sem mover o cursor.             |
| **advance()**       | Move para o próximo token.                            |
| **expect()**        | Verifica se o próximo token é do tipo esperado.       |
| **parse_expression()** | Analisa expressões de forma recursiva.             |
| **parse_statement()**  | Analisa comandos e instruções.                    |
| **error()**         | Emite um erro sintático, exibindo a posição no código. |

---

## **⚙️ Funcionalidades**

1. **Análise de Sintaxe**:
    - Verifica se o código está conforme a gramática da linguagem.
    - Exibe **erros sintáticos claros**, com a linha e a coluna do erro.

2. **Geração de AST (Árvore de Sintaxe Abstrata)**:
    - Cada comando, expressão ou operação forma um **nó** da árvore.
    - A AST permite realizar otimizações, verificações semânticas e geração de código.

3. **Suporte a Expressões e Comandos**:
    - **Comandos de atribuição** (`x := 10`).
    - **Expressões aritméticas** (`a + b * c`).
    - **Instruções de controle de fluxo** (`IF`, `WHILE`, etc.).

---

## **🚀 Como Executar**

1. **Compilar o código:**
   ```bash
   g++ -o parser main.cpp
   ```

2. **Executar o parser com um arquivo de entrada:**
   ```bash
   ./parser < arquivo.txt
   ```

O programa exibirá a **AST** ou apontará erros sintáticos no código-fonte.

---

## **📖 Exemplos de Uso**

**Exemplo de Código de Entrada (`arquivo.txt`):**
```txt
VAR x : INT;
x := 10 + 5 * 2;
IF x > 10 THEN
    x := x + 1;
END_IF
```

**Saída do Parser (AST):**
```
Program
  └── Declaration(VAR, x, INT)
  └── Assignment(x)
        └── Addition
            ├── Literal(10)
            └── Multiplication
                ├── Literal(5)
                └── Literal(2)
  └── IfCondition
        ├── GreaterThan(x, Literal(10))
        └── Body
            └── Assignment(x)
                └── Addition
                    ├── Identifier(x)
                    └── Literal(1)
```
# **Documentação do Analisador Semântico (Semantic Analyzer)**

O **Analisador Semântico** é a **terceira etapa do compilador**, responsável por verificar se a **Árvore de Sintaxe Abstrata (AST)** está semanticamente correta. Diferente do **Analisador Léxico** e do **Analisador Sintático**, que verificam se o código está **escrito corretamente**, o **Analisador Semântico** verifica **se o código faz sentido**.

---

## **📋 Índice**
- [📚 Introdução](#📚-introdução)
- [🎯 Objetivo](#🎯-objetivo)
- [📦 Estrutura do Projeto](#📦-estrutura-do-projeto)
- [📚 Classes e Estruturas](#📚-classes-e-estruturas)
    - [1️⃣ Classe SemanticAnalyzer](#1️⃣-classe-semanticanalyzer)
    - [2️⃣ Classe SymbolTable](#2️⃣-classe-symboltable)
- [⚙️ Funcionalidades](#⚙️-funcionalidades)
- [🚀 Como Executar](#🚀-como-executar)
- [📖 Exemplos de Uso](#📖-exemplos-de-uso)
- [🚩 Possíveis Melhorias](#🚩-possíveis-melhorias)
- [📄 Licença](#📄-licença)

---

## **📚 Introdução**

O **Analisador Semântico** verifica se as operações do código fazem sentido.  
Exemplos de **erros semânticos** incluem:
- **Variáveis não declaradas** sendo utilizadas.
- **Operações inválidas**, como `10 / "texto"`.
- **Tipos incompatíveis**, como `x := 10 + "abc"`.

Se a análise semântica for bem-sucedida, a **AST** permanece válida e o compilador pode prosseguir para as próximas etapas (análise intermediária ou geração de código). Caso contrário, o Analisador Semântico **emite mensagens de erro claras**.

---

## **🎯 Objetivo**

- **Verificar tipos de variáveis** para garantir operações coerentes (ex: `INT + STRING` não é permitido).
- **Garantir que todas as variáveis sejam declaradas antes de serem usadas**.
- **Analisar expressões e operações** para garantir que sejam **semanticamente válidas**.
- **Registrar a tabela de símbolos** para armazenar informações sobre as variáveis e seus tipos.

---

## **📦 Estrutura do Projeto**

```
📁 Projeto
├── 📄 main.cpp        # Código-fonte principal
├── 📄 semantic_analyzer.cpp  # Implementação do Analisador Semântico
├── 📄 semantic_analyzer.h    # Cabeçalho do Analisador Semântico
├── 📄 symbol_table.cpp       # Implementação da Tabela de Símbolos
├── 📄 symbol_table.h         # Cabeçalho da Tabela de Símbolos
└── 📄 README.md       # Documentação do projeto
```

---

## **📚 Classes e Estruturas**

A implementação do **Analisador Semântico** é composta por duas classes principais:

- **SemanticAnalyzer**: Verifica a AST e detecta erros de lógica e semântica.
- **SymbolTable**: Estrutura que armazena os símbolos (variáveis, funções, tipos) e permite consultar, inserir e verificar duplicações.

---

### **1️⃣ Classe `SemanticAnalyzer`**
A classe **`SemanticAnalyzer`** é o **coração do analisador semântico**. Ela percorre a **AST** e realiza verificações de tipo, uso de variáveis e declarações.

#### **Atributos**
| Atributo           | Tipo               | Descrição                                      |
|-------------------|-------------------|-----------------------------------------------|
| **symbol_table**   | `SymbolTable`     | Referência à tabela de símbolos.             |
| **errors**         | `std::vector<std::string>` | Lista de erros encontrados na análise. |

#### **Métodos**
| Método                | Descrição                                             |
|----------------------|------------------------------------------------------|
| **SemanticAnalyzer()**| Construtor, inicializa a tabela de símbolos.          |
| **analyze()**         | Recebe a AST e executa as verificações semânticas.    |
| **visit_node()**      | Visita cada nó da AST recursivamente.                 |
| **check_variable_declaration()** | Verifica se as variáveis usadas foram declaradas. |
| **check_type_compatibility()**    | Verifica compatibilidade de tipos em operações. |
| **report_error()**    | Adiciona mensagens de erro à lista de erros.          |

---

### **2️⃣ Classe `SymbolTable`**
A **Tabela de Símbolos** mantém informações sobre as **variáveis** e **funções** declaradas no programa.  
Para cada símbolo, são armazenados **nome**, **tipo** e **escopo**.

#### **Atributos**
| Atributo           | Tipo               | Descrição                                      |
|-------------------|-------------------|-----------------------------------------------|
| **symbols**        | `std::unordered_map<std::string, Symbol>` | Mapa de símbolos, onde o nome é a chave. |

#### **Métodos**
| Método                | Descrição                                             |
|----------------------|------------------------------------------------------|
| **add_symbol()**      | Adiciona um símbolo à tabela, lançando erro se duplicado. |
| **lookup()**          | Busca o símbolo pelo nome.                             |
| **exists()**          | Verifica se um símbolo já foi declarado.               |

---

## **⚙️ Funcionalidades**

1. **Verificação de Declaração de Variáveis**:
    - **Erro** se uma variável for usada antes de ser declarada.

2. **Verificação de Tipos**:
    - **Erro** se tentar somar `INT + STRING`.
    - **Erro** se uma função for chamada com argumentos errados.

3. **Detecção de Variáveis Duplicadas**:
    - **Erro** se a mesma variável for declarada duas vezes no mesmo escopo.

4. **Tabela de Símbolos**:
    - **Guarda as variáveis e seus tipos**.
    - **Permite buscas rápidas** para verificar se uma variável já foi declarada.

---

## **🚀 Como Executar**

1. **Compilar o código**:
   ```bash
   g++ -o semantic_analyzer semantic_analyzer.cpp symbol_table.cpp main.cpp
   ```

2. **Executar com arquivo de entrada**:
   ```bash
   ./semantic_analyzer < arquivo.txt
   ```

---

## **📖 Exemplos de Uso**

**Exemplo de Código de Entrada (`arquivo.txt`):**
```txt
VAR x : INT;
x := 10 + 5;
y := x + 1;  # ERRO: 'y' não foi declarado
``` 

**Saída do Analisador Semântico:**
```
Erro: Variável 'y' não foi declarada antes de ser usada. Linha 3.
```

## **💡 Conclusão**

O **Analisador Semântico** é responsável por garantir que o código-fonte **faça sentido**. Ele identifica **variáveis não declaradas**, **operações inválidas** e **inconsistências de tipo**.  
A **Tabela de Símbolos** armazena informações sobre as variáveis, garantindo que todas estejam corretamente declaradas antes de serem usadas.

Este Analisador Semântico faz parte de um compilador maior, composto por **Lexer**, **Parser**, **Semantic Analyzer** e, posteriormente, **Gerador de Código**.

Se precisar de melhorias, exemplos adicionais ou qualquer dúvida, estou aqui para ajudar! 😊
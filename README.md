# Compilador-st

DOCUMENTAÇÃO TÉCNICA EXTREMAMENTE DETALHADA DO COMPILADOR (IDEIA PRINCIPAL)

INTRODUÇÃO:
Este documento técnico detalha de forma ampla e minuciosa o funcionamento e estrutura interna do compilador desenvolvido para interpretar programas escritos em uma linguagem derivada do padrão IEC 61131, amplamente adotado em automação industrial. O compilador está implementado em C++ com objetivo claro de garantir manutenção eficiente e permitir expansões futuras.

TECNOLOGIA E AMBIENTE:

Linguagem de Programação: C++ (versão recomendada C++11 ou superior).

Bibliotecas padrão utilizadas: iostream, string, memory, vector, unordered_map, algorithm, cctype, cmath, fstream.

Ambiente de desenvolvimento recomendado: Linux, compatível também com adaptações para Windows.

Ferramentas de compilação sugeridas: GCC, Makefile ou CMake.

ESTRUTURA DETALHADA DO COMPILADOR:
O compilador está dividido claramente em módulos, permitindo uma fácil manutenção e futuras melhorias. Cada módulo é explicado detalhadamente abaixo.

DEFINIÇÕES DE TOKENS:
Tokens são unidades fundamentais de análise léxica, tais como palavras reservadas, operadores, símbolos especiais e identificadores. São representados internamente através de enumerações para garantir um processamento rápido e padronizado.

Exemplo prático de enumeração (trecho real do código):

enum TokenId {
    TOK_IF,            // Condicional IF
    TOK_ELSE,          // Condicional ELSE
    TOK_INT,           // Tipo inteiro
    TOK_REAL,          // Tipo real (ponto flutuante)
    TOK_STRING,        // Tipo string
    TOK_BOOL,          // Tipo booleano
    TOK_WHILE,         // Loop WHILE
    TOK_FOR,           // Loop FOR
    TOK_VAR,           // Declaração de variável
    TOK_FUNCTION,      // Declaração de função
    TOK_END_FUNCTION,  // Fim de função
    TOK_PROGRAM,       // Início de programa
    TOK_END_PROGRAM,   // Fim de programa
    TOK_IDENTIFIER,    // Identificador (nomes de variáveis, funções)
    // etc.
};

ANALISADOR LÉXICO:
O analisador léxico é responsável por ler o arquivo fonte (.st) e produzir uma sequência de tokens reconhecidos pelo compilador. Cada token é classificado de acordo com sua categoria (operador, identificador, palavra reservada).

Trecho do funcionamento básico:

Leitura sequencial do arquivo.

Identificação e classificação de caracteres (dígitos, letras, símbolos especiais).

Geração dos tokens com posicionamento exato para reportar erros claramente ao usuário.

ANALISADOR SINTÁTICO:
O analisador sintático recebe a sequência de tokens gerada pelo analisador léxico e verifica se estes seguem corretamente a gramática da linguagem IEC 61131. Ele utiliza técnicas como a análise recursiva descendente para construir uma árvore de análise sintática.

Exemplo estruturado (IF-ELSE):

IF (condição) THEN
    instruções;
ELSE
    instruções_alternativas;
END_IF;

ANALISADOR SEMÂNTICO:
Após a etapa de análise sintática, o analisador semântico verifica se as instruções têm lógica válida dentro do contexto do programa. Ele checa declarações, tipos de variáveis, compatibilidade de tipos e escopos.

Exemplo de verificação semântica:

Garantir que a variável usada esteja declarada previamente.

Verificar se operações matemáticas estão sendo realizadas entre tipos compatíveis (por exemplo, não permitir soma de booleanos com números).

GERADOR DE CÓDIGO:
Transforma a árvore sintática e semântica já validada em código intermediário ou binário executável. Este módulo pode incluir otimizações para melhor desempenho ou redução do uso de recursos.

Exemplo simplificado da etapa final:

Código fonte:

VAR
   x : INT := 10;
END_VAR

Código intermediário:

allocate_integer x
assign x, 10

DETALHES ADICIONAIS E ESPECIFICAÇÕES DO CÓDIGO:

Arquivo principal: main.cpp

Funções essenciais:

token_id_to_string(TokenId id) transforma enumerações em texto legível, útil para debug e logs.

Exemplo de código real:

std::string token_id_to_string(TokenId id) {
    switch(id) {
        case TOK_IF: return "TOK_IF";
        case TOK_ELSE: return "TOK_ELSE";
        case TOK_INT: return "TOK_INT";
        // outros casos aqui...
        default: return "UNKNOWN";
    }
}

RECOMENDAÇÕES TÉCNICAS PARA MANUTENÇÃO:

Atualizar enumerações ao adicionar novas funcionalidades.

Realizar testes unitários após modificações significativas.

Manter comentários detalhados e precisos.

Criar documentação interna (inline) constante nos novos códigos.

CONCLUSÃO:
Esta documentação técnica detalhada tem como objetivo proporcionar uma base completa e minuciosa sobre o compilador, facilitando profundamente a compreensão, manutenção e expansão do projeto por equipes técnicas especializadas e futuras contribuições de desenvolvimento.

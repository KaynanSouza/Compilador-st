#include <iostream>       // Para entradas e saídas (std::cout, std::cerr, etc.)
#include <string>         // Manipulação de strings
#include <cctype>         // Funções de classificação de caracteres (std::isspace, std::isalnum, etc.)
#include <cmath>
#include <memory>
#include <unordered_map>  // Mapa de hash (utilizado no classify_keyword)
#include <vector>         // Vetor dinâmico (utilizado para gerar código intermediário, armazenar instruções, etc.)
#include <stdexcept>      // Exceções padrão (std::runtime_error)
#include <fstream>        // Para leitura de arquivos (std::ifstream)

/*
 * Observações Gerais:
 *
 * - Este é um compilador básico, composto principalmente por 2 etapas: análise léxica (Lexer) e análise sintática (Parser).
 * - A análise léxica converte caracteres de entrada em "tokens".
 * - A análise sintática (Parser) verifica se a sequência de tokens faz sentido gramaticalmente
 *   e, em alguns trechos, também faz análises semânticas simples (ex: verificar se variável foi declarada).
 * - Por fim, é gerado um "código intermediário" (IR) que descreve as operações a serem realizadas.
 */

// ===============================
// 1) Definições de Tipos e Tokens
// ===============================

// Tipos básicos possíveis no nosso compilador
enum VarType {
    TYPE_INT,
    TYPE_REAL,
    TYPE_STRING,
    TYPE_BOOL,
    TYPE_ARRAY_INT,
    TYPE_ARRAY_REAL,
    TYPE_ARRAY_STRING,
    TYPE_ARRAY_BOOL,
    TYPE_UNKNOWN
};

// IDs (identificadores) possíveis para cada token que o lexer retorna
enum TokenId {
    // Palavras-chave
    TOK_VAR,
    TOK_END_VAR,
    TOK_FUNCTION,
    TOK_END_FUNCTION,
    TOK_OF,
    TOK_PROGRAM,
    TOK_END_PROGRAM,

    // Estruturas de controle (Condicional)
    TOK_IF,
    TOK_THEN,
    TOK_ELSE,
    TOK_END_IF,

    // Tipos básicos
    TOK_BOOL,
    TOK_INT,
    TOK_REAL,
    TOK_STRING,
    TOK_ARRAY,
    TOK_STRUCT,
    TOK_END_STRUCT,

    // Palavras reservadas (valores verdadeiros/falsos, parênteses, colchetes, etc.)
    TOK_TRUE,
    TOK_FALSE,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_LBRACKET,
    TOK_RBRACKET,
    TOK_IGUAL,
    TOK_ATRIBUICAO,
    TOK_COLON,
    TOK_SEMICOLON,
    TOK_COMMA,
    TOK_DOT_DOT,

    // Operadores aritméticos
    TOK_PLUS,
    TOK_MINUS,
    TOK_MULTIPLY,
    TOK_DIVIDE,
    TOK_MOD,

    // Operadores relacionais
    TOK_DIFERENTE,
    TOK_MAIOR,
    TOK_MENOR,
    TOK_MAIOR_IGUAL,
    TOK_MENOR_IGUAL,

    // Operadores lógicos
    TOK_AND,
    TOK_OR,
    TOK_NOT,
    TOK_XOR,

    // Literais
    TOK_INT_LITERAL,
    TOK_REAL_LITERAL,
    TOK_STRING_LITERAL,
    TOK_HEX_LITERAL,
    TOK_OCTAL_LITERAL,
    TOK_BINARY_LITERAL,
    TOK_IDENTIFIER,

    // Outros
    TOK_COMMENT,
    TOK_EOF,
    TOK_INVALID,
    TOK_ERROR
};

// Converte o enumerador `TokenId` em string, para fins de debug/log
std::string token_id_to_string(TokenId id) {
    switch(id) {
        case TOK_VAR: return "TOK_VAR";
        case TOK_END_VAR: return "TOK_END_VAR";
        case TOK_FUNCTION: return "TOK_FUNCTION";
        case TOK_END_FUNCTION: return "TOK_END_FUNCTION";
        case TOK_OF: return "TOK_OF";
        case TOK_PROGRAM: return "TOK_PROGRAM";
        case TOK_END_PROGRAM: return "TOK_END_PROGRAM";
        case TOK_IF: return "TOK_IF";
        case TOK_THEN: return "TOK_THEN";
        case TOK_ELSE: return "TOK_ELSE";
        case TOK_END_IF: return "TOK_END_IF";
        case TOK_BOOL: return "TOK_BOOL";
        case TOK_INT: return "TOK_INT";
        case TOK_REAL: return "TOK_REAL";
        case TOK_STRING: return "TOK_STRING";
        case TOK_ARRAY: return "TOK_ARRAY";
        case TOK_STRUCT: return "TOK_STRUCT";
        case TOK_END_STRUCT: return "TOK_END_STRUCT";
        case TOK_TRUE: return "TOK_TRUE";
        case TOK_FALSE: return "TOK_FALSE";
        case TOK_LPAREN: return "TOK_LPAREN";
        case TOK_RPAREN: return "TOK_RPAREN";
        case TOK_LBRACKET: return "TOK_LBRACKET";
        case TOK_RBRACKET: return "TOK_RBRACKET";
        case TOK_IGUAL: return "TOK_IGUAL";
        case TOK_ATRIBUICAO: return "TOK_ATRIBUICAO";
        case TOK_COLON: return "TOK_COLON";
        case TOK_SEMICOLON: return "TOK_SEMICOLON";
        case TOK_COMMA: return "TOK_COMMA";
        case TOK_DOT_DOT: return "TOK_DOT_DOT";
        case TOK_PLUS: return "TOK_PLUS";
        case TOK_MINUS: return "TOK_MINUS";
        case TOK_MULTIPLY: return "TOK_MULTIPLY";
        case TOK_DIVIDE: return "TOK_DIVIDE";
        case TOK_MOD: return "TOK_MOD";
        case TOK_DIFERENTE: return "TOK_DIFERENTE";
        case TOK_MAIOR: return "TOK_MAIOR";
        case TOK_MENOR: return "TOK_MENOR";
        case TOK_MAIOR_IGUAL: return "TOK_MAIOR_IGUAL";
        case TOK_MENOR_IGUAL: return "TOK_MENOR_IGUAL";
        case TOK_AND: return "TOK_AND";
        case TOK_OR: return "TOK_OR";
        case TOK_NOT: return "TOK_NOT";
        case TOK_XOR: return "TOK_XOR";
        case TOK_INT_LITERAL: return "TOK_INT_LITERAL";
        case TOK_REAL_LITERAL: return "TOK_REAL_LITERAL";
        case TOK_STRING_LITERAL: return "TOK_STRING_LITERAL";
        case TOK_HEX_LITERAL: return "TOK_HEX_LITERAL";
        case TOK_OCTAL_LITERAL: return "TOK_OCTAL_LITERAL";
        case TOK_BINARY_LITERAL: return "TOK_BINARY_LITERAL";
        case TOK_IDENTIFIER: return "TOK_IDENTIFIER";
        case TOK_COMMENT: return "TOK_COMMENT";
        case TOK_EOF: return "TOK_EOF";
        case TOK_INVALID: return "TOK_INVALID";
        case TOK_ERROR: return "TOK_ERROR";
        default: return "UNKNOWN_TOKEN";
    }
}

// ===============================
// 2) Estrutura de um Token
// ===============================

class Token {
public:
    TokenId id;         // Tipo do token (ex: TOK_INT, TOK_IDENTIFIER, etc.)
    std::string value;  // Texto contido no token (ex: "x", "123", "IF")
    int line;           // Linha em que o token foi encontrado
    int column;         // Coluna em que o token foi encontrado

    // Construtores
    Token() : id(TOK_INVALID), value(""), line(0), column(0) {}
    Token(TokenId id, std::string value, int line, int column)
        : id(id), value(std::move(value)), line(line), column(column) {}

    // Para fins de debug, retorna string com informação do token
    std::string to_string() const  {
        return "Token(" + value + ", line: " + std::to_string(line) +
               ", col: " + std::to_string(column) +
               ", id: " + token_id_to_string(id) + ")";
    }
};

// ===============================
// 3) Lexer (Analisador Léxico)
// ===============================
/*
 * Converte o código-fonte (string) em uma sequência de tokens.
 * Avança caractere a caractere, e vai reconhecendo padrões:
 * - Identificadores/keywords
 * - Números
 * - Strings
 * - Operadores, símbolos especiais, etc.
 * - Espaços em branco e comentários são descartados.
 */

class Lexer {
private:
    std::string input;   // Armazena todo o código-fonte
    size_t pos = 0;      // Posição atual no input
    int current_line = 1;
    int current_column = 1;

    // Retorna o caractere atual (ou '\0' se estiver no fim)
    char peek() const {
        return (pos < input.size()) ? input[pos] : '\0';
    }

    // Avança para o próximo caractere e atualiza linha/coluna
    char advance() {
        char current = peek();
        if (current == '\0') {
            // Fim de arquivo
            return '\0';
        }

        // Move para a próxima posição
        pos++;

        // Atualiza linha e coluna dependendo do caractere lido
        if (current == '\n') {
            current_line++;
            current_column = 1;
        }
        else if (current == '\t') {
            // Se quisermos que tab avance em 4 colunas
            current_column += 4;
        }
        else {
            current_column++;
        }
        return current;
    }

    // Ignora todos os caracteres em branco (espaços, tabs, newlines etc.)
    void skip_whitespace() {
        while (std::isspace((unsigned char)peek())) {
            advance();
        }
    }

    // Lê e descarta um comentário do tipo (* ... *)
    void skip_comment() {
        // Já sabemos que '(' e '*' estão na sequência
        advance(); // consome '('
        advance(); // consome '*'

        while (true) {
            char c = peek();
            // Se fim de arquivo antes de fechar comentário, avisamos
            if (c == '\0') {
                std::cerr << "Unterminated comment on line " << current_line << "\n";
                return;
            }
            // Se encontramos '*' e próximo for ')', encerra
            if (c == '*' && (pos + 1 < input.size()) && input[pos + 1] == ')') {
                // Consome '*' e ')'
                advance();
                advance();
                break;
            }
            // Avança até fechar
            advance();
        }
    }

    /*
     * parse_identifier():
     * Lê caracteres que formam um identificador (ou palavra-chave).
     * Identificadores normalmente começam com letra ou '_',
     * e podem conter letras, dígitos ou '_'.
     */
    std::string parse_identifier() {
        std::string ident;

        // Primeiro caractere deve ser letra ou '_'
        char first = peek();
        if (!(std::isalpha((unsigned char)first) || first == '_')) {
            // Retorna vazio para indicar que não é identificador válido
            return "";
        }

        // Continua enquanto for alfanumérico ou '_'
        while (std::isalnum((unsigned char)peek()) || peek() == '_') {
            ident.push_back(advance());
        }
        return ident;
    }

    // Verifica se a string corresponde a uma palavra-chave; caso não, é identificador comum
    TokenId classify_keyword(const std::string& ident) {
        static const std::unordered_map<std::string, TokenId> keywords = {
            {"VAR", TOK_VAR}, {"END_VAR", TOK_END_VAR},
            {"IF", TOK_IF}, {"THEN", TOK_THEN},
            {"ELSE", TOK_ELSE}, {"END_IF", TOK_END_IF},
            {"BOOL", TOK_BOOL}, {"INT", TOK_INT},
            {"REAL", TOK_REAL}, {"STRING", TOK_STRING},
            {"ARRAY", TOK_ARRAY}, {"STRUCT", TOK_STRUCT},
            {"AND", TOK_AND}, {"OR", TOK_OR},
            {"NOT", TOK_NOT}, {"XOR", TOK_XOR},
            {"FUNCTION", TOK_FUNCTION}, {"END_FUNCTION", TOK_END_FUNCTION},
            {"OF", TOK_OF}, {"MOD", TOK_MOD},
            {"PROGRAM", TOK_PROGRAM}, {"END_PROGRAM", TOK_END_PROGRAM},
            {"TRUE", TOK_TRUE}, {"FALSE", TOK_FALSE},
            {"END_STRUCT", TOK_END_STRUCT}
        };

        auto it = keywords.find(ident);
        return (it != keywords.end()) ? it->second : TOK_IDENTIFIER;
    }

    // Lê um número (inteiro ou real simples)
    Token parse_number() {
        std::string num;
        bool is_real = false;

        // Continua enquanto houver dígitos ou ponto '.'
        while (std::isdigit((unsigned char)peek()) || peek() == '.') {
            // Se encontramos ponto e já tínhamos encontrado antes, paramos
            // (limitação simples para só permitir um ponto)
            if (peek() == '.') {
                if (is_real) break;
                // Se próximo char também é '.', é range '..', paramos
                if (pos + 1 < input.size() && input[pos + 1] == '.') {
                    break;
                }
                is_real = true;
            }
            num.push_back(advance());
        }

        // Retorna token de literal real ou inteiro
        if (is_real) {
            return Token(TOK_REAL_LITERAL, num, current_line, current_column);
        } else {
            return Token(TOK_INT_LITERAL, num, current_line, current_column);
        }
    }

    // Lê uma string entre aspas simples ou duplas (ex: "Olá Mundo")
    Token parse_string(char delimiter) {
        advance(); // consome a aspa inicial
        std::string str;

        while (true) {
            char c = peek();
            if (c == '\0') {
                // String não foi fechada
                std::cerr << "Unterminated string on line " << current_line << "\n";
                return Token(TOK_INVALID, str, current_line, current_column);
            }
            // Se encontrar o mesmo delimitador, fecha a string
            if (c == delimiter) {
                advance(); // consome a aspa final
                break;
            }
            str.push_back(advance());
        }
        return Token(TOK_STRING_LITERAL, str, current_line, current_column);
    }

    // Lida com '.' e '..' (range)
    Token parse_range_or_dot() {
        advance(); // consome '.'
        // Se for "..", gera token DOT_DOT
        if (peek() == '.') {
            advance();
            return Token(TOK_DOT_DOT, "..", current_line, current_column);
        }
        // Senão, é somente '.' (mas não temos tratamento especial, então retorna token inválido ou adaptado)
        return Token(TOK_INVALID, ".", current_line, current_column);
    }

public:
    // Construtor recebe o código-fonte como string
    explicit Lexer(std::string input_code)
        : input(std::move(input_code)) {}

    // Função principal do lexer, retorna o próximo token
    Token next_token() {
        skip_whitespace();  // Ignora espaços, tabs, etc.

        char c = peek();
        if (c == '\0') {
            return Token(TOK_EOF, "EOF", current_line, current_column);
        }

        // Reconhece comentário do tipo (* ... *)
        if (c == '(' && (pos + 1 < input.size()) && input[pos + 1] == '*') {
            skip_comment();
            return next_token();
        }

        // Identificadores (ou palavras-chave)
        if (std::isalpha((unsigned char)c) || c == '_') {
            int start_line = current_line;
            int start_col = current_column;
            std::string ident = parse_identifier();
            if (ident.empty()) {
                // Se por algum motivo não conseguiu ler, retorna inválido
                return Token(TOK_INVALID, std::string(1, advance()), start_line, start_col);
            }
            TokenId id = classify_keyword(ident);
            return Token(id, ident, start_line, start_col);
        }

        // Números
        if (std::isdigit((unsigned char)c)) {
            return parse_number();
        }

        // Strings entre aspas simples ou duplas
        if (c == '"' || c == '\'') {
            return parse_string(c);
        }

        // Símbolos, operadores, pontuação
        switch (c) {
            case '+': advance(); return Token(TOK_PLUS, "+", current_line, current_column);
            case '-': advance(); return Token(TOK_MINUS, "-", current_line, current_column);
            case '*': advance(); return Token(TOK_MULTIPLY, "*", current_line, current_column);
            case '/': advance(); return Token(TOK_DIVIDE, "/", current_line, current_column);
            case '[': advance(); return Token(TOK_LBRACKET, "[", current_line, current_column);
            case ']': advance(); return Token(TOK_RBRACKET, "]", current_line, current_column);
            case '.': return parse_range_or_dot();
            case ':':
                advance();
                // Se for ':=', é atribuição
                if (peek() == '=') {
                    advance();
                    return Token(TOK_ATRIBUICAO, ":=", current_line, current_column);
                }
                return Token(TOK_COLON, ":", current_line, current_column);
            case ';': advance(); return Token(TOK_SEMICOLON, ";", current_line, current_column);
            case ',': advance(); return Token(TOK_COMMA, ",", current_line, current_column);
            case '(':
                advance();
                return Token(TOK_LPAREN, "(", current_line, current_column);
            case ')':
                advance();
                return Token(TOK_RPAREN, ")", current_line, current_column);
            case '>':
            {
                advance();
                if (peek() == '=') {
                    advance();
                    return Token(TOK_MAIOR_IGUAL, ">=", current_line, current_column);
                }
                return Token(TOK_MAIOR, ">", current_line, current_column);
            }
            case '<':
            {
                advance();
                if (peek() == '=') {
                    advance();
                    return Token(TOK_MENOR_IGUAL, "<=", current_line, current_column);
                }
                else if (peek() == '>') {
                    advance();
                    return Token(TOK_DIFERENTE, "<>", current_line, current_column);
                }
                return Token(TOK_MENOR, "<", current_line, current_column);
            }
            default:
            {
                // Caractere não reconhecido
                char invalid_char = advance();
                return Token(TOK_INVALID, std::string(1, invalid_char), current_line, current_column);
            }
        }
    }

    // Retorna a posição atual no arquivo (usado para debug/erros)
    size_t getPos() const { return pos; }
    // Retorna a string do código-fonte completo
    const std::string &getInput() const { return input; }
};

// ===============================
// 4) Tabela de Símbolos
// ===============================
/*
 * Mapeia nomes de variáveis -> tipo delas (VarType).
 * Ex: "x" -> TYPE_INT
 *     "texto" -> TYPE_STRING
 * Esse é um sistema bem simples de tabela de símbolos.
 */

class SymbolTable {
private:
    std::unordered_map<std::string, VarType> table; // Hash map com nome->tipo
public:
    // Declaração: insere ou atualiza o tipo associado ao nome
    void declareVar(const std::string &name, VarType type) {
        table[name] = type;
    }

    // Verifica se existe uma entrada para 'name'
    bool isDeclared(const std::string &name) const {
        return (table.find(name) != table.end());
    }

    // Obtém o tipo associado a 'name'
    VarType getType(const std::string &name) const {
        auto it = table.find(name);
        if (it == table.end()) return TYPE_UNKNOWN;
        return it->second;
    }
};

// ===============================
// 5) Parser (Analisador Sintático)
// ===============================
/*
 * O parser recebe tokens do Lexer e verifica se eles obedecem à gramática definida.
 * Também constrói alguma forma de representação intermediária (neste caso, strings de "código intermediário").
 */

class Parser {
private:
    Lexer &lexer;                 // Referência ao lexer para obter tokens
    Token current_token;          // Token atual em análise
    SymbolTable symtab;           // Tabela de símbolos para declarações e verificações
    std::vector<std::string> code; // Armazena instruções de código intermediário

    // Declarações de funções internas ao parser:
    void parseProgram();
    void parseVarBlock();
    bool isVarBlockStart();
    bool isVarDeclarationStart();
    void parseVarDeclaration();
    VarType mapBasicType(TokenId id);
    VarType parseTypeAndGetVarType();
    void parseStructDeclaration();
    void parseValue();
    void parseNumber();
    void parseInstructions();
    bool isInstructionStart();
    void parseInstruction();
    void parseAssignment();

    // EXPRESSÕES
    std::string parseExpressionToIR();
    std::string parseTermToIR();
    std::string parseFactorToIR();

    // CONDIÇÕES (IFs, operadores lógicos)
    void parseIfStatement();
    std::string parseComparisonToIR();
    std::string parseConditionToIR();
    void parseRelationalOperator();
    void parseCondition();        // (usada em outro ponto, ex: parseCondition -> parseExpression -> parseRelationalOperator -> parseExpression)
    void parseExpression();
    void parseTerm();
    void parseFactor();

    // ARRAYS (inicialização de arrays)
    std::vector<std::pair<std::string,int>> parseArrayInitializer();
    std::pair<std::string,int> parseInitValue();

    // Funções auxiliares de verificação e manipulação de tokens
    void expect(TokenId expected);
    bool check(TokenId id);
    void advance();

    // Funções de erros
    void parser_error(const Token &tok, const std::string &msg);
    void semantic_error(const Token &tok, const std::string &msg);

    // Gera um novo temporário (t1, t2, t3, etc.)
    std::string newTemp() {
        static int temp_count = 0;
        return "t" + std::to_string(++temp_count);
    }

public:
    // Construtor: inicia lendo o primeiro token
    explicit Parser(Lexer &l) : lexer(l) {
        current_token = lexer.next_token();
    }

    // Ponto de entrada do parser
    void parse();
};

// ==========================================================
// Métodos de avanço, verificação e erros do Parser
// ==========================================================

// Lê o próximo token do lexer e atualiza current_token
void Parser::advance() {
    current_token = lexer.next_token();
}

// Erro de sintaxe: mostra mensagem e levanta exceção
void Parser::parser_error(const Token &tok, const std::string &msg) {
    std::cerr << "Erro de sintaxe na linha " << tok.line
              << ", coluna " << tok.column
              << ": " << msg
              << " (encontrado: " << tok.value << ")\n";

    // Para mostrar a linha do código onde ocorreu o erro
    const std::string &input = lexer.getInput();
    size_t filePos = lexer.getPos();

    // Localiza a posição de início e fim da linha
    size_t line_start = input.rfind('\n', filePos);
    if (line_start == std::string::npos) {
        line_start = 0;
    } else {
        line_start++;
    }

    size_t line_end = input.find('\n', filePos);
    if (line_end == std::string::npos) {
        line_end = input.size();
    }

    // Extrai a linha para exibir
    std::string line_str = input.substr(line_start, line_end - line_start);
    std::cerr << line_str << "\n";

    // Imprime um '^' apontando a coluna
    for (int i = 1; i < tok.column; i++) {
        std::cerr << " ";
    }
    std::cerr << "^\n";

    throw std::runtime_error("Erro de sintaxe.");
}

// Erro semântico: mostra mensagem e levanta exceção
// (ex: usar variável não declarada)
void Parser::semantic_error(const Token &tok, const std::string &msg) {
    std::cerr << "Erro semântico na linha " << tok.line
              << ", coluna " << tok.column
              << ": " << msg
              << " (encontrado: " << tok.value << ")\n";
    throw std::runtime_error("Erro semântico.");
}

// Verifica se o token atual é o esperado; se for, avança. Senão, dá erro
void Parser::expect(TokenId expected) {
    if (current_token.id == expected) {
        advance();
    } else {
        parser_error(current_token, "Token esperado não encontrado");
    }
}

// Retorna true se o token atual é igual ao id passado
bool Parser::check(TokenId id) {
    return current_token.id == id;
}

// ==========================================================
// Mapeia tipos de token para VarType
// ==========================================================
VarType Parser::mapBasicType(TokenId id) {
    switch (id) {
        case TOK_INT: return TYPE_INT;
        case TOK_REAL: return TYPE_REAL;
        case TOK_BOOL: return TYPE_BOOL;
        case TOK_STRING: return TYPE_STRING;
        default: return TYPE_UNKNOWN;
    }
}

// ==========================================================
// 6) Regras de Gramática Principais
// ==========================================================

// parseTypeAndGetVarType(): lê tokens que representam tipos (int, real, array, struct, etc.)
VarType Parser::parseTypeAndGetVarType() {
    // Se for BOOL, INT, REAL ou STRING
    if (check(TOK_BOOL) || check(TOK_INT) || check(TOK_REAL) || check(TOK_STRING)) {
        VarType t = mapBasicType(current_token.id);
        advance();
        return t;
    }
    // Se for "ARRAY"
    else if (check(TOK_ARRAY)) {
        advance();
        expect(TOK_LBRACKET);
        parseNumber();     // Pega o limite inferior (ex: 1)
        expect(TOK_DOT_DOT);
        parseNumber();     // Pega o limite superior (ex: 5)
        expect(TOK_RBRACKET);
        expect(TOK_OF);
        VarType elemType = parseTypeAndGetVarType();

        // Define o tipo de array baseado no tipo interno
        if (elemType == TYPE_INT) return TYPE_ARRAY_INT;
        if (elemType == TYPE_REAL) return TYPE_ARRAY_REAL;
        if (elemType == TYPE_BOOL) return TYPE_ARRAY_BOOL;
        if (elemType == TYPE_STRING) return TYPE_ARRAY_STRING;

        return TYPE_UNKNOWN;
    }
    // Se for "STRUCT"
    else if (check(TOK_STRUCT)) {
        advance();
        // Lê declarações internas (ex: campos de struct) até encontrar END_STRUCT
        while (check(TOK_IDENTIFIER)) {
            parseStructDeclaration();
        }
        expect(TOK_END_STRUCT);
        // Por simplicidade, retornamos TYPE_UNKNOWN
        return TYPE_UNKNOWN;
    }
    else {
        // Se não for nenhum dos casos, erro de tipo
        parser_error(current_token, "Tipo inválido");
        return TYPE_UNKNOWN;
    }
}

// parseStructDeclaration(): quando estamos dentro de um STRUCT, lemos um identificador + : + tipo + ;
void Parser::parseStructDeclaration() {
    expect(TOK_IDENTIFIER); // nome do campo
    expect(TOK_COLON);
    parseTypeAndGetVarType();  // tipo do campo
    expect(TOK_SEMICOLON);
}

// parseValue(): para ler um valor simples (int, real, string, booleano)
void Parser::parseValue() {
    if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL)
        || check(TOK_STRING_LITERAL) || check(TOK_TRUE) || check(TOK_FALSE))
    {
        advance(); // só consome
    } else {
        parser_error(current_token, "Valor inválido na atribuição");
    }
}

// parseNumber(): para ler um número inteiro ou real
void Parser::parseNumber() {
    if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL)) {
        advance();
    } else {
        parser_error(current_token, "Número esperado");
    }
}

// Verifica se é o início de um bloco VAR
bool Parser::isVarBlockStart() {
    return check(TOK_VAR);
}

// Verifica se é o início de uma declaração de variável (identificador)
bool Parser::isVarDeclarationStart() {
    return check(TOK_IDENTIFIER);
}

// parseVarDeclaration(): parseia algo do tipo "x : INT := 10;"
void Parser::parseVarDeclaration() {
    Token varName = current_token;
    expect(TOK_IDENTIFIER); // consome o nome da variável
    expect(TOK_COLON);
    VarType varType = parseTypeAndGetVarType(); // obtém o tipo

    bool initialized = false;
    // Se tiver ':=' significa inicialização
    if (check(TOK_ATRIBUICAO)) {
        advance(); // consome ':='
        // Se for colchete '[', significa inicialização de array
        if (check(TOK_LBRACKET)) {
            // Verifica se a variável é de tipo array
            if (varType != TYPE_ARRAY_INT && varType != TYPE_ARRAY_REAL
                && varType != TYPE_ARRAY_BOOL && varType != TYPE_ARRAY_STRING) {
                semantic_error(current_token, "Inicialização de array em variável não-array");
            }
            auto init_values = parseArrayInitializer();

            // Gera código intermediário para armazenar os valores no array
            int index = 1;
            for (auto &val_rep : init_values) {
                std::string val = val_rep.first;
                int rep = val_rep.second;
                for (int i = 0; i < rep; i++) {
                    code.push_back("STORE_INDEX " + val + " " + varName.value + " " + std::to_string(index));
                    index++;
                }
            }
            initialized = true;
        } else {
            // Inicialização escalar (ex: x := 10)
            Token lit = current_token;
            if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL)
                || check(TOK_STRING_LITERAL) || check(TOK_TRUE) || check(TOK_FALSE))
            {
                advance();
                std::string temp = newTemp();
                // Carrega valor imediato
                if (lit.id == TOK_INT_LITERAL || lit.id == TOK_REAL_LITERAL) {
                    code.push_back("LOAD_IMM " + lit.value + " " + temp);
                } else if (lit.id == TOK_STRING_LITERAL) {
                    code.push_back("LOAD_IMM \"" + lit.value + "\" " + temp);
                } else if (lit.id == TOK_TRUE) {
                    code.push_back("LOAD_IMM 1 " + temp);
                } else if (lit.id == TOK_FALSE) {
                    code.push_back("LOAD_IMM 0 " + temp);
                }
                // Armazena em varName
                code.push_back("STORE " + temp + " " + varName.value);
                initialized = true;
            } else {
                parser_error(current_token, "Valor inválido na atribuição");
            }
        }
    }

    // Esperamos um ponto e vírgula para finalizar declaração
    expect(TOK_SEMICOLON);

    // Declara a variável na tabela de símbolos
    symtab.declareVar(varName.value, varType);
}

// parseArrayInitializer(): parseia algo como [1, 2, 3] ou [2(3), 5, 6]
std::vector<std::pair<std::string,int>> Parser::parseArrayInitializer() {
    std::vector<std::pair<std::string,int>> values;
    expect(TOK_LBRACKET);
    // Se não for imediatamente ']', parseia os valores
    if (!check(TOK_RBRACKET)) {
        do {
            values.push_back(parseInitValue());
        } while (check(TOK_COMMA) && (advance(), true));
        // Se ver ",", avança e parseia mais um valor
    }
    expect(TOK_RBRACKET);
    return values;
}

// parseInitValue(): parseia cada valor do array, permitindo repetição ex: 2(3)
std::pair<std::string,int> Parser::parseInitValue() {
    // parseExpressionToIR() retorna um nome de registrador temporário (ex: t1)
    std::string val = parseExpressionToIR();
    int rep = 1;

    // Se tiver '(' e um número, ex: 2(3), significa 2 repetido 3 vezes
    if (check(TOK_LPAREN)) {
        advance(); // consome '('
        if (!check(TOK_INT_LITERAL)) {
            parser_error(current_token, "Número esperado após '('");
        }
        int count = std::stoi(current_token.value);
        advance(); // consome o literal
        expect(TOK_RPAREN);
        rep = count;
    }
    return {val, rep};
}

// parseInstructions(): lê várias instruções (atribuições, if, etc.) até esgotar
void Parser::parseInstructions() {
    while (isInstructionStart()) {
        parseInstruction();
    }
}

// isInstructionStart(): instruções podem começar com identificador ou if
bool Parser::isInstructionStart() {
    return check(TOK_IDENTIFIER) || check(TOK_IF);
}

// parseInstruction(): decide se é atribuição ou if
void Parser::parseInstruction() {
    if (check(TOK_IDENTIFIER)) {
        parseAssignment();
    } else if (check(TOK_IF)) {
        parseIfStatement();
    } else {
        parser_error(current_token, "Instrução inválida");
    }
}

// parseAssignment(): algo como x := expr;
void Parser::parseAssignment() {
    Token varName = current_token;
    expect(TOK_IDENTIFIER);

    // Verifica se a variável existe
    if (!symtab.isDeclared(varName.value)) {
        semantic_error(varName, "Variável não declarada");
    }

    // Se for array, processa [índices]
    VarType varType = symtab.getType(varName.value);
    std::vector<std::string> indices;
    while (check(TOK_LBRACKET)) {
        // Verifica se de fato é array
        if (varType != TYPE_ARRAY_INT && varType != TYPE_ARRAY_REAL
            && varType != TYPE_ARRAY_BOOL && varType != TYPE_ARRAY_STRING) {
            semantic_error(current_token, "Acesso a array em variável não-array");
        }
        advance(); // consome '['
        std::string idx = parseExpressionToIR();
        expect(TOK_RBRACKET);

        // Ajusta varType do "resto" (se fosse multidimensional, repetiria)
        if (varType == TYPE_ARRAY_INT) varType = TYPE_INT;
        else if (varType == TYPE_ARRAY_REAL) varType = TYPE_REAL;
        else if (varType == TYPE_ARRAY_BOOL) varType = TYPE_BOOL;
        else if (varType == TYPE_ARRAY_STRING) varType = TYPE_STRING;

        indices.push_back(idx);
    }

    // Lê ':='
    expect(TOK_ATRIBUICAO);

    // Lê a expressão do lado direito
    std::string rhs = parseExpressionToIR();

    expect(TOK_SEMICOLON);

    // Se não houve índices, é variável simples
    if (indices.empty()) {
        code.push_back("STORE " + rhs + " " + varName.value);
    } else {
        // Caso unidimensional, usa somente o primeiro índice
        code.push_back("STORE_INDEX " + rhs + " " + varName.value + " " + indices[0]);
    }
}

// ==========================================================
// 7) Expressões e Geração de IR (código intermediário)
// ==========================================================

// parseExpressionToIR(): parseia expressão que pode ter + e -
std::string Parser::parseExpressionToIR() {
    std::string left = parseTermToIR();
    while (check(TOK_PLUS) || check(TOK_MINUS)) {
        Token op = current_token;
        advance();
        std::string right = parseTermToIR();
        std::string temp = newTemp();
        // ADD ou SUB no IR
        code.push_back((op.id == TOK_PLUS ? "ADD" : "SUB") + std::string(" ")
                       + left + " " + right + " " + temp);
        left = temp;
    }
    return left;
}

// parseTermToIR(): parseia termos com * / mod
std::string Parser::parseTermToIR() {
    std::string left = parseFactorToIR();
    while (check(TOK_MULTIPLY) || check(TOK_DIVIDE) || check(TOK_MOD)) {
        Token op = current_token;
        advance();
        std::string right = parseFactorToIR();
        std::string temp = newTemp();
        std::string instr;
        if (op.id == TOK_MULTIPLY) instr = "MUL";
        else if (op.id == TOK_DIVIDE) instr = "DIV";
        else instr = "MOD";

        code.push_back(instr + " " + left + " " + right + " " + temp);
        left = temp;
    }
    return left;
}

// parseFactorToIR(): parseia fatores, que podem ser (expr), variáveis ou literais
std::string Parser::parseFactorToIR() {
    if (check(TOK_LPAREN)) {
        // ( expr )
        advance();
        std::string res = parseExpressionToIR();
        expect(TOK_RPAREN);
        return res;
    }
    else if (check(TOK_IDENTIFIER)) {
        // Carregar variável ou array
        Token varName = current_token;
        advance();
        if (!symtab.isDeclared(varName.value)) {
            semantic_error(varName, "Uso de variável não declarada");
        }

        VarType varType = symtab.getType(varName.value);
        std::vector<std::string> indices;
        while (check(TOK_LBRACKET)) {
            if (varType != TYPE_ARRAY_INT && varType != TYPE_ARRAY_REAL
                && varType != TYPE_ARRAY_BOOL && varType != TYPE_ARRAY_STRING) {
                semantic_error(current_token, "Acesso a array em variável não-array");
            }
            advance();
            std::string idx = parseExpressionToIR();
            expect(TOK_RBRACKET);

            if (varType == TYPE_ARRAY_INT) varType = TYPE_INT;
            else if (varType == TYPE_ARRAY_REAL) varType = TYPE_REAL;
            else if (varType == TYPE_ARRAY_BOOL) varType = TYPE_BOOL;
            else if (varType == TYPE_ARRAY_STRING) varType = TYPE_STRING;

            indices.push_back(idx);
        }

        // Gera instrução LOAD ou LOAD_INDEX
        std::string temp = newTemp();
        if (indices.empty()) {
            code.push_back("LOAD " + varName.value + " " + temp);
        } else {
            code.push_back("LOAD_INDEX " + varName.value + " " + indices[0] + " " + temp);
        }
        return temp;
    }
    else if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL)) {
        // Inteiro ou real
        Token lit = current_token;
        advance();
        std::string temp = newTemp();
        code.push_back("LOAD_IMM " + lit.value + " " + temp);
        return temp;
    }
    else if (check(TOK_STRING_LITERAL)) {
        // Literal string
        Token lit = current_token;
        advance();
        std::string temp = newTemp();
        code.push_back("LOAD_IMM \"" + lit.value + "\" " + temp);
        return temp;
    }
    else if (check(TOK_TRUE)) {
        // true -> 1
        Token lit = current_token;
        advance();
        std::string temp = newTemp();
        code.push_back("LOAD_IMM 1 " + temp);
        return temp;
    }
    else if (check(TOK_FALSE)) {
        // false -> 0
        Token lit = current_token;
        advance();
        std::string temp = newTemp();
        code.push_back("LOAD_IMM 0 " + temp);
        return temp;
    }
    else {
        parser_error(current_token, "Fator inválido");
        return "";
    }
}

// ==========================================================
// 8) Comandos IF e Expressões Lógicas
// ==========================================================

// parseIfStatement(): reconhece "IF <cond> THEN <instrucoes> [ELSE <instrucoes>] END_IF"
void Parser::parseIfStatement() {
    expect(TOK_IF);
    // Agora usamos parseConditionToIR() para lidar com operadores lógicos e comparações
    std::string cond = parseConditionToIR();

    expect(TOK_THEN);
    parseInstructions();

    if (check(TOK_ELSE)) {
        advance();
        parseInstructions();
    }

    expect(TOK_END_IF);
}

// parseComparisonToIR(): lida com "expr < expr", "expr > expr", etc.
std::string Parser::parseComparisonToIR() {
    // Lê expressão aritmética do lado esquerdo
    std::string left = parseExpressionToIR();

    // Se o token atual for um operador relacional, parseia
    if (check(TOK_IGUAL) || check(TOK_DIFERENTE) ||
        check(TOK_MENOR) || check(TOK_MENOR_IGUAL) ||
        check(TOK_MAIOR) || check(TOK_MAIOR_IGUAL))
    {
        Token op = current_token;
        parseRelationalOperator();
        // Lê expressão do lado direito
        std::string right = parseExpressionToIR();

        std::string temp = newTemp();
        std::string instr;
        switch (op.id) {
            case TOK_IGUAL:        instr = "COMP_EQ"; break;
            case TOK_DIFERENTE:    instr = "COMP_NE"; break;
            case TOK_MENOR:        instr = "COMP_LT"; break;
            case TOK_MENOR_IGUAL:  instr = "COMP_LE"; break;
            case TOK_MAIOR:        instr = "COMP_GT"; break;
            case TOK_MAIOR_IGUAL:  instr = "COMP_GE"; break;
            default:               instr = "COMP_UNKNOWN"; break;
        }
        // Gera instrução de comparação
        code.push_back(instr + " " + left + " " + right + " " + temp);
        return temp;
    }

    // Se não houver operador relacional, retorna a expressão tal como está (pode ser booleana pura)
    return left;
}

// parseConditionToIR(): parseia condições lógicas contendo AND/OR
std::string Parser::parseConditionToIR() {
    // Lê a primeira "comparação"
    std::string left = parseComparisonToIR();

    // Enquanto houver AND/OR, consome e gera IR de operação lógica
    while (check(TOK_AND) || check(TOK_OR)) {
        Token op = current_token;
        advance();   // consome AND/OR
        // Lê outra comparação à direita
        std::string right = parseComparisonToIR();

        std::string temp = newTemp();
        // Podemos chamar as instruções de "LOGICAL_AND"/"LOGICAL_OR" ou algo parecido
        if (op.id == TOK_AND) {
            code.push_back("LOGICAL_AND " + left + " " + right + " " + temp);
        } else {
            code.push_back("LOGICAL_OR " + left + " " + right + " " + temp);
        }
        left = temp;
    }
    return left;
}

// parseRelationalOperator(): consome um token se ele for <, <=, >, >=, = ou <>
void Parser::parseRelationalOperator() {
    if (check(TOK_IGUAL) || check(TOK_DIFERENTE) || check(TOK_MENOR)
        || check(TOK_MENOR_IGUAL) || check(TOK_MAIOR) || check(TOK_MAIOR_IGUAL))
    {
        advance();
    } else {
        parser_error(current_token, "Operador relacional esperado");
    }
}

// parseCondition(): exemplo de regra que não é muito usada (pode ser substituída pela parseConditionToIR)
void Parser::parseCondition() {
    parseExpression();
    parseRelationalOperator();
    parseExpression();
}

// parseExpression(): versão simplificada usada em parseCondition
void Parser::parseExpression() {
    parseTerm();
    while (check(TOK_PLUS) || check(TOK_MINUS)) {
        advance();
        parseTerm();
    }
}

// parseTerm(): idem
void Parser::parseTerm() {
    parseFactor();
    while (check(TOK_MULTIPLY) || check(TOK_DIVIDE) || check(TOK_MOD)) {
        advance();
        parseFactor();
    }
}

// parseFactor(): idem
void Parser::parseFactor() {
    if (check(TOK_LPAREN)) {
        advance();
        parseExpression();
        expect(TOK_RPAREN);
    } else if (check(TOK_IDENTIFIER)) {
        advance();
        while (check(TOK_LBRACKET)) {
            advance();
            parseExpression();
            expect(TOK_RBRACKET);
        }
    } else if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL)
               || check(TOK_STRING_LITERAL) || check(TOK_TRUE) || check(TOK_FALSE)) {
        advance();
    } else {
        parser_error(current_token, "Fator inválido");
    }
}

// ==========================================================
// 9) Estrutura principal do programa: PROGRAM / END_PROGRAM
// ==========================================================

void Parser::parseProgram() {
    expect(TOK_PROGRAM);       // Exige a palavra-chave PROGRAM
    expect(TOK_IDENTIFIER);    // Exige nome do programa
    // Lê blocos de variáveis (var ... end_var) se houver
    while (isVarBlockStart()) {
        parseVarBlock();
    }
    // Lê instruções do corpo do programa
    parseInstructions();
    expect(TOK_END_PROGRAM);   // Exige a palavra-chave END_PROGRAM
}

void Parser::parseVarBlock() {
    expect(TOK_VAR);           // "VAR"
    while (isVarDeclarationStart()) {
        parseVarDeclaration();
    }
    expect(TOK_END_VAR);       // "END_VAR"
}

// parse(): ponto de entrada do parser
void Parser::parse() {
    parseProgram();
    // Se ainda houver tokens após END_PROGRAM, erro
    if (current_token.id != TOK_EOF) {
        parser_error(current_token, "Tokens extras após fim do programa");
    }

    // Se chegou aqui, compilou sem erros sintáticos
    std::cout << "=== CODE GENERATION (IR) ===\n";
    for (const auto &instr : code) {
        std::cout << instr << "\n";
    }
}

// ===============================
// 10) Função principal (main)
// ===============================
int main(int argc, char **argv) {
    // Pode receber o nome do arquivo via parâmetro ou usar caminho fixo
    std::string nomeArquivo = (argc > 1) ? argv[1]
                                         : "/home/kaynan/Documentos/desenvolvimento/c++/Compilador-st-am/teste-1.st";

    // Abre o arquivo
    std::ifstream file(nomeArquivo);
    if (!file) {
        std::cerr << "Não foi possível abrir o arquivo: " << nomeArquivo << "\n";
        return 1;
    }

    // Lê todo o conteúdo do arquivo para uma string
    std::string code((std::istreambuf_iterator<char>(file)),
                     std::istreambuf_iterator<char>());

    // 1) Primeiro, usamos o lexer para imprimir todos os tokens (debug)
    Lexer lexer(code);
    {
        Token token = lexer.next_token();
        while (token.id != TOK_EOF) {
            std::cout << token.to_string() << "\n";
            token = lexer.next_token();
        }
    }

    // 2) Reinicializa o lexer para passar ao parser (pois já consumimos todos os tokens acima)
    Lexer lexer2(code);
    Parser parser(lexer2);

    // 3) Executa o parser
    try {
        parser.parse();
        std::cout << "Parsing, análise semântica e geração de código concluídos com sucesso!\n";
    } catch (const std::exception &e) {
        std::cerr << "Falha: " << e.what() << "\n";
        return 1;
    }

    return 0;
}

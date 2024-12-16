#include <iostream>
#include <string>
#include <cctype>
#include <memory>
#include <unordered_map>
#include <vector>
#include <stdexcept>
#include <fstream>

/*
 * Preciso adicionar as funcionalidades
 *   if x > 0 and y < 10 then
 *   e matrizes
 *   ainda nao esta funcionando
 */

// Tipos básicos
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

// Tablela de símbolos && lista de Tokens
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

    // Palavras reservadas
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
    TOK_IDENTIFIER,

    // Outros
    TOK_COMMENT,
    TOK_EOF,
    TOK_INVALID,
};

// Token
class Token {
public:
    // Atributos
    TokenId id;
    std::string value;
    int line;
    int column;

    // Construtor
    Token() : id(TOK_INVALID), value(""), line(0), column(0) {}
    Token(TokenId id, std::string value, int line, int column)
        : id(id), value(value), line(line), column(column) {}

    std::string to_string() const {
        return "Token(" + value + ", line: " + std::to_string(line) +
               ", col: " + std::to_string(column) + ")";
    }
};

class Lexer {
private:
    // Atributos
    std::string input; // Entrada
    size_t pos = 0; // Posicao atual
    int current_line = 1; // Linha
    int current_column = 1; // Coluna atual

    // verifica se chegou ao fim do arquivo
    char peek() const {
        // Se pos for maior que o tamanho da string, retorna '\0'
        return pos < input.size() ? input[pos] : '\0';
    }
    // Avança para o próximo caractere
    char advance() {
        // Avança a posição e retorna o caractere atual
        char current = peek();
        pos++;
        if (current == '\n') {
            // Se o caractere atual for uma quebra de linha, incrementa a linha e reseta a coluna
            current_line++;
            current_column = 1;
        } else {
            // Senão, incrementa a coluna
            current_column++;
        }
        // Retorna o caractere atual
        return current;
    }

    // Pula espaços em branco
    void skip_whitespace() {
        // Enquanto o caractere atual for um espaço em branco, avança
        while (isspace((unsigned char)peek())) advance();
    }

    // Pula comentários
    void skip_comment() {
        advance(); // '('
        advance(); // '*'
        // Enquanto não encontrar '*)', avança
        while (peek() != '*' || pos+1 >= input.size() || input[pos+1] != ')') {
            // Se encontrar uma quebra de linha, incrementa a linha
            if (peek() == '\0') {
                // Se chegar ao fim do arquivo sem encontrar '*)', exibe um erro
                std::cerr << "Unterminated comment on line " << current_line << "\n";
                return;
            }
            advance();
        }
        advance(); // '*'
        advance(); // ')'
    }

    // Parseia identificadores
    std::string parse_identifier() {
        // Cria uma string vazia
        std::string ident;
        // Identificadores podem conter letras, dígitos e '_'
        while (isalnum((unsigned char)peek()) || peek() == '_') {
            // Adiciona o caractere atual ao identificador e avança
            ident += advance();
        }
        // Retorna o identificador
        return ident;
    }

    // Classifica palavras-chave
    TokenId classify_keyword(const std::string& ident) {
        // Mapa de palavras-chave
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
        // Procura a palavra-chave no mapa
        auto it = keywords.find(ident);
        // Retorna o token correspondente, ou TOK_IDENTIFIER se não for uma palavra-chave
        return it != keywords.end() ? it->second : TOK_IDENTIFIER;
    }
    // Parseia números
    Token parse_number() {
        // Cria uma string vazia
        std::string num;
        // Flag para verificar se é um número real
        bool is_real = false;
        // Enquanto o caractere atual for um dígito ou '.', adiciona ao número
        while (isdigit((unsigned char)peek()) || peek() == '.') {
            // Se o caractere atual for '.', verifica se o próximo caractere é '.' e sai do loop
            if (peek() == '.') {
                // Se o próximo caractere for '.', sai do loop
                if (pos+1 < input.size() && input[pos+1] == '.') {
                    break;
                }
                if (is_real) break;
                is_real = true;
            }
            // Adiciona o caractere atual ao número e avança
            num += advance();
        }
        // Retorna o token correspondente
        return is_real ? Token(TOK_REAL_LITERAL, num, current_line, current_column)
                       : Token(TOK_INT_LITERAL, num, current_line, current_column);
    }

    // Parseia strings
    Token parse_string() {
        advance(); // skip "
        // Cria uma string vazia
        std::string str;
        // Enquanto o caractere atual não for '"' nem '\0', adiciona ao string
        while (peek() != '"' && peek() != '\0') {
            // Adiciona o caractere atual ao string e avança
            str += advance();
        }
        // Se o caractere atual for '"', avança
        if (peek() == '"') {
            advance();
            // Retorna o token correspondente
            return Token(TOK_STRING_LITERAL, str, current_line, current_column);
        } else {
            std::cerr << "Unterminated string on line " << current_line << "\n";
            return Token(TOK_INVALID, str, current_line, current_column);
        }
    }

    // Parseia '..' ou '.'
    Token parse_range_or_dot() {
        advance(); // '.'
        // Se o próximo caractere for '.', avança e retorna '..'
        if (peek() == '.') {
            advance();
            return Token(TOK_DOT_DOT, "..", current_line, current_column);
        } else {
            return Token(TOK_INVALID, ".", current_line, current_column);
        }
    }

// Métodos públicos
public:
    // Construtor
    Lexer(const std::string& input) : input(input) {}

    // Próximo token
    Token next_token() {
        // Pula espaços em branco
        skip_whitespace();

        // Se chegou ao fim do arquivo, retorna EOF (fim de arquivo)
        if (peek() == '\0') return Token(TOK_EOF, "EOF", current_line, current_column);

        // Comentário
        if (peek() == '(' && pos+1 < input.size() && input[pos+1] == '*') {
            skip_comment();
            return next_token();
        }

        // Identificador
        if (isalpha((unsigned char)peek()) || peek() == '_') {
            std::string ident = parse_identifier(); // Parseia o identificador
            TokenId id = classify_keyword(ident); // Classifica a palavra-chave
            // Retorna o token correspondente
            return Token(id, ident, current_line, current_column);
        }

        // Número
        if (isdigit((unsigned char)peek())) {
            return parse_number();
        }

        // Strings
        if (peek() == '"') {
            return parse_string();
        }

        // Operadores e pontuação
        switch (peek()) {
            case '+': advance(); return Token(TOK_PLUS, "+", current_line, current_column);
            case '-': advance(); return Token(TOK_MINUS, "-", current_line, current_column);
            case '*': advance(); return Token(TOK_MULTIPLY, "*", current_line, current_column);
            case '/': advance(); return Token(TOK_DIVIDE, "/", current_line, current_column);
            case '[': advance(); return Token(TOK_LBRACKET, "[", current_line, current_column);
            case ']': advance(); return Token(TOK_RBRACKET, "]", current_line, current_column);
            case '.':
                return parse_range_or_dot();
            case ':':
                advance();
                if (peek() == '=') {
                    advance();
                    return Token(TOK_ATRIBUICAO, ":=", current_line, current_column);
                } else {
                    return Token(TOK_COLON, ":", current_line, current_column);
                }
            case ';': advance(); return Token(TOK_SEMICOLON, ";", current_line, current_column);
            case ',': advance(); return Token(TOK_COMMA, ",", current_line, current_column);
            case '(':
                advance();
                return Token(TOK_LPAREN, "(", current_line, current_column);
            case ')':
                advance();
                return Token(TOK_RPAREN, ")", current_line, current_column);
            case '>':
                advance();
                if (peek() == '=') {
                    advance();
                    return Token(TOK_MAIOR_IGUAL, ">=", current_line, current_column);
                } else {
                    return Token(TOK_MAIOR, ">", current_line, current_column);
                }
            case '<':
                advance();
                if (peek() == '=') {
                    advance();
                    return Token(TOK_MENOR_IGUAL, "<=", current_line, current_column);
                } else if (peek() == '>') {
                    advance();
                    return Token(TOK_DIFERENTE, "<>", current_line, current_column);
                } else {
                    return Token(TOK_MENOR, "<", current_line, current_column);
                }
            // Operadores lógicos
            default: {
                std::string invalid(1, advance());
                return Token(TOK_INVALID, invalid, current_line, current_column);
            }
        }
    }
    // Retorna a posição atual
    size_t getPos() const { return pos; }
    // Retorna a entrada
    const std::string &getInput() const { return input; }
};

// Tabela de símbolos
class SymbolTable {
private:
    // Mapa de variáveis
    std::unordered_map<std::string, VarType> table;
public:
    // Adiciona uma variável à tabela
    void declareVar(const std::string &name, VarType type) {
        // Adiciona a variável ao mapa
        table[name] = type;
    }

    // Verifica se uma variável foi declarada
    bool isDeclared(const std::string &name) const {
        // Verifica se a variável está no mapa
        return table.find(name) != table.end();
    }

    // Retorna o tipo de uma variável
    VarType getType(const std::string &name) const {
        // Procura a variável no mapa
        auto it = table.find(name);
        // Se não encontrar, retorna TYPE_UNKNOWN
        if (it == table.end()) return TYPE_UNKNOWN;
        return it->second;
    }
};

// Parser
class Parser {
private:
    // Atributos
    Lexer &lexer; // Lexer
    Token current_token; // Token atual
    SymbolTable symtab; // Tabela de símbolos
    std::vector<std::string> code; // IR

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
    std::string parseExpressionToIR();
    std::string parseTermToIR();
    std::string parseFactorToIR();
    void parseIfStatement();
    std::string parseConditionToIR();
    void parseRelationalOperator();
    void parseCondition();
    void parseExpression();
    void parseTerm();
    void parseFactor();
    std::vector<std::pair<std::string,int>> parseArrayInitializer();
    std::pair<std::string,int> parseInitValue();

    void expect(TokenId expected);
    bool check(TokenId id);
    void advance();
    void parser_error(const Token &tok, const std::string &msg);
    void semantic_error(const Token &tok, const std::string &msg);
    // Geração de código
    std::string newTemp() {
        static int temp_count = 0;
        // Retorna um novo temporário
        return "t" + std::to_string(++temp_count);
    }

public:
    // Construtor
    Parser(Lexer &l) : lexer(l) {
        // Inicializa o token atual
        current_token = lexer.next_token();
    }

    // Parseia o programa
    void parse();
};

// Avança para o próximo token
void Parser::advance() {
    // Avança para o próximo token
    current_token = lexer.next_token();
}

// Exibe um erro de sintaxe
void Parser::parser_error(const Token &tok, const std::string &msg) {
    // Exibe a mensagem de erro
    std::cerr << "Erro de sintaxe na linha " << tok.line << ", coluna " << tok.column
              << ": " << msg << " (encontrado: " << tok.value << ")\n";

    // Exibe a linha onde ocorreu o erro
    const std::string &input = lexer.getInput();
    size_t pos = lexer.getPos(); // Posição atual

    // Encontra o início da linha
    size_t line_start = input.rfind('\n', pos);

    // Se não encontrar, começa do início
    if (line_start == std::string::npos)
        line_start = 0;
    else // Senão, avança para o próximo caractere
        line_start = line_start + 1;

    // Encontra o fim da linha
    size_t line_end = input.find('\n', pos);

    // Se não encontrar, vai até o final da string
    if (line_end == std::string::npos)
        line_end = input.size();

    // pega a linha onde ocorreu o erro e exibe
    std::string line_str = input.substr(line_start, line_end - line_start);
    std::cerr << line_str << "\n"; // Exibe a linha

    // Exibe a seta apontando para o erro
    for (int i = 1; i < tok.column; i++) std::cerr << " ";
    std::cerr << "^\n";

    throw std::runtime_error("Erro de sintaxe.");
}

// Exibe um erro semântico
void Parser::semantic_error(const Token &tok, const std::string &msg) {
    std::cerr << "Erro semântico na linha " << tok.line << ", coluna " << tok.column
              << ": " << msg << " (encontrado: " << tok.value << ")\n";
    throw std::runtime_error("Erro semântico.");
}

// Verifica se o token atual é igual ao esperado
void Parser::expect(TokenId expected) {
    // Se o token atual for igual ao esperado, avança
    if (current_token.id == expected) {
        advance();
    } else {
        // Senão, exibe um erro
        parser_error(current_token, "Token esperado não encontrado");
    }
}

// Verifica se o token atual é igual ao esperado
bool Parser::check(TokenId id) {
    return current_token.id == id; // Retorna verdadeiro se o token atual for igual ao esperado
}

// Mapeia tipos básicos
VarType Parser::mapBasicType(TokenId id) {
    switch (id) {
        case TOK_INT: return TYPE_INT;
        case TOK_REAL: return TYPE_REAL;
        case TOK_BOOL: return TYPE_BOOL;
        case TOK_STRING: return TYPE_STRING;
        default: return TYPE_UNKNOWN;
    }
}

// Parseia e retorna o tipo de uma variável
VarType Parser::parseTypeAndGetVarType() {
    // Verifica o tipo do token atual
    if (check(TOK_BOOL) || check(TOK_INT) || check(TOK_REAL) || check(TOK_STRING)) {
        // Se for um tipo básico, mapeia e avança
        VarType t = mapBasicType(current_token.id);
        advance();
        return t;
    } else if (check(TOK_ARRAY)) {
        advance();
        // Verifica se o próximo token é '['
        expect(TOK_LBRACKET);
        parseNumber();
        expect(TOK_DOT_DOT);
        parseNumber();
        // Verifica se o próximo token é ']'
        expect(TOK_RBRACKET);
        // Verifica se o próximo token é 'OF'
        expect(TOK_OF);
        VarType elemType = parseTypeAndGetVarType();
        if (elemType == TYPE_INT) return TYPE_ARRAY_INT;
        if (elemType == TYPE_REAL) return TYPE_ARRAY_REAL;
        if (elemType == TYPE_BOOL) return TYPE_ARRAY_BOOL;
        if (elemType == TYPE_STRING) return TYPE_ARRAY_STRING;
        return TYPE_UNKNOWN;
        // return elemType == TYPE_INT ? TYPE_ARRAY_INT : TYPE_ARRAY_REAL;
    } else if (check(TOK_STRUCT)) {
        advance();
        // Verifica se o próximo token é um identificador
        while (check(TOK_IDENTIFIER)) {
            // Declaração de struct
            parseStructDeclaration();
        }
        // Verifica se o próximo token é 'END_STRUCT'
        expect(TOK_END_STRUCT);
        return TYPE_UNKNOWN;
    } else {
        parser_error(current_token, "Tipo inválido");
        return TYPE_UNKNOWN;
    }
}

// Parseia e retorna o tipo de uma variável
void Parser::parseStructDeclaration() {
    expect(TOK_IDENTIFIER); // consome o identificador
    expect(TOK_COLON); // consome ':'
    parseTypeAndGetVarType(); // consome o tipo
    expect(TOK_SEMICOLON); // consome ';'
}

// Parseia e retorna o tipo de uma variável
void Parser::parseValue() {
    // parseValue é apenas para valores escalares
    if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL) || check(TOK_STRING_LITERAL) || check(TOK_TRUE) || check(TOK_FALSE)) {
        advance(); // Apenas consome por agora
    } else {
        parser_error(current_token, "Valor inválido na atribuição");
    }
}

// Parseia e retorna o tipo de uma variável
void Parser::parseNumber() {
    if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL)) {
        advance();
    } else {
        parser_error(current_token, "Número esperado");
    }
}

// Verifica se é o início de um bloco de variáveis
bool Parser::isVarBlockStart() {
    return check(TOK_VAR); // Verifica se o token atual é 'VAR'
}

// Verifica se é o início de uma declaração de variável
bool Parser::isVarDeclarationStart() {
    return check(TOK_IDENTIFIER); // Verifica se o token atual é um identificador
}

// Parseia uma declaração de variável
void Parser::parseVarDeclaration() {
    Token varName = current_token; // Salva o identificador
    expect(TOK_IDENTIFIER); // Consome o identificador
    expect(TOK_COLON); // Consome ':'
    VarType varType = parseTypeAndGetVarType(); // Parseia e retorna o tipo da variável

    bool initialized = false;
    // Verifica se o próximo token é ':=' (inicialização)
    if (check(TOK_ATRIBUICAO)) {
        advance(); // consome :=
        if (check(TOK_LBRACKET)) {
            // inicialização de array (array initializer)
            if (varType != TYPE_ARRAY_INT && varType != TYPE_ARRAY_REAL && varType != TYPE_ARRAY_BOOL && varType != TYPE_ARRAY_STRING) {
                semantic_error(current_token, "Inicialização de array em variável não-array");
            }

            // parsear array initializer e gerar IR para inicialização de array
            auto init_values = parseArrayInitializer();
            // gerar IR para inicialização
            int index = 1;
            // para cada valor no array initializer
            for (auto &val_rep : init_values) {// val_rep = {valor, repetições}
                // val_rep.first = valor, val_rep.second = repetições
                // gerar IR para armazenar o valor no array
                // STORE_INDEX val varName index
                std::string val = val_rep.first;
                int rep = val_rep.second; // número de repetições do valor no array
                // para cada repetição do valor no array
                for (int i = 0; i < rep; i++) {
                    code.push_back("STORE_INDEX " + val + " " + varName.value + " " + std::to_string(index)); // STORE_INDEX val varName index
                    index++;
                }
            }
            initialized = true;
        // se não for array initializer
        } else {
            // inicialização escalar
            // Precisamos gerar IR para inicialização de variável escalar
            Token lit = current_token;
            // Verifica se o token atual é um valor válido para a atribuição
            if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL) || check(TOK_STRING_LITERAL) || check(TOK_TRUE) || check(TOK_FALSE)) {
                advance();
                std::string temp = newTemp();
                // Gerar IR para inicialização de variável escalar
                if (lit.id == TOK_INT_LITERAL || lit.id == TOK_REAL_LITERAL) {
                    code.push_back("LOAD_IMM " + lit.value + " " + temp); // LOAD_IMM value temp
                // se for string literal
                } else if (lit.id == TOK_STRING_LITERAL) {
                    code.push_back("LOAD_IMM \"" + lit.value + "\" " + temp); // LOAD_IMM "value" temp
                // se for true ou false
                } else if (lit.id == TOK_TRUE) {
                    code.push_back("LOAD_IMM 1 " + temp);
                } else if (lit.id == TOK_FALSE) {
                    code.push_back("LOAD_IMM 0 " + temp);
                }
                // STORE temp varName
                code.push_back("STORE " + temp + " " + varName.value);
                initialized = true;
                // parseValue(); // consome o valor
            } else {
                parser_error(current_token, "Valor inválido na atribuição");
            }
        }
    }

    // Verifica se o próximo token é ';' (fim da declaração)
    expect(TOK_SEMICOLON);
    symtab.declareVar(varName.value, varType); // Adiciona a variável à tabela de símbolos
}

// Parseia um bloco de variáveis (VAR ... END_VAR)
std::vector<std::pair<std::string,int>> Parser::parseArrayInitializer() {
    std::vector<std::pair<std::string,int>> values; // array initializer
    expect(TOK_LBRACKET); // consome '['
    // se não for ']'
    if (!check(TOK_RBRACKET)) {
        // parsear valores iniciais do array
        do {
            values.push_back(parseInitValue()); // parsear valor inicial e adicionar ao array initializer
        } while (check(TOK_COMMA) && (advance(), true)); // enquanto encontrar ','
    }
    expect(TOK_RBRACKET); // consome ']'
    return values;
}

// Parseia um bloco de variáveis (VAR ... END_VAR)
std::pair<std::string,int> Parser::parseInitValue() {
    // init_value ::= <expression> [ '(' <number> ')' ]
    // parsear expressão
    std::string val = parseExpressionToIR();
    int rep = 1;
    // se encontrar '('
    if (check(TOK_LPAREN)) {
        advance(); // '('
        // parsear número e armazenar em rep
        if (!check(TOK_INT_LITERAL)) {
            parser_error(current_token, "Número esperado após '('");
        }
        int count = std::stoi(current_token.value); // número de repetições
        advance(); // consome numero
        expect(TOK_RPAREN);
        rep = count; // armazena o número de repetições
    }
    return {val, rep}; // retorna o valor e o número de repetições
}

// Parseia um bloco de variáveis (VAR ... END_VAR)
bool Parser::isInstructionStart() {
    return check(TOK_IDENTIFIER) || check(TOK_IF); // Verifica se o token atual é um identificador
}

// Parseia um bloco de variáveis (VAR ... END_VAR)
void Parser::parseInstructions() {
    // Enquanto for o início de uma instrução, parseia a instrução
    while (isInstructionStart()) {
        parseInstruction(); // Parseia a instrução atual
    }
}

// Parseia um bloco de variáveis (VAR ... END_VAR)
void Parser::parseInstruction() {
    if (check(TOK_IDENTIFIER)) { // Verifica se o token atual é um identificador
        parseAssignment();
    } else if (check(TOK_IF)) { // Verifica se o token atual é 'IF'
        parseIfStatement();
    } else {
        parser_error(current_token, "Instrução inválida");
    }
}

// Parseia um bloco de variáveis (VAR ... END_VAR)
void Parser::parseAssignment() {
    Token varName = current_token; // Salva o identificador
    expect(TOK_IDENTIFIER); // Consome o identificador
    if (!symtab.isDeclared(varName.value)) { // Verifica se a variável foi declarada
        semantic_error(varName, "Variável não declarada");
    }

    VarType varType = symtab.getType(varName.value); // Retorna o tipo da variável atual (identificador)

    std::vector<std::string> indices;
    // Verifica se o próximo token é '[' (array access) e parseia os índices do array (se houver)
    // e atualiza o tipo da variável (se for um array) e armazena os índices em 'indices' (se houver)
    while (check(TOK_LBRACKET)) {
        if (varType != TYPE_ARRAY_INT && varType != TYPE_ARRAY_REAL && varType != TYPE_ARRAY_BOOL && varType != TYPE_ARRAY_STRING) {
            semantic_error(current_token, "Acesso a array em variável não-array"); // Erro semântico (acesso a array em variável não-array)
        }

        advance(); // '[' (consome '[')
        std::string idx = parseExpressionToIR(); // Parseia a expressão e retorna o IR
        expect(TOK_RBRACKET);

        // Atualiza o tipo da variável (se for um array) e armazena os índices em 'indices' (se houver)
        if (varType == TYPE_ARRAY_INT) varType = TYPE_INT;
        else if (varType == TYPE_ARRAY_REAL) varType = TYPE_REAL;
        else if (varType == TYPE_ARRAY_BOOL) varType = TYPE_BOOL;
        else if (varType == TYPE_ARRAY_STRING) varType = TYPE_STRING;

        indices.push_back(idx);
    }

    expect(TOK_ATRIBUICAO);
    std::string rhs = parseExpressionToIR();
    expect(TOK_SEMICOLON);

    if (indices.empty()) {
        code.push_back("STORE " + rhs + " " + varName.value);
    } else {
        code.push_back("STORE_INDEX " + rhs + " " + varName.value + " " + indices[0]);
    }
}

std::string Parser::parseExpressionToIR() {
    std::string left = parseTermToIR();
    while (check(TOK_PLUS) || check(TOK_MINUS)) {
        Token op = current_token;
        advance();
        std::string right = parseTermToIR();
        std::string temp = newTemp();
        code.push_back((op.id == TOK_PLUS ? "ADD" : "SUB") + std::string(" ") + left + " " + right + " " + temp);
        left = temp;
    }
    return left;
}

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

std::string Parser::parseFactorToIR() {
    if (check(TOK_LPAREN)) {
        advance();
        std::string res = parseExpressionToIR();
        expect(TOK_RPAREN);
        return res;
    } else if (check(TOK_IDENTIFIER)) {
        Token varName = current_token;
        advance();
        if (!symtab.isDeclared(varName.value)) {
            semantic_error(varName, "Uso de variável não declarada");
        }

        VarType varType = symtab.getType(varName.value);
        std::vector<std::string> indices;
        while (check(TOK_LBRACKET)) {
            if (varType != TYPE_ARRAY_INT && varType != TYPE_ARRAY_REAL && varType != TYPE_ARRAY_BOOL && varType != TYPE_ARRAY_STRING) {
                semantic_error(current_token, "Acesso a array em variável não-array");
            }
            advance(); // '['
            std::string idx = parseExpressionToIR();
            expect(TOK_RBRACKET);

            if (varType == TYPE_ARRAY_INT) varType = TYPE_INT;
            else if (varType == TYPE_ARRAY_REAL) varType = TYPE_REAL;
            else if (varType == TYPE_ARRAY_BOOL) varType = TYPE_BOOL;
            else if (varType == TYPE_ARRAY_STRING) varType = TYPE_STRING;

            indices.push_back(idx);
        }

        std::string temp = newTemp();
        if (indices.empty()) {
            code.push_back("LOAD " + varName.value + " " + temp);
        } else {
            code.push_back("LOAD_INDEX " + varName.value + " " + indices[0] + " " + temp);
        }
        return temp;
    } else if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL)) {
        Token lit = current_token;
        advance();
        std::string temp = newTemp();
        code.push_back("LOAD_IMM " + lit.value + " " + temp);
        return temp;
    } else if (check(TOK_STRING_LITERAL)) {
        Token lit = current_token;
        advance();
        std::string temp = newTemp();
        code.push_back("LOAD_IMM \"" + lit.value + "\" " + temp);
        return temp;
    } else if (check(TOK_TRUE)) {
        Token lit = current_token;
        advance();
        std::string temp = newTemp();
        code.push_back("LOAD_IMM 1 " + temp);
        return temp;
    } else if (check(TOK_FALSE)) {
        Token lit = current_token;
        advance();
        std::string temp = newTemp();
        code.push_back("LOAD_IMM 0 " + temp);
        return temp;
    } else {
        parser_error(current_token, "Fator inválido");
        return "";
    }
}

void Parser::parseIfStatement() {
    expect(TOK_IF);
    std::string cond = parseConditionToIR();
    expect(TOK_THEN);
    parseInstructions();

    if (check(TOK_ELSE)) {
        advance();
        parseInstructions();
    }

    expect(TOK_END_IF);
}

std::string Parser::parseConditionToIR() {
    // Primeiro parse uma expressão
    std::string left = parseExpressionToIR();

    // Verifica se o próximo token é um operador relacional
    if (check(TOK_IGUAL) || check(TOK_DIFERENTE) || check(TOK_MENOR) || check(TOK_MENOR_IGUAL) || check(TOK_MAIOR) || check(TOK_MAIOR_IGUAL)) {
        // Tem operador relacional, então parseia normalmente
        Token op = current_token;
        parseRelationalOperator();
        std::string right = parseExpressionToIR();

        std::string temp = newTemp();
        std::string instr;
        switch (op.id) {
            case TOK_IGUAL: instr = "COMP_EQ"; break;
            case TOK_DIFERENTE: instr = "COMP_NE"; break;
            case TOK_MENOR: instr = "COMP_LT"; break;
            case TOK_MENOR_IGUAL: instr = "COMP_LE"; break;
            case TOK_MAIOR: instr = "COMP_GT"; break;
            case TOK_MAIOR_IGUAL: instr = "COMP_GE"; break;
            default: instr = "COMP_UNKNOWN"; break;
        }
        code.push_back(instr + " " + left + " " + right + " " + temp);
        return temp;
    } else {
        // Não tem operador relacional, a condição é apenas 'left'
        // Aqui assumimos que 'left' é booleano, idealmente deveria checar se o tipo é booleano.
        // Como não temos verificação completa de tipo, assumiremos ser válido.
        return left;
    }
}

void Parser::parseRelationalOperator() {
    if (check(TOK_IGUAL) || check(TOK_DIFERENTE) || check(TOK_MENOR) || check(TOK_MENOR_IGUAL) || check(TOK_MAIOR) || check(TOK_MAIOR_IGUAL)) {
        advance();
    } else {
        parser_error(current_token, "Operador relacional esperado");
    }
}

void Parser::parseCondition() {
    parseExpression();
    parseRelationalOperator();
    parseExpression();
}

void Parser::parseExpression() {
    parseTerm();
    while (check(TOK_PLUS) || check(TOK_MINUS)) {
        advance();
        parseTerm();
    }
}

void Parser::parseTerm() {
    parseFactor();
    while (check(TOK_MULTIPLY) || check(TOK_DIVIDE) || check(TOK_MOD)) {
        advance();
        parseFactor();
    }
}

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
    } else if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL) || check(TOK_STRING_LITERAL) || check(TOK_TRUE) || check(TOK_FALSE)) {
        advance();
    } else {
        parser_error(current_token, "Fator inválido");
    }
}

void Parser::parseProgram() {
    expect(TOK_PROGRAM);
    expect(TOK_IDENTIFIER);
    while (isVarBlockStart()) {
        parseVarBlock();
    }
    parseInstructions();
    expect(TOK_END_PROGRAM);
}

void Parser::parseVarBlock() {
    expect(TOK_VAR);
    while (isVarDeclarationStart()) {
        parseVarDeclaration();
    }
    expect(TOK_END_VAR);
}

void Parser::parse() {
    parseProgram();
    if (current_token.id != TOK_EOF) {
        parser_error(current_token, "Tokens extras após fim do programa");
    }

    std::cout << "=== CODE GENERATION (IR) ===\n";
    for (auto &instr : code) {
        std::cout << instr << "\n";
    }
}

int main(int argc, char **argv) {

    // if (argc < 2) {
    //     std::cerr << "Uso: " << argv[0] << " <arquivo fonte>\n";
    //     return 1;
    // }

    std::ifstream file(/*argv[1]*/ "teste.st");
    if (!file) {
        std::cerr << "Não foi possível abrir o arquivo: " << argv[1] << "\n";
        return 1;
    }

    std::string code((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());

    Lexer lexer(code);
    Parser parser(lexer);

    try {
        parser.parse();
        std::cout << "Parsing, análise semântica e geração de código concluídos com sucesso!\n";
    } catch (const std::exception &e) {
        std::cerr << "Falha: " << e.what() << "\n";
    }

    return 0;
}

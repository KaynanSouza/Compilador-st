#include <iostream>
#include <string>
#include <cctype>
#include <cmath>
#include <memory>
#include <unordered_map>
#include <vector>
#include <stdexcept>
#include <fstream>

/*
 * Observações e melhorias:
 * 1) Removidos muitos prints de debug que poluíam o código (ex: std::cout << "linha X").
 * 2) Ajustadas algumas condições de retorno prematuro que acabavam escondendo código.
 * 3) Melhorada a função parse_identifier para ser mais clara e robusta,
 *    validando se o primeiro caractere é válido e também retornando um erro
 *    se não houver nenhum caractere de identificador (pode ser adaptado conforme a regra de linguagem).
 * 4) Incluídos comentários adicionais para explicar o funcionamento de partes importantes.
 * 5) Ajustada a lógica de parse_comment para evitar possíveis leituras fora de índice.
 */

// Tipos básicos
enum VarType {
    TYPE_INT,
    TYPE_REAL,
    TYPE_STRING,
    TYPE_BOOL,
    // Arrays de básicos
    TYPE_ARRAY_INT,
    TYPE_ARRAY_REAL,
    TYPE_ARRAY_STRING,
    TYPE_ARRAY_BOOL,
    // Desconhecido (ou não suportado ainda)
    TYPE_UNKNOWN
};

// Tabela de símbolos && lista de Tokens
enum TokenId {
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
        TOK_HEX_LITERAL,
        TOK_OCTAL_LITERAL,
        TOK_BINARY_LITERAL,
        TOK_IDENTIFIER,

        // Outros
        TOK_COMMENT,
        TOK_EOF,
        TOK_INVALID,
        TOK_ERROR,

        // NOVAS PALAVRAS-CHAVE
        TOK_VAR_INPUT,
        TOK_END_VAR_INPUT,
        TOK_VAR_OUTPUT,
        TOK_END_VAR_OUTPUT,
        TOK_ELSIF,
        TOK_FOR,
        TOK_TO,
        TOK_BY,
        TOK_DO,
        TOK_END_FOR,
        TOK_WHILE,
        TOK_END_WHILE,
        TOK_REPEAT,
        TOK_UNTIL,
        TOK_END_REPEAT,
        TOK_CASE,
        TOK_END_CASE,

        // PARA SUPORTE A TIPOS IEC
        TOK_TYPE,
        TOK_END_TYPE,
};

// Função auxiliar para imprimir o nome de cada TokenId
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
        case TOK_VAR_INPUT: return "TOK_VAR_INPUT";
        case TOK_END_VAR_INPUT: return "TOK_END_VAR_INPUT";
        case TOK_VAR_OUTPUT: return "TOK_VAR_OUTPUT";
        case TOK_END_VAR_OUTPUT: return "TOK_END_VAR_OUTPUT";
        case TOK_ELSIF: return "TOK_ELSIF";
        case TOK_FOR: return "TOK_FOR";
        case TOK_TO: return "TOK_TO";
        case TOK_BY: return "TOK_BY";
        case TOK_DO: return "TOK_DO";
        case TOK_END_FOR: return "TOK_END_FOR";
        case TOK_WHILE: return "TOK_WHILE";
        case TOK_END_WHILE: return "TOK_END_WHILE";
        case TOK_REPEAT: return "TOK_REPEAT";
        case TOK_UNTIL: return "TOK_UNTIL";
        case TOK_END_REPEAT: return "TOK_END_REPEAT";
        case TOK_CASE: return "TOK_CASE";
        case TOK_END_CASE: return "TOK_END_CASE";
        case TOK_TYPE: return "TOK_TYPE";
        case TOK_END_TYPE: return "TOK_END_TYPE";
        default: return "UNKNOWN_TOKEN";
    }
}

// Classe que representa um token
class Token {
public:
    TokenId id;
    std::string value;
    int line;
    int column;

    Token() : id(TOK_INVALID), value(""), line(0), column(0) {}
    Token(TokenId id, std::string value, int line, int column)
        : id(id), value(std::move(value)), line(line), column(column) {}

    // Para debug
    std::string to_string() const  {
        return "Token(" + value + ", line: " + std::to_string(line) +
               ", col: " + std::to_string(column) +
               ", id: " + token_id_to_string(id) + ")";
    }
};

class Lexer {
private:
    std::string input;     // Todo o código-fonte
    size_t pos = 0;        // Posição atual no input
    int current_line = 1;
    int current_column = 1;

    // Retorna caractere atual (ou '\0' se fim)
    char peek() const {
        return (pos < input.size()) ? input[pos] : '\0';
    }

    // Avança um caractere e atualiza linha/coluna
    char advance() {
        char c = peek();
        if (c != '\0') {
            pos++;
            if (c == '\n') {
                current_line++;
                current_column = 1;
            }
            else if (c == '\t') {
                current_column += 4;
            }
            else {
                current_column++;
            }
        }
        return c;
    }

    // Ignora espaços em branco
    void skip_whitespace() {
        while (std::isspace((unsigned char)peek())) {
            advance();
        }
    }

    // Pula comentário do tipo (* ... *)
    void skip_comment() {
        // Já viu '(' e '*'
        advance(); // '('
        advance(); // '*'
        while (true) {
            char c = peek();
            if (c == '\0') {
                std::cerr << "Unterminated comment on line " << current_line << "\n";
                return;
            }
            // Se encontrar '*', e o próximo for ')', fecha
            if (c == '*' && (pos+1 < input.size()) && input[pos+1] == ')') {
                advance(); // consome '*'
                advance(); // consome ')'
                break;
            }
            advance();
        }
    }

    // Lê identificador: letra ou '_' + (letra, dígito, '_')*
    std::string parse_identifier() {
        std::string ident;
        char first = peek();
        if (!(std::isalpha((unsigned char)first) || first == '_')) {
            return "";
        }
        while (std::isalnum((unsigned char)peek()) || peek() == '_') {
            ident.push_back(advance());
        }
        return ident;
    }

    // classifica identificador -> keyword ou TOK_IDENTIFIER
    TokenId classify_keyword(const std::string &ident) {
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
            {"END_STRUCT", TOK_END_STRUCT}, {"VAR_INPUT", TOK_VAR_INPUT},
            {"END_VAR_INPUT", TOK_END_VAR_INPUT}, {"VAR_OUTPUT", TOK_VAR_OUTPUT},
            {"END_VAR_OUTPUT", TOK_END_VAR_OUTPUT}, {"ELSIF", TOK_ELSIF},
            {"FOR", TOK_FOR}, {"TO", TOK_TO},
            {"BY", TOK_BY}, {"DO", TOK_DO},
            {"END_FOR", TOK_END_FOR}, {"WHILE", TOK_WHILE},
            {"END_WHILE", TOK_END_WHILE}, {"REPEAT", TOK_REPEAT},
            {"UNTIL", TOK_UNTIL}, {"END_REPEAT", TOK_END_REPEAT},
            {"CASE", TOK_CASE}, {"END_CASE", TOK_END_CASE},
            {"TYPE", TOK_TYPE}, {"END_TYPE", TOK_END_TYPE},
        };
        auto it = keywords.find(ident);
        return (it != keywords.end()) ? it->second : TOK_IDENTIFIER;
    }

    // parse_number() que suporta sinais, base e real
    Token parse_number() {
        int start_line = current_line;
        int start_col  = current_column;

        // 1) Sinal opcional
        bool negative = false;
        bool positive = false;
        if (peek() == '-' || peek() == '+') {
            if (peek() == '-') negative = true;
            else positive = true;
            advance();
        }

        std::string num;
        bool is_real = false;
        bool is_hex = false;
        bool is_oct = false;
        bool is_bin = false;

        // Lê dígitos/base
        // A sintaxe de base aqui é <base>#<valor>, ex: "16#FF"
        // Se não quiser base, remova esse trecho
        while (std::isdigit((unsigned char)peek()) || peek() == '.' || peek() == '#') {
            if (peek() == '.') {
                // se já tinha ponto => paramos
                if (is_real) break;
                // se for '..', paramos tb
                if (pos+1 < input.size() && input[pos+1] == '.') {
                    break;
                }
                is_real = true;
                num.push_back(advance());
            }
            else if (peek() == '#') {
                // ex: "16#"
                int baseVal = 10;
                try {
                    baseVal = std::stoi(num);
                } catch(...) {
                    // se falhar stoi, baseVal = 10
                }
                advance(); // consome '#'
                num.clear();
                if (baseVal == 16) {
                    is_hex = true;
                } else if (baseVal == 8) {
                    is_oct = true;
                } else if (baseVal == 2) {
                    is_bin = true;
                } else {
                    std::string msg = "Base inválida: " + std::to_string(baseVal);
                    return Token(TOK_ERROR, msg, start_line, start_col);
                }
                // break do while -> iremos ler o valor na base
                break;
            }
            else {
                // dígito
                num.push_back(advance());
            }
        }

        // Se for base 16...
        if (is_hex) {
            // ler [0-9A-Fa-f]
            while (std::isxdigit((unsigned char)peek())) {
                num.push_back(advance());
            }
            if (negative) num.insert(num.begin(), '-');
            return Token(TOK_HEX_LITERAL, num, start_line, start_col);
        }
        if (is_oct) {
            // ler [0-7]
            while (peek() >= '0' && peek() <= '7') {
                num.push_back(advance());
            }
            if (negative) num.insert(num.begin(), '-');
            return Token(TOK_OCTAL_LITERAL, num, start_line, start_col);
        }
        if (is_bin) {
            // ler [0-1]
            while (peek() == '0' || peek() == '1') {
                num.push_back(advance());
            }
            if (negative) num.insert(num.begin(), '-');
            return Token(TOK_BINARY_LITERAL, num, start_line, start_col);
        }

        // Se não houve base especial
        bool read_any = !num.empty();
        if (!read_any) {
            // ex: só tinha '-' e nada de dígito
            std::string s = negative ? "-" : (positive ? "+" : "");
            return Token(TOK_INVALID, s, start_line, start_col);
        }

        // se tem is_real => real
        if (negative) num.insert(num.begin(), '-');
        if (is_real) {
            return Token(TOK_REAL_LITERAL, num, start_line, start_col);
        }
        else {
            return Token(TOK_INT_LITERAL, num, start_line, start_col);
        }
    }

    Token parse_string(char delimiter) {
        int start_line = current_line;
        int start_col  = current_column;
        advance(); // consome a aspa
        std::string str;
        while (true) {
            char c = peek();
            if (c == '\0') {
                std::cerr << "Unterminated string on line " << current_line << "\n";
                return Token(TOK_INVALID, str, start_line, start_col);
            }
            if (c == delimiter) {
                advance(); // fecha
                break;
            }
            str.push_back(advance());
        }
        return Token(TOK_STRING_LITERAL, str, start_line, start_col);
    }

    // parse '.' ou '..'
    Token parse_range_or_dot() {
        int start_line = current_line;
        int start_col  = current_column;
        advance(); // consome '.'
        if (peek() == '.') {
            advance();
            return Token(TOK_DOT_DOT, "..", start_line, start_col);
        }
        return Token(TOK_INVALID, ".", start_line, start_col);
    }

public:
    explicit Lexer(std::string code)
        : input(std::move(code)) {}

    Token next_token() {
        skip_whitespace();
        char c = peek();
        if (c == '\0') {
            return Token(TOK_EOF, "EOF", current_line, current_column);
        }

        // Comentário '(* ... *)'
        if (c == '(' && (pos+1 < input.size()) && input[pos+1] == '*') {
            skip_comment();
            return next_token();
        }

        // Se começa por letra ou '_', parse identifier
        if (std::isalpha((unsigned char)c) || c == '_') {
            int start_line = current_line;
            int start_col  = current_column;
            std::string ident = parse_identifier();
            if (ident.empty()) {
                // erro
                return Token(TOK_INVALID, std::string(1, advance()), start_line, start_col);
            }
            TokenId tid = classify_keyword(ident);
            return Token(tid, ident, start_line, start_col);
        }

        // Se é dígito ou sinal -> parse número
        if (std::isdigit((unsigned char)c) || c == '-' || c == '+') {
            return parse_number();
        }

        // Se é string
        if (c == '"' || c == '\'') {
            return parse_string(c);
        }

        // Se for '.' ou '..'
        if (c == '.') {
            return parse_range_or_dot();
        }

        // Demais símbolos
        // Pega info de linha/col p/ debug
        int start_line = current_line;
        int start_col  = current_column;

        switch(c) {
            case ':':
                advance();
                if (peek() == '=') {
                    advance();
                    return Token(TOK_ATRIBUICAO, ":=", start_line, start_col);
                }
                return Token(TOK_COLON, ":", start_line, start_col);
            case ';':
                advance();
                return Token(TOK_SEMICOLON, ";", start_line, start_col);
            case ',':
                advance();
                return Token(TOK_COMMA, ",", start_line, start_col);
            case '(':
            {
                advance();
                return Token(TOK_LPAREN, "(", start_line, start_col);
            }
            case ')':
            {
                advance();
                return Token(TOK_RPAREN, ")", start_line, start_col);
            }
            case '[':
            {
                advance();
                return Token(TOK_LBRACKET, "[", start_line, start_col);
            }
            case ']':
            {
                advance();
                return Token(TOK_RBRACKET, "]", start_line, start_col);
            }
            case '+':
            {
                advance();
                return Token(TOK_PLUS, "+", start_line, start_col);
            }
            case '-':
            {
                // Se quiser separar '-' sozinho, trate aqui.
                // Mas se usar parse_number() com sinal,
                // pode remover esse case e deixar parse_number() cuidar.
                advance();
                return Token(TOK_MINUS, "-", start_line, start_col);
            }
            case '*':
            {
                advance();
                return Token(TOK_MULTIPLY, "*", start_line, start_col);
            }
            case '/':
            {
                advance();
                return Token(TOK_DIVIDE, "/", start_line, start_col);
            }
            case '>':
            {
                advance();
                if (peek() == '=') {
                    advance();
                    return Token(TOK_MAIOR_IGUAL, ">=", start_line, start_col);
                }
                return Token(TOK_MAIOR, ">", start_line, start_col);
            }
            case '<':
            {
                advance();
                if (peek() == '=') {
                    advance();
                    return Token(TOK_MENOR_IGUAL, "<=", start_line, start_col);
                }
                else if (peek() == '>') {
                    advance();
                    return Token(TOK_DIFERENTE, "<>", start_line, start_col);
                }
                return Token(TOK_MENOR, "<", start_line, start_col);
            }
            default:
            {
                char inv = advance();
                return Token(TOK_INVALID, std::string(1, inv), start_line, start_col);
            }
        }
    }

    size_t getPos() const { return pos; }
    const std::string& getInput() const { return input; }
};

// Tabela de símbolos simples (nome -> tipo)
class SymbolTable {
private:
    std::unordered_map<std::string, VarType> table;
public:
    void declareVar(const std::string &name, VarType type) {
        table[name] = type;
    }

    bool isDeclared(const std::string &name) const {
        return (table.find(name) != table.end());
    }

    VarType getType(const std::string &name) const {
        auto it = table.find(name);
        if (it == table.end()) return TYPE_UNKNOWN;
        return it->second;
    }
};

// ----------------------------------------------------
// 4.1) Estrutura para armazenar infos de tipos
// ----------------------------------------------------
enum UserTypeKind {
    UTYPE_BASE,       // Tipo base (ex: REAL) com init
    UTYPE_ENUM,       // Enumerado
    UTYPE_SUBRANGE,   // Subfaixa
    UTYPE_ARRAY,      // Array
    UTYPE_STRUCT,     // Struct
    UTYPE_DERIVED     // Derivado de outro
};

// Para simplificar, apenas guardamos strings e sub-infos
struct UserTypeInfo {
    UserTypeKind kind;
    std::string name;      // ex: "FREQ"
    std::string baseName;  // se for derivado de outro tipo, ou base simples (INT,REAL...)
    double initValueNumeric; // se for base numeric
    std::string initValueString;
    bool hasInitValue = false;

    // enum
    std::vector<std::string> enumValues;
    std::string enumInit;

    // subrange
    long subrangeLow;
    long subrangeHigh;

    // array
    long arrayLow;
    long arrayHigh;
    std::string arrayElementType; // ex: "ANALOG_DATA"
    // possivel array initializer

    // struct
    // guardamos <fieldName, fieldType, initValueOpcional> ...
    struct Field {
        std::string name;
        std::string type;
        std::string initValue; // literal
    };
    std::vector<Field> structFields;

    // derived struct
    std::vector<std::pair<std::string, std::string>> derivedInits;
    // ex: (field1, "0"), (field2, "4000") ...
};

// ----------------------------------------------------
// 4.2) Tabela de tipos do usuário
// ----------------------------------------------------
class TypeTable {
public:
    std::unordered_map<std::string, UserTypeInfo> types;

    bool isTypeDeclared(const std::string &name) const {
        return (types.find(name) != types.end());
    }
    void declareType(const std::string &name, const UserTypeInfo &info) {
        types[name] = info;
    }
    const UserTypeInfo* getTypeInfo(const std::string &name) const {
        auto it = types.find(name);
        if (it != types.end()) return &it->second;
        return nullptr;
    }
};

// Parser
class Parser {
private:
    Lexer &lexer;
    Token current_token;
    SymbolTable symtab;  // Para variáveis
    TypeTable typetab;   // Para tipos de usuário
    std::vector<std::string> code;

    // Funções de parsing
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

    // Expressões
    std::string parseExpressionToIR();
    std::string parseTermToIR();
    std::string parseFactorToIR();

    // Condições
    void parseIfStatement();
    std::string parseComparisonToIR();
    std::string parseConditionToIR();
    void parseRelationalOperator();

    void parseCondition();
    void parseExpression();
    void parseTerm();
    void parseFactor();

    // Array initializer
    std::vector<std::pair<std::string,int>> parseArrayInitializer();
    std::pair<std::string,int> parseInitValue();

    // Suporte a TYPE ... END_TYPE
    void parseTypeBlock();
    void parseTypeDeclaration();

    // Auxiliares
    void expect(TokenId expected);
    bool check(TokenId id);
    void advance();
    void parser_error(const Token &tok, const std::string &msg);
    void semantic_error(const Token &tok, const std::string &msg);

    std::string newTemp() {
        static int temp_count = 0;
        return "t" + std::to_string(++temp_count);
    }

public:
    explicit Parser(Lexer &l) : lexer(l) {
        current_token = lexer.next_token();
    }
    void parse();
};

// Avança para o próximo token
void Parser::advance() {
    current_token = lexer.next_token();
}

// Emite erro de sintaxe
void Parser::parser_error(const Token &tok, const std::string &msg) {
    std::cerr << "Erro de sintaxe na linha " << tok.line << ", coluna " << tok.column
              << ": " << msg << " (encontrado: " << tok.value << ")\n";

    const std::string &input = lexer.getInput();
    size_t filePos = lexer.getPos();

    // Localiza início da linha
    size_t line_start = input.rfind('\n', filePos);
    if (line_start == std::string::npos) {
        line_start = 0;
    } else {
        line_start++;
    }

    // Localiza fim da linha
    size_t line_end = input.find('\n', filePos);
    if (line_end == std::string::npos) {
        line_end = input.size();
    }

    std::string line_str = input.substr(line_start, line_end - line_start);
    std::cerr << line_str << "\n";

    // Marca a coluna
    for (int i = 1; i < tok.column; i++) {
        std::cerr << " ";
    }
    std::cerr << "^\n";

    throw std::runtime_error("Erro de sintaxe.");
}

// Emite erro semântico
void Parser::semantic_error(const Token &tok, const std::string &msg) {
    std::cerr << "Erro semântico na linha " << tok.line << ", coluna " << tok.column
              << ": " << msg << " (encontrado: " << tok.value << ")\n";
    throw std::runtime_error("Erro semântico.");
}

// Verifica se o token atual é igual ao esperado e avança, caso contrário lança erro
void Parser::expect(TokenId expected) {
    if (current_token.id == expected) {
        advance();
    } else {
        parser_error(current_token, "Token esperado não encontrado");
    }
}

bool Parser::check(TokenId id) {
    return current_token.id == id;
}

// Mapeia tipos básicos do token para VarType
VarType Parser::mapBasicType(TokenId id) {
    switch (id) {
        case TOK_INT: return TYPE_INT;
        case TOK_REAL: return TYPE_REAL;
        case TOK_BOOL: return TYPE_BOOL;
        case TOK_STRING: return TYPE_STRING;
        default: return TYPE_UNKNOWN;
    }
}

// Parseia um tipo (int, real, array, struct etc.) // Corrigir aos testes
VarType Parser::parseTypeAndGetVarType() {
    if (check(TOK_BOOL) || check(TOK_INT) || check(TOK_REAL) || check(TOK_STRING)) {
        VarType t = mapBasicType(current_token.id);
        advance();
        return t;
    }
    else if (check(TOK_ARRAY)) {
        advance();
        expect(TOK_LBRACKET);
        parseNumber(); // Exemplo simples (poderia guardar valor)
        expect(TOK_DOT_DOT);
        parseNumber();
        expect(TOK_RBRACKET);
        expect(TOK_OF);
        VarType elemType = parseTypeAndGetVarType();
        if (elemType == TYPE_INT) return TYPE_ARRAY_INT;
        if (elemType == TYPE_REAL) return TYPE_ARRAY_REAL;
        if (elemType == TYPE_BOOL) return TYPE_ARRAY_BOOL;
        if (elemType == TYPE_STRING) return TYPE_ARRAY_STRING;
        return TYPE_UNKNOWN;
    }
    else if (check(TOK_STRUCT)) {
        advance();
        while (check(TOK_IDENTIFIER)) {
            parseStructDeclaration();
        }
        expect(TOK_END_STRUCT);
        return TYPE_UNKNOWN; // Exemplo: não tratamos struct em VarType
    }
    else {
        parser_error(current_token, "Tipo inválido");
        return TYPE_UNKNOWN;
    }
}

// Exemplo de parse de uma declaração de struct
void Parser::parseStructDeclaration() {
    expect(TOK_IDENTIFIER);
    expect(TOK_COLON);
    parseTypeAndGetVarType();
    expect(TOK_SEMICOLON);
}

// Para valores simples (int, float, string, bool)
void Parser::parseValue() {
    if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL)
        || check(TOK_STRING_LITERAL) || check(TOK_TRUE) || check(TOK_FALSE)) {
        advance();
    } else {
        parser_error(current_token, "Valor inválido na atribuição");
    }
}

void Parser::parseNumber() {
    if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL)) {
        advance();
    } else {
        parser_error(current_token, "Número esperado");
    }
}

bool Parser::isVarBlockStart() {
    return check(TOK_VAR);
}

bool Parser::isVarDeclarationStart() {
    return check(TOK_IDENTIFIER);
}

void Parser::parseVarDeclaration() {
    Token varName = current_token;
    expect(TOK_IDENTIFIER);
    expect(TOK_COLON);
    VarType varType = parseTypeAndGetVarType();

    bool initialized = false;
    if (check(TOK_ATRIBUICAO)) {
        advance();
        if (check(TOK_LBRACKET)) {
            if (varType != TYPE_ARRAY_INT && varType != TYPE_ARRAY_REAL
                && varType != TYPE_ARRAY_BOOL && varType != TYPE_ARRAY_STRING) {
                semantic_error(current_token, "Inicialização de array em variável não-array");
            }
            auto init_values = parseArrayInitializer();
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
            // Inicialização escalar
            Token lit = current_token;
            if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL)
                || check(TOK_STRING_LITERAL) || check(TOK_TRUE) || check(TOK_FALSE)) {
                advance();
                std::string temp = newTemp();
                if (lit.id == TOK_INT_LITERAL || lit.id == TOK_REAL_LITERAL) {
                    code.push_back("LOAD_IMM " + lit.value + " " + temp);
                } else if (lit.id == TOK_STRING_LITERAL) {
                    code.push_back("LOAD_IMM \"" + lit.value + "\" " + temp);
                } else if (lit.id == TOK_TRUE) {
                    code.push_back("LOAD_IMM 1 " + temp);
                } else if (lit.id == TOK_FALSE) {
                    code.push_back("LOAD_IMM 0 " + temp);
                }
                code.push_back("STORE " + temp + " " + varName.value);
                initialized = true;
            } else {
                parser_error(current_token, "Valor inválido na atribuição");
            }
        }
    }

    expect(TOK_SEMICOLON);
    symtab.declareVar(varName.value, varType);
}

// Exemplo de parse de array initializer
std::vector<std::pair<std::string,int>> Parser::parseArrayInitializer() {
    std::vector<std::pair<std::string,int>> values;
    expect(TOK_LBRACKET);
    if (!check(TOK_RBRACKET)) {
        do {
            values.push_back(parseInitValue());
        } while (check(TOK_COMMA) && (advance(), true));
    }
    expect(TOK_RBRACKET);
    return values;
}

std::pair<std::string,int> Parser::parseInitValue() {
    std::string val = parseExpressionToIR();
    int rep = 1;
    if (check(TOK_LPAREN)) {
        advance();
        if (!check(TOK_INT_LITERAL)) {
            parser_error(current_token, "Número esperado após '('");
        }
        int count = std::stoi(current_token.value);
        advance();
        expect(TOK_RPAREN);
        rep = count;
    }
    return {val, rep};
}

bool Parser::isInstructionStart() {
    return check(TOK_IDENTIFIER) || check(TOK_IF);
}

void Parser::parseInstructions() {
    while (isInstructionStart()) {
        parseInstruction();
    }
}

void Parser::parseInstruction() {
    if (check(TOK_IDENTIFIER)) {
        parseAssignment();
    } else if (check(TOK_IF)) {
        parseIfStatement();
    } else {
        parser_error(current_token, "Instrução inválida");
    }
}

void Parser::parseAssignment() {
    Token varName = current_token;
    expect(TOK_IDENTIFIER);
    if (!symtab.isDeclared(varName.value)) {
        semantic_error(varName, "Variável não declarada");
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

    expect(TOK_ATRIBUICAO);
    std::string rhs = parseExpressionToIR();
    expect(TOK_SEMICOLON);

    if (indices.empty()) {
        code.push_back("STORE " + rhs + " " + varName.value);
    } else {
        // Para arrays unidimensionais, peguei apenas o primeiro índice
        // Caso deseje multidimensional, precisará ajustar
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
    }
    else if (check(TOK_IDENTIFIER)) {
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

        std::string temp = newTemp();
        if (indices.empty()) {
            code.push_back("LOAD " + varName.value + " " + temp);
        } else {
            code.push_back("LOAD_INDEX " + varName.value + " " + indices[0] + " " + temp);
        }
        return temp;
    }
    else if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL)) {
        Token lit = current_token;
        advance();
        std::string temp = newTemp();
        code.push_back("LOAD_IMM " + lit.value + " " + temp);
        return temp;
    }
    else if (check(TOK_STRING_LITERAL)) {
        Token lit = current_token;
        advance();
        std::string temp = newTemp();
        code.push_back("LOAD_IMM \"" + lit.value + "\" " + temp);
        return temp;
    }
    else if (check(TOK_TRUE)) {
        Token lit = current_token;
        advance();
        std::string temp = newTemp();
        code.push_back("LOAD_IMM 1 " + temp);
        return temp;
    }
    else if (check(TOK_FALSE)) {
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

void Parser::parseIfStatement() {
    expect(TOK_IF);

    // Em vez de parseExpressionToIR(), chame parseConditionToIR()
    std::string cond = parseConditionToIR();

    expect(TOK_THEN);
    parseInstructions();

    if (check(TOK_ELSE)) {
        advance();
        parseInstructions();
    }

    expect(TOK_END_IF);
}

// parseComparisonToIR() lida com a parte "expr < expr", "expr <= expr" etc.
std::string Parser::parseComparisonToIR() {
    // Primeiro, parseia uma expressão aritmética
    std::string left = parseExpressionToIR();

    // Se o próximo token for operador relacional ( <, >, =, <> etc. )
    if (check(TOK_IGUAL) || check(TOK_DIFERENTE) ||
        check(TOK_MENOR) || check(TOK_MENOR_IGUAL) ||
        check(TOK_MAIOR) || check(TOK_MAIOR_IGUAL))
    {
        Token op = current_token;
        parseRelationalOperator();   // avança e valida o operador
        std::string right = parseExpressionToIR(); // parseia a expressão da direita

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
        // Gera IR da comparação
        code.push_back(instr + " " + left + " " + right + " " + temp);
        return temp; // Retorna o temporário que guarda o resultado booleano da comparação
    }
    // Caso não haja operador relacional, apenas retorna a expressão (ex: "x" sozinho).
    return left;
}

// parseConditionToIR() é responsável por parsear expressões lógicas
// que podem conter vários AND/OR.
std::string Parser::parseConditionToIR() {
    // Primeiro, parseia uma comparação
    std::string left = parseComparisonToIR();

    // Enquanto o próximo token for AND/OR, continua parseando
    while (check(TOK_AND) || check(TOK_OR)) {
        Token op = current_token;  // AND ou OR
        advance();                 // consome AND/OR

        // Parseia a próxima comparação
        std::string right = parseComparisonToIR();

        // Gera um temporário para guardar o resultado
        std::string temp = newTemp();

        // Exemplo de instrução IR:
        // - Pode usar "LOGICAL_AND" / "LOGICAL_OR" ou qualquer nome que preferir
        if (op.id == TOK_AND) {
            code.push_back("LOGICAL_AND " + left + " " + right + " " + temp);
        } else {
            code.push_back("LOGICAL_OR " + left + " " + right + " " + temp);
        }

        // O resultado da expressão parcial passa a ser o "temp"
        left = temp;
    }

    // Ao final, left contém o "temporário" que guarda o resultado da expressão booleana inteira
    return left;
}

void Parser::parseRelationalOperator() {
    if (check(TOK_IGUAL) || check(TOK_DIFERENTE) || check(TOK_MENOR)
        || check(TOK_MENOR_IGUAL) || check(TOK_MAIOR) || check(TOK_MAIOR_IGUAL)) {
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
    } else if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL)
               || check(TOK_STRING_LITERAL) || check(TOK_TRUE) || check(TOK_FALSE)) {
        advance();
    } else {
        parser_error(current_token, "Fator inválido");
    }
}

void Parser::parseProgram() {
    expect(TOK_PROGRAM);
    expect(TOK_IDENTIFIER);

    // Enquanto o token for TYPE, parseia blocos de tipo
    while (check(TOK_TYPE)) {
        parseTypeBlock();  // <-- Implementa a leitura de TYPE ... END_TYPE
    }

    // Enquanto o token for VAR, parseia blocos de variáveis
    while (isVarBlockStart()) {
        parseVarBlock();
    }

    // Instruções do corpo do programa
    parseInstructions();

    // Encerrar programa
    expect(TOK_END_PROGRAM);
}

void Parser::parseTypeBlock() {
    expect(TOK_TYPE);             // consome "TYPE"
    // Poderão existir várias declarações de tipo em sequência
    while (!check(TOK_END_TYPE) && current_token.id != TOK_EOF) {
        parseTypeDeclaration();   // Ex.: FREQ : REAL := 50.0;
    }
    expect(TOK_END_TYPE);         // consome "END_TYPE"
}

void Parser::parseVarBlock() {
    expect(TOK_VAR);
    while (isVarDeclarationStart()) {
        parseVarDeclaration();
    }
    expect(TOK_END_VAR);
}

// parseTypeDeclaration:
//   <IDENT> :  (BASE / ENUM / SUBRANGE / ARRAY / STRUCT / DERIVADO) [ := init ] ;
// Exemplo: FREQ : REAL := 50.0;
//          MyEnum : (RED, GREEN, BLUE) := GREEN;
//          MyRange : INT (-4095..4095) := 0;
//          MyArr : ARRAY [1..16] OF MyInt := [8(0), 8(4095)];
//          MyStruct : STRUCT ... END_STRUCT
//          MyDerived : AnotherStruct := ( field1 := 0, field2 := 4000 );
void Parser::parseTypeDeclaration() {
    // Ler nome do tipo
    if (!check(TOK_IDENTIFIER)) {
        parser_error(current_token, "Nome de tipo esperado");
    }
    std::string typeName = current_token.value;
    advance(); // consome identificador

    expect(TOK_COLON);

    UserTypeInfo info;
    info.name = typeName;
    info.kind = UTYPE_BASE; // default

    // Verifica se é '(' => enumerado,
    // se é base => INT, REAL, etc.
    // se é 'IDENT(' => subrange
    // se é 'ARRAY' => array
    // se é 'STRUCT' => struct
    // ou se é outro tipo => derivado
    if (check(TOK_LPAREN)) {
        // Pode ser enumerado OU subrange
        // Precisamos olhar se há .. (subrange) ou vírgula (enum)
        // Olha próximo token sem consumir
        advance(); // consome '('
        if (check(TOK_INT_LITERAL) || check(TOK_MINUS)) {
            // Subrange: ex.: INT (-4095..4095)
            // Precisamos de um base type antes, ex: INT
            parser_error(current_token, "Sintaxe de subrange incompleta (falta base type antes do '(').");
        }
        else {
            // enum
            info.kind = UTYPE_ENUM;
            // parse enumeradores
            // ex: (RED, GREEN, BLUE)
            while (!check(TOK_RPAREN)) {
                if (!check(TOK_IDENTIFIER)) {
                    parser_error(current_token, "Identificador de enumerador esperado");
                }
                info.enumValues.push_back(current_token.value);
                advance();
                if (check(TOK_COMMA)) {
                    advance();
                } else {
                    break;
                }
            }
            expect(TOK_RPAREN);
        }
    }
    else if (check(TOK_INT) || check(TOK_REAL) || check(TOK_BOOL) || check(TOK_STRING)) {
        // Tipo base
        info.baseName = current_token.value;
        info.kind = UTYPE_BASE;
        advance();
        // Verifica se é subrange:
        if (check(TOK_LPAREN)) {
            // subrange
            // ex: INT ( -4095..4095 )
            info.kind = UTYPE_SUBRANGE;
            advance(); // '('
            // parse low bound
            // neste ex, pode ser int literal ou '-'
            // para simplificar, assumo TOK_INT_LITERAL
            if (!check(TOK_INT_LITERAL) && !check(TOK_MINUS)) {
                parser_error(current_token, "Valor inteiro esperado para limite inferior do subrange");
            }
            bool neg = false;
            long lower = 0;
            if (check(TOK_MINUS)) {
                neg = true;
                advance();
            }
            if (!check(TOK_INT_LITERAL)) {
                parser_error(current_token, "Valor inteiro esperado apos '-'");
            }
            lower = std::stol(current_token.value);
            if (neg) lower = -lower;
            advance();
            expect(TOK_DOT_DOT);
            // parse upper bound
            neg = false;
            long upper = 0;
            if (check(TOK_MINUS)) {
                neg = true;
                advance();
            }
            if (!check(TOK_INT_LITERAL)) {
                parser_error(current_token, "Valor inteiro esperado para limite superior do subrange");
            }
            upper = std::stol(current_token.value);
            if (neg) upper = -upper;
            advance();
            expect(TOK_RPAREN);
            info.subrangeLow = lower;
            info.subrangeHigh = upper;
        }
    }
    else if (check(TOK_ARRAY)) {
        // ex: ARRAY [1..16] OF INT
        info.kind = UTYPE_ARRAY;
        advance(); // consome ARRAY
        expect(TOK_LBRACKET);
        if (!check(TOK_INT_LITERAL)) {
            parser_error(current_token, "Valor inteiro esperado para indice inferior do array");
        }
        long low = std::stol(current_token.value);
        advance();
        expect(TOK_DOT_DOT);
        if (!check(TOK_INT_LITERAL)) {
            parser_error(current_token, "Valor inteiro esperado para indice superior do array");
        }
        long high = std::stol(current_token.value);
        advance();
        expect(TOK_RBRACKET);
        expect(TOK_OF);
        // baseName do array
        if (!check(TOK_IDENTIFIER) &&
            !check(TOK_INT) && !check(TOK_REAL) && !check(TOK_BOOL) && !check(TOK_STRING)) {
            parser_error(current_token, "Tipo esperado após OF");
        }
        std::string arrType = current_token.value;
        advance();
        info.arrayLow = low;
        info.arrayHigh = high;
        info.arrayElementType = arrType;
    }
    else if (check(TOK_STRUCT)) {
        // STRUCT ... END_STRUCT
        info.kind = UTYPE_STRUCT;
        advance();
        while (!check(TOK_END_STRUCT)) {
            // parse field
            if (!check(TOK_IDENTIFIER)) {
                parser_error(current_token, "Nome de campo esperado");
            }
            UserTypeInfo::Field f;
            f.name = current_token.value;
            advance();
            expect(TOK_COLON);
            // aqui poderia parsear um tipo, mas simplificamos assumindo IDENT
            if (!check(TOK_IDENTIFIER) &&
                !check(TOK_INT) && !check(TOK_REAL) && !check(TOK_BOOL) && !check(TOK_STRING)) {
                parser_error(current_token, "Tipo de campo esperado");
            }
            f.type = current_token.value;
            advance();
            // opcional init
            if (check(TOK_ATRIBUICAO)) {
                advance();
                // parse literal
                if (!check(TOK_INT_LITERAL) && !check(TOK_REAL_LITERAL)
                    && !check(TOK_STRING_LITERAL) && !check(TOK_TRUE) && !check(TOK_FALSE)) {
                    parser_error(current_token, "Valor literal esperado para inicialização de campo");
                }
                f.initValue = current_token.value;
                advance();
            }
            expect(TOK_SEMICOLON);
            info.structFields.push_back(f);
        }
        expect(TOK_END_STRUCT);
    }
    else if (check(TOK_IDENTIFIER)) {
        // derivado de outro type
        info.kind = UTYPE_DERIVED;
        info.baseName = current_token.value; // ex: AnotherStruct
        advance();
    }
    else {
        parser_error(current_token, "Tipo de dado não reconhecido");
    }

    // Verifica se existe inicialização do tipo => " := <alguma coisa>"
    if (check(TOK_ATRIBUICAO)) {
        // ex:  := 50.0
        // ex:  := ( MIN_SCALE := 0, MAX_SCALE := 4000 )
        advance();
        // se for struct derivado, ex:  ( field1 := 0, field2 := 4000 )
        if (info.kind == UTYPE_DERIVED && check(TOK_LPAREN)) {
            advance(); // '('
            // parse list: <field> := <value>
            while (!check(TOK_RPAREN)) {
                if (!check(TOK_IDENTIFIER)) {
                    parser_error(current_token, "Nome de campo esperado em init do type derivado");
                }
                std::string fld = current_token.value;
                advance();
                expect(TOK_ATRIBUICAO);
                if (!check(TOK_INT_LITERAL) && !check(TOK_REAL_LITERAL)
                    && !check(TOK_STRING_LITERAL) && !check(TOK_TRUE) && !check(TOK_FALSE)) {
                    parser_error(current_token, "Literal esperado para init de campo");
                }
                std::string val = current_token.value;
                advance();
                info.derivedInits.push_back({fld, val});
                if (check(TOK_COMMA)) {
                    advance();
                } else {
                    break;
                }
            }
            expect(TOK_RPAREN);
        }
        // se for base ou subrange => parse literal
        else if (info.kind == UTYPE_BASE || info.kind == UTYPE_SUBRANGE) {
            if (!check(TOK_INT_LITERAL) && !check(TOK_REAL_LITERAL)
                && !check(TOK_STRING_LITERAL) && !check(TOK_TRUE) && !check(TOK_FALSE)) {
                parser_error(current_token, "Valor literal inválido para init do tipo base/subrange");
            }
            info.hasInitValue = true;
            info.initValueString = current_token.value;
            // se for num, podemos converter
            if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL)) {
                info.initValueNumeric = std::stod(current_token.value);
            }
            advance();
        }
        // se for enum => ident
        else if (info.kind == UTYPE_ENUM) {
            if (!check(TOK_IDENTIFIER)) {
                parser_error(current_token, "Enumerador esperado para init do enum");
            }
            info.enumInit = current_token.value;
            advance();
        }
        // se for array => parse ex: [8(4095), 8(-4095)]
        else if (info.kind == UTYPE_ARRAY) {
            if (!check(TOK_LBRACKET)) {
                parser_error(current_token, "Esperado '[' para init de array");
            }
        }
        else if (info.kind == UTYPE_STRUCT) {
            // Ex: := ( campo1 := valor1, campo2 := valor2 )
            // Em ST real, normalmente não se faz init de struct ali,
            // mas se quiser, seria lógico parsear algo similar ao DERIVED
            // Deixamos como exercício.
        }
    }

    // Por fim, exige ';'
    expect(TOK_SEMICOLON);

    // Armazena esse tipo na TypeTable
    typetab.declareType(typeName, info);
}

void Parser::parse() {
    parseProgram();
    if (current_token.id != TOK_EOF) {
        parser_error(current_token, "Tokens extras após fim do programa");
    }

    std::cout << "=== CODE GENERATION (IR) ===\n";
    for (const auto &instr : code) {
        std::cout << instr << "\n";
    }
}

int main(int argc, char **argv) {
    // Exemplo de uso: nome de arquivo fixo ou via argv
    std::string nomeArquivo = (argc > 1) ? argv[1]
                                         : "/home/kaynan/Documentos/desenvolvimento/c++/Compilador-st-am/teste-1.st";

    std::ifstream file(nomeArquivo);
    if (!file) {
        std::cerr << "Não foi possível abrir o arquivo: " << nomeArquivo << "\n";
        return 1;
    }

    // Lê todo o conteúdo do arquivo
    std::string code((std::istreambuf_iterator<char>(file)),
                     std::istreambuf_iterator<char>());

    // Inicializa o lexer e consome tokens para debug
    Lexer lexer(code);
    {
        Token token = lexer.next_token();
        while (token.id != TOK_EOF) {
            std::cout << token.to_string() << "\n";
            token = lexer.next_token();
        }
    }

    // Reinicializa o lexer para o parser (pois avançamos o lexer acima)
    Lexer lexer2(code);
    Parser parser(lexer2);

    try {
        parser.parse();
        std::cout << "Parsing, análise semântica e geração de código concluídos com sucesso!\n";
    } catch (const std::exception &e) {
        std::cerr << "Falha: " << e.what() << "\n";
        return 1;
    }

    return 0;
}

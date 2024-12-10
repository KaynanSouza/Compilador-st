#include <iostream>
#include <string>
#include <cctype>
#include <memory>
#include <unordered_map>
#include <vector>
#include <stdexcept>
#include <fstream>

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

enum TokenId {
    TOK_VAR,
    TOK_END_VAR,
    TOK_IF,
    TOK_THEN,
    TOK_ELSE,
    TOK_END_IF,
    TOK_BOOL,
    TOK_INT,
    TOK_REAL,
    TOK_STRING,
    TOK_ARRAY,
    TOK_STRUCT,
    TOK_FUNCTION,
    TOK_END_FUNCTION,
    TOK_OF,
    TOK_PROGRAM,
    TOK_END_PROGRAM,
    TOK_TRUE,
    TOK_FALSE,
    TOK_END_STRUCT,

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

    TOK_PLUS,
    TOK_MINUS,
    TOK_MULTIPLY,
    TOK_DIVIDE,
    TOK_MOD,

    TOK_DIFERENTE,
    TOK_MAIOR,
    TOK_MENOR,
    TOK_MAIOR_IGUAL,
    TOK_MENOR_IGUAL,

    TOK_AND,
    TOK_OR,
    TOK_NOT,
    TOK_XOR,

    TOK_INT_LITERAL,
    TOK_REAL_LITERAL,
    TOK_STRING_LITERAL,
    TOK_IDENTIFIER,

    TOK_COMMENT,
    TOK_EOF,
    TOK_INVALID,
};

class Token {
public:
    TokenId id;
    std::string value;
    int line;
    int column;

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
    std::string input;
    size_t pos = 0;
    int current_line = 1;
    int current_column = 1;

    char peek() const {
        return pos < input.size() ? input[pos] : '\0';
    }

    char advance() {
        char current = peek();
        pos++;
        if (current == '\n') {
            current_line++;
            current_column = 1;
        } else {
            current_column++;
        }
        return current;
    }

    void skip_whitespace() {
        while (isspace((unsigned char)peek())) advance();
    }

    void skip_comment() {
        advance(); // '('
        advance(); // '*'
        while (peek() != '*' || pos+1 >= input.size() || input[pos+1] != ')') {
            if (peek() == '\0') {
                std::cerr << "Unterminated comment on line " << current_line << "\n";
                return;
            }
            advance();
        }
        advance(); // '*'
        advance(); // ')'
    }

    std::string parse_identifier() {
        std::string ident;
        while (isalnum((unsigned char)peek()) || peek() == '_') {
            ident += advance();
        }
        return ident;
    }

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
        return it != keywords.end() ? it->second : TOK_IDENTIFIER;
    }

    Token parse_number() {
        std::string num;
        bool is_real = false;
        while (isdigit((unsigned char)peek()) || peek() == '.') {
            if (peek() == '.') {
                if (pos+1 < input.size() && input[pos+1] == '.') {
                    break;
                }
                if (is_real) break;
                is_real = true;
            }
            num += advance();
        }
        return is_real ? Token(TOK_REAL_LITERAL, num, current_line, current_column)
                       : Token(TOK_INT_LITERAL, num, current_line, current_column);
    }

    Token parse_string() {
        advance(); // skip "
        std::string str;
        while (peek() != '"' && peek() != '\0') {
            str += advance();
        }
        if (peek() == '"') {
            advance();
            return Token(TOK_STRING_LITERAL, str, current_line, current_column);
        } else {
            std::cerr << "Unterminated string on line " << current_line << "\n";
            return Token(TOK_INVALID, str, current_line, current_column);
        }
    }

    Token parse_range_or_dot() {
        advance(); // '.'
        if (peek() == '.') {
            advance();
            return Token(TOK_DOT_DOT, "..", current_line, current_column);
        } else {
            return Token(TOK_INVALID, ".", current_line, current_column);
        }
    }

public:
    Lexer(const std::string& input) : input(input) {}

    Token next_token() {
        skip_whitespace();

        if (peek() == '\0') return Token(TOK_EOF, "EOF", current_line, current_column);

        if (peek() == '(' && pos+1 < input.size() && input[pos+1] == '*') {
            skip_comment();
            return next_token();
        }

        if (isalpha((unsigned char)peek()) || peek() == '_') {
            std::string ident = parse_identifier();
            TokenId id = classify_keyword(ident);
            return Token(id, ident, current_line, current_column);
        }

        if (isdigit((unsigned char)peek())) {
            return parse_number();
        }

        if (peek() == '"') {
            return parse_string();
        }

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
            default: {
                std::string invalid(1, advance());
                return Token(TOK_INVALID, invalid, current_line, current_column);
            }
        }
    }

    size_t getPos() const { return pos; }
    const std::string &getInput() const { return input; }
};

class SymbolTable {
private:
    std::unordered_map<std::string, VarType> table;
public:
    void declareVar(const std::string &name, VarType type) {
        table[name] = type;
    }

    bool isDeclared(const std::string &name) const {
        return table.find(name) != table.end();
    }

    VarType getType(const std::string &name) const {
        auto it = table.find(name);
        if (it == table.end()) return TYPE_UNKNOWN;
        return it->second;
    }
};

class Parser {
private:
    Lexer &lexer;
    Token current_token;
    SymbolTable symtab;
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
    std::string newTemp() {
        static int temp_count = 0;
        return "t" + std::to_string(++temp_count);
    }

public:
    Parser(Lexer &l) : lexer(l) {
        current_token = lexer.next_token();
    }

    void parse();
};



void Parser::advance() {
    current_token = lexer.next_token();
}

void Parser::parser_error(const Token &tok, const std::string &msg) {
    std::cerr << "Erro de sintaxe na linha " << tok.line << ", coluna " << tok.column
              << ": " << msg << " (encontrado: " << tok.value << ")\n";

    const std::string &input = lexer.getInput();
    size_t pos = lexer.getPos();

    size_t line_start = input.rfind('\n', pos);
    if (line_start == std::string::npos)
        line_start = 0;
    else
        line_start = line_start + 1;

    size_t line_end = input.find('\n', pos);
    if (line_end == std::string::npos)
        line_end = input.size();

    std::string line_str = input.substr(line_start, line_end - line_start);
    std::cerr << line_str << "\n";

    for (int i = 1; i < tok.column; i++) std::cerr << " ";
    std::cerr << "^\n";

    throw std::runtime_error("Erro de sintaxe.");
}

void Parser::semantic_error(const Token &tok, const std::string &msg) {
    std::cerr << "Erro semântico na linha " << tok.line << ", coluna " << tok.column
              << ": " << msg << " (encontrado: " << tok.value << ")\n";
    throw std::runtime_error("Erro semântico.");
}

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

VarType Parser::mapBasicType(TokenId id) {
    switch (id) {
        case TOK_INT: return TYPE_INT;
        case TOK_REAL: return TYPE_REAL;
        case TOK_BOOL: return TYPE_BOOL;
        case TOK_STRING: return TYPE_STRING;
        default: return TYPE_UNKNOWN;
    }
}

VarType Parser::parseTypeAndGetVarType() {
    if (check(TOK_BOOL) || check(TOK_INT) || check(TOK_REAL) || check(TOK_STRING)) {
        VarType t = mapBasicType(current_token.id);
        advance();
        return t;
    } else if (check(TOK_ARRAY)) {
        advance();
        expect(TOK_LBRACKET);
        parseNumber();
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
    } else if (check(TOK_STRUCT)) {
        advance();
        while (check(TOK_IDENTIFIER)) {
            parseStructDeclaration();
        }
        expect(TOK_END_STRUCT);
        return TYPE_UNKNOWN;
    } else {
        parser_error(current_token, "Tipo inválido");
        return TYPE_UNKNOWN;
    }
}

void Parser::parseStructDeclaration() {
    expect(TOK_IDENTIFIER);
    expect(TOK_COLON);
    parseTypeAndGetVarType();
    expect(TOK_SEMICOLON);
}

void Parser::parseValue() {
    // parseValue é apenas para valores escalares
    if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL) || check(TOK_STRING_LITERAL) || check(TOK_TRUE) || check(TOK_FALSE)) {
        advance(); // Apenas consome por agora
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
        advance(); // consome :=
        if (check(TOK_LBRACKET)) {
            // inicialização de array
            if (varType != TYPE_ARRAY_INT && varType != TYPE_ARRAY_REAL && varType != TYPE_ARRAY_BOOL && varType != TYPE_ARRAY_STRING) {
                semantic_error(current_token, "Inicialização de array em variável não-array");
            }

            auto init_values = parseArrayInitializer();
            // gerar IR para inicialização
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
            // inicialização escalar
            // Precisamos gerar IR
            Token lit = current_token;
            if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL) || check(TOK_STRING_LITERAL) || check(TOK_TRUE) || check(TOK_FALSE)) {
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
    // init_value ::= <expression> [ '(' <number> ')' ]
    std::string val = parseExpressionToIR();
    int rep = 1;
    if (check(TOK_LPAREN)) {
        advance(); // '('
        if (!check(TOK_INT_LITERAL)) {
            parser_error(current_token, "Número esperado após '('");
        }
        int count = std::stoi(current_token.value);
        advance(); // consome numero
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

    if (argc < 2) {
        std::cerr << "Uso: " << argv[0] << " <arquivo fonte>\n";
        return 1;
    }

    std::ifstream file(argv[1]);
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

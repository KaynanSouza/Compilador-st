#include <iostream>
#include <string>
#include <cctype>
#include <memory>
#include <unordered_map>
#include <vector>
#include <stdexcept>

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
        advance();
        advance();
        while (peek() != '*' || input.size() <= pos+1 || input[pos+1] != ')') {
            if (peek() == '\0') {
                std::cerr << "Unterminated comment on line " << current_line << "\n";
                return;
            }
            advance();
        }
        advance();
        advance();
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
                if (is_real) break;
                is_real = true;
            }
            num += advance();
        }

        return is_real ? Token(TOK_REAL_LITERAL, num, current_line, current_column)
                       : Token(TOK_INT_LITERAL, num, current_line, current_column);
    }

    Token parse_string() {
        advance();
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
        advance();
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

        if (peek() == '(' && input.size() > pos+1 && input[pos+1] == '*') {
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
};

void parser_error(const Token &tok, const std::string &msg) {
    std::cerr << "Erro de sintaxe na linha " << tok.line << ", coluna " << tok.column
              << ": " << msg << " (encontrado: " << tok.value << ")\n";
    throw std::runtime_error("Erro de sintaxe.");
}

class Parser {
private:
    Lexer &lexer;
    Token current_token;

    void advance() {
        current_token = lexer.next_token();
    }

    void expect(TokenId expected) {
        if (current_token.id == expected) {
            advance();
        } else {
            parser_error(current_token, "Token esperado não encontrado");
        }
    }

    bool check(TokenId id) {
        return current_token.id == id;
    }

    void parseProgram() {
        expect(TOK_PROGRAM);
        expect(TOK_IDENTIFIER); // nome do programa
        while (isVarBlockStart()) {
            parseVarBlock();
        }

        parseInstructions();
        expect(TOK_END_PROGRAM);
    }

    bool isVarBlockStart() {
        return check(TOK_VAR);
    }

    void parseVarBlock() {
        expect(TOK_VAR);
        while (isVarDeclarationStart()) {
            parseVarDeclaration();
        }
        expect(TOK_END_VAR);
    }

    bool isVarDeclarationStart() {
        return check(TOK_IDENTIFIER);
    }

    void parseVarDeclaration() {
        expect(TOK_IDENTIFIER);
        expect(TOK_COLON);
        parseType();

        if (check(TOK_ATRIBUICAO)) {
            advance();
            parseValue();
        }

        expect(TOK_SEMICOLON);
    }

    void parseType() {
        if (check(TOK_BOOL) || check(TOK_INT) || check(TOK_REAL) || check(TOK_STRING)) {
            advance();
        } else if (check(TOK_ARRAY)) {
            advance();
            expect(TOK_LBRACKET);
            parseNumber();
            expect(TOK_DOT_DOT);
            parseNumber();
            expect(TOK_RBRACKET);
            expect(TOK_OF);
            parseType();
        } else if (check(TOK_STRUCT)) {
            advance();
            while (check(TOK_IDENTIFIER)) {
                parseStructDeclaration();
            }
            expect(TOK_END_STRUCT);
        } else {
            parser_error(current_token, "Tipo inválido");
        }
    }

    void parseStructDeclaration() {
        expect(TOK_IDENTIFIER);
        expect(TOK_COLON);
        parseType();
        expect(TOK_SEMICOLON);
    }

    void parseValue() {
        if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL) || check(TOK_STRING_LITERAL)) {
            advance();
        } else if (check(TOK_TRUE) || check(TOK_FALSE)) {
            advance();
        } else {
            parser_error(current_token, "Valor inválido na atribuição");
        }
    }

    void parseNumber() {
        if (check(TOK_INT_LITERAL)) {
            advance();
        } else if (check(TOK_REAL_LITERAL)) {
            advance();
        } else {
            parser_error(current_token, "Número esperado");
        }
    }

    void parseInstructions() {
        while (isInstructionStart()) {
            parseInstruction();
        }
    }

    bool isInstructionStart() {
        return check(TOK_IDENTIFIER) || check(TOK_IF);
    }

    void parseInstruction() {
        if (check(TOK_IDENTIFIER)) {
            parseAssignment();
        } else if (check(TOK_IF)) {
            parseIfStatement();
        } else {
            parser_error(current_token, "Instrução inválida");
        }
    }

    void parseAssignment() {
        expect(TOK_IDENTIFIER);
        expect(TOK_ATRIBUICAO);
        parseExpression();
        expect(TOK_SEMICOLON);
    }

    void parseExpression() {
        parseTerm();
        while (check(TOK_PLUS) || check(TOK_MINUS)) {
            advance();
            parseTerm();
        }
    }

    void parseTerm() {
        parseFactor();
        while (check(TOK_MULTIPLY) || check(TOK_DIVIDE) || check(TOK_MOD)) {
            advance();
            parseFactor();
        }
    }

    void parseFactor() {
        if (check(TOK_LPAREN)) {
            advance();
            parseExpression();
            expect(TOK_RPAREN);
        } else if (check(TOK_IDENTIFIER)) {
            advance();
        } else if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL)) {
            advance();
        } else {
            parser_error(current_token, "Fator inválido");
        }
    }

    void parseIfStatement() {
        expect(TOK_IF);
        parseCondition();
        expect(TOK_THEN);
        parseInstructions();

        if (check(TOK_ELSE)) {
            advance();
            parseInstructions();
        }

        expect(TOK_END_IF);
    }

    void parseCondition() {
        parseExpression();
        parseRelationalOperator();
        parseExpression();
    }

    void parseRelationalOperator() {
        if (check(TOK_IGUAL) || check(TOK_DIFERENTE) || check(TOK_MENOR) || check(TOK_MENOR_IGUAL) || check(TOK_MAIOR) || check(TOK_MAIOR_IGUAL)) {
            advance();
        } else {
            parser_error(current_token, "Operador relacional esperado");
        }
    }

public:
    Parser(Lexer &l) : lexer(l) {
        current_token = lexer.next_token();
    }

    void parse() {
        parseProgram();
        if (current_token.id != TOK_EOF) {
            parser_error(current_token, "Tokens extras após fim do programa");
        }
    }
};

int main() {
    // Código ST corrigido: remover o ';' após o nome do programa
    std::string code = R"(
        PROGRAM MyProg
        VAR

          x : INT;
          in: ARRAY[1..10] OF INT;

        END_VAR

        x := 10;
        IF x < 20   THEN
          x := x + 1;
        END_IF

        END_PROGRAM
    )";

    Lexer lexer(code);
    Parser parser(lexer);

    try {
        parser.parse();
        std::cout << "Parsing concluído com sucesso!\n";
    } catch (const std::exception &e) {
        std::cerr << "Falha na análise sintática: " << e.what() << "\n";
    }

    return 0;
}

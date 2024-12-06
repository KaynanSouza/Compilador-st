#include <iostream>
#include <string>
#include <cctype>
#include <memory>
#include <unordered_map>
#include <vector>

// Enumeração que define todos os tipos possíveis de tokens na linguagem ST
enum TokenId {
    // Palavras-chave
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

    // Operadores e símbolos
    TOK_LPAREN,       // (
    TOK_RPAREN,       // )
    TOK_LBRACKET,     // [
    TOK_RBRACKET,     // ]
    TOK_IGUAL,        // =
    TOK_ATRIBUICAO,   // :=
    TOK_COLON,        // :
    TOK_SEMICOLON,    // ;
    TOK_COMMA,        // ,
    TOK_DOT_DOT,      // ..

    // Operadores aritméticos
    TOK_PLUS,         // +
    TOK_MINUS,        // -
    TOK_MULTIPLY,     // *
    TOK_DIVIDE,       // /
    TOK_MOD,          // MOD

    // Operadores relacionais
    TOK_DIFERENTE,    // <>
    TOK_MAIOR,        // >
    TOK_MENOR,        // <
    TOK_MAIOR_IGUAL,  // >=
    TOK_MENOR_IGUAL,  // <=

    // Operadores lógicos
    TOK_AND,
    TOK_OR,
    TOK_NOT,
    TOK_XOR,

    // Literais e identificadores
    TOK_INT_LITERAL,
    TOK_REAL_LITERAL,
    TOK_STRING_LITERAL,
    TOK_IDENTIFIER,

    // Comentários, Fim de arquivo, Inválido
    TOK_COMMENT,
    TOK_EOF,
    TOK_INVALID,
};

// Classe representando um Token
class Token {
public:
    TokenId id;
    std::string value;
    int line;
    int column;

    Token(TokenId id, std::string value, int line, int column)
        : id(id), value(value), line(line), column(column) {}

    std::string to_string() const {
        return "Token(" + value + ", line: " + std::to_string(line) +
               ", col: " + std::to_string(column) + ")";
    }
};

// Classe Lexer: Responsável pela análise léxica do código fonte
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
        while (isspace(peek())) advance();
    }

    void skip_comment() {
        // Consome '(*'
        advance();
        advance();
        while (peek() != '*' || input[pos + 1] != ')') {
            if (peek() == '\0') {
                std::cerr << "Unterminated comment on line " << current_line << "\n";
                return;
            }
            advance();
        }
        // Consome '*)'
        advance();
        advance();
    }

    std::string parse_identifier() {
        std::string ident;
        while (isalnum(peek()) || peek() == '_') {
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
            {"OF", TOK_OF}, {"MOD", TOK_MOD}
        };
        auto it = keywords.find(ident);
        return it != keywords.end() ? it->second : TOK_IDENTIFIER;
    }

    Token parse_number() {
        std::string num;
        bool is_real = false;

        while (isdigit(peek()) || peek() == '.') {
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
        advance(); // Skip "
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
        // Quando encontramos um '.', precisamos verificar se é o operador de intervalo '..'
        advance(); // consome o primeiro '.'
        if (peek() == '.') {
            advance(); // consome o segundo '.'
            return Token(TOK_DOT_DOT, "..", current_line, current_column);
        } else {
            // Se não for '..', pode ser um erro ou um simples ponto
            return Token(TOK_INVALID, ".", current_line, current_column);
        }
    }

public:
    Lexer(const std::string& input) : input(input) {}

    Token next_token() {
        skip_whitespace();

        if (peek() == '\0') return Token(TOK_EOF, "EOF", current_line, current_column);

        // Comentário
        if (peek() == '(' && input.size() > pos+1 && input[pos+1] == '*') {
            skip_comment();
            return next_token();
        }

        // Identificadores e palavras-chave
        if (isalpha(peek()) || peek() == '_') {
            std::string ident = parse_identifier();
            TokenId id = classify_keyword(ident);
            return Token(id, ident, current_line, current_column);
        }

        // Números
        if (isdigit(peek())) {
            return parse_number();
        }

        // Strings
        if (peek() == '"') {
            return parse_string();
        }

        // Operadores e símbolos
        switch (peek()) {
            case '+': advance(); return Token(TOK_PLUS, "+", current_line, current_column);
            case '-': advance(); return Token(TOK_MINUS, "-", current_line, current_column);
            case '*': advance(); return Token(TOK_MULTIPLY, "*", current_line, current_column);
            case '/': advance(); return Token(TOK_DIVIDE, "/", current_line, current_column);
            case '[': advance(); return Token(TOK_LBRACKET, "[", current_line, current_column);
            case ']': advance(); return Token(TOK_RBRACKET, "]", current_line, current_column);
            case '.':
                // Verificar se é '..'
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

int main() {
    // Exemplo de código ST com FUNCTION, ARRAY, OF, etc.
    std::string code = R"(
        FUNCTION VAZAO : REAL
          VAR_INPUT
            IN1 : REAL;  (* ALTURA *)
            IN2 : REAL;  (* POTENCIA *)
            IN3 : ARRAY[0..3100] OF REAL; (* TABELA *)
          END_VAR
          VAR
            H1,H2,P1,P2: REAL;
            I,J: INT;
            B : BOOL;
          END_VAR
        END_FUNCTION
    )";

    Lexer lexer(code);
    Token token = lexer.next_token();

    while (token.id != TOK_EOF) {
        std::cout << token.to_string() << "\n";
        token = lexer.next_token();
    }

    return 0;
}

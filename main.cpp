    #include <iostream>
    #include <string>
    #include <cctype>
    #include <cmath>
    #include <memory>
    #include <unordered_map>
    #include <vector>
    #include <stdexcept>
    #include <fstream>

    // Tipos básicos
    enum VarType {
        TYPE_INT,
        TYPE_UINT,
        TYPE_REAL,
        TYPE_BYTE,
        TYPE_STRING,
        TYPE_BOOL,
        TYPE_DATE_AND_TIME,
        TYPE_DWORD,
        TYPE_TIME,
        // Arrays de básicos
        TYPE_ARRAY_INT,
        TYPE_ARRAY_UINT,
        TYPE_ARRAY_REAL,
        TYPE_ARRAY_STRING,
        TYPE_ARRAY_BOOL,
        // Desconhecido (ou não suportado ainda)
        TYPE_UNKNOWN
    };

    // Tabela de símbolos e lista de Tokens
    enum TokenId {
        TOK_VAR,
        TOK_END_VAR,
        TOK_FUNCTION,
        TOK_END_FUNCTION,
        TOK_OF,
        TOK_PROGRAM,
        TOK_END_PROGRAM,

        // Estruturas de controle
        TOK_IF,
        TOK_THEN,
        TOK_ELSE,
        TOK_END_IF,

        // Tipos básicos
        TOK_BOOL,
        TOK_INT,
        TOK_UINT,
        TOK_BYTE,
        TOK_REAL,
        TOK_STRING,
        TOK_CONSTANT,
        TOK_ARRAY,
        TOK_STRUCT,
        TOK_END_STRUCT,
        TOK_DATE_AND_TIME,
        TOK_TIME,

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
        TOK_RETAIN,
        TOK_WORD,
        TOK_DWORD,

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

        // Operadores lógicos e bitwise (Note: usamos os mesmos tokens AND/OR/NOT para ambos)
        TOK_AND,
        TOK_OR,
        TOK_NOT,
        TOK_XOR,

        // Literais
        TOK_INT_LITERAL,
        TOK_UINT_LITERAL,
        TOK_REAL_LITERAL,
        TOK_STRING_LITERAL,
        TOK_HEX_LITERAL,
        TOK_OCTAL_LITERAL,
        TOK_BINARY_LITERAL,
        TOK_IDENTIFIER,
        TOK_TIME_LITERAL,

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

        // Suporte a tipos IEC
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
            case TOK_UINT: return "TOK_UINT";
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

        std::string to_string() const  {
            return "Token(" + value + ", line: " + std::to_string(line) +
                   ", col: " + std::to_string(column) +
                   ", id: " + token_id_to_string(id) + ")";
        }
    };

    class Lexer {
    private:
        std::string input;
        size_t pos = 0;
        int current_line = 1;
        int current_column = 1;

        char peek() const {
            return (pos < input.size()) ? input[pos] : '\0';
        }

        char advance() {
            char c = peek();
            if (c != '\0') {
                pos++;
                if (c == '\n') {
                    current_line++;
                    current_column = 1;
                } else if (c == '\t') {
                    current_column += 4;
                } else {
                    current_column++;
                }
            }
            return c;
        }

        void skip_whitespace() {
            while (std::isspace((unsigned char)peek())) {
                advance();
            }
        }

        void skip_comment() {
            // Consome "(*"
            advance(); // '('
            advance(); // '*'
            while (true) {
                char c = peek();
                if (c == '\0') {
                    std::cerr << "Unterminated comment on line " << current_line << "\n";
                    return;
                }
                if (c == '*' && (pos+1 < input.size()) && input[pos+1] == ')') {
                    advance(); // consome '*'
                    advance(); // consome ')'
                    break;
                }
                advance();
            }
        }

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
                {"ELSIF", TOK_ELSIF},
                {"FUNCTION", TOK_FUNCTION}, {"END_FUNCTION", TOK_END_FUNCTION},
                {"OF", TOK_OF}, {"MOD", TOK_MOD},
                {"PROGRAM", TOK_PROGRAM}, {"END_PROGRAM", TOK_END_PROGRAM},
                {"TRUE", TOK_TRUE}, {"FALSE", TOK_FALSE},
                {"END_STRUCT", TOK_END_STRUCT}, {"VAR_INPUT", TOK_VAR_INPUT},
                {"END_VAR_INPUT", TOK_END_VAR_INPUT}, {"VAR_OUTPUT", TOK_VAR_OUTPUT},
                {"END_VAR_OUTPUT", TOK_END_VAR_OUTPUT}, {"FOR", TOK_FOR},
                {"TO", TOK_TO}, {"BY", TOK_BY}, {"DO", TOK_DO},
                {"END_FOR", TOK_END_FOR}, {"WHILE", TOK_WHILE},
                {"END_WHILE", TOK_END_WHILE}, {"REPEAT", TOK_REPEAT},
                {"UNTIL", TOK_UNTIL}, {"END_REPEAT", TOK_END_REPEAT},
                {"CASE", TOK_CASE}, {"END_CASE", TOK_END_CASE},
                {"TYPE", TOK_TYPE}, {"END_TYPE", TOK_END_TYPE},
                {"RETAIN", TOK_RETAIN}, {"WORD", TOK_WORD},
                {"DWORD", TOK_DWORD}, {"BYTE", TOK_BYTE},
                {"CONSTANT", TOK_CONSTANT}, {"DATE_AND_TIME", TOK_DATE_AND_TIME},
                {"UINT", TOK_UINT}, {"TIME", TOK_TIME},
            };
            auto it = keywords.find(ident);
            return (it != keywords.end()) ? it->second : TOK_IDENTIFIER;
        }

        Token parse_number() {
            int start_line = current_line;
            int start_col  = current_column;
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
            while (std::isdigit((unsigned char)peek()) || peek() == '.' || peek() == '#') {
                if (peek() == '.') {
                    if (is_real) break;
                    if (pos+1 < input.size() && input[pos+1] == '.') {
                        break;
                    }
                    is_real = true;
                    num.push_back(advance());
                }
                else if (peek() == '#') {
                    advance(); // consome '#'
                    if (num == "16") {
                        num.clear();
                        is_hex = true;
                    } else if (num == "8") {
                        num.clear();
                        is_oct = true;
                    } else if (num == "2") {
                        num.clear();
                        is_bin = true;
                    } else {
                        std::string msg = "Base inválida: " + num;
                        return Token(TOK_ERROR, msg, start_line, start_col);
                    }
                    break;
                }
                else {
                    num.push_back(advance());
                }
            }
            if (is_hex) {
                while (std::isxdigit((unsigned char)peek()) || peek() == '_') {
                    if (peek() == '_') { advance(); continue; }
                    num.push_back(advance());
                }
                if (num.empty()) {
                    std::string msg = "Número hexadecimal inválido (nenhum dígito após #)";
                    return Token(TOK_ERROR, msg, start_line, start_col);
                }
                if (negative) { num.insert(num.begin(), '-'); }
                return Token(TOK_HEX_LITERAL, num, start_line, start_col);
            }
            if (is_oct) {
                while ((peek() >= '0' && peek() <= '7') || peek() == '_') {
                    if (peek() == '_') { advance(); continue; }
                    num.push_back(advance());
                }
                if (num.empty()) {
                    std::string msg = "Número octal inválido (nenhum dígito após #)";
                    return Token(TOK_ERROR, msg, start_line, start_col);
                }
                if (negative) num.insert(num.begin(), '-');
                return Token(TOK_OCTAL_LITERAL, num, start_line, start_col);
            }
            if (is_bin) {
                while (peek() == '0' || peek() == '1') {
                    num.push_back(advance());
                }
                if (num.empty()) {
                    std::string msg = "Número binario inválido (nenhum dígito após #)";
                    return Token(TOK_ERROR, msg, start_line, start_col);
                }
                if (negative) num.insert(num.begin(), '-');
                return Token(TOK_BINARY_LITERAL, num, start_line, start_col);
            }
            bool read_any = !num.empty();
            if (!read_any) {
                std::string s = negative ? "-" : (positive ? "+" : "");
                return Token(TOK_INVALID, s, start_line, start_col);
            }
            if (negative) num.insert(num.begin(), '-');
            if (is_real) {
                return Token(TOK_REAL_LITERAL, num, start_line, start_col);
            }
            else {
                return Token(TOK_INT_LITERAL, num, start_line, start_col);
            }
        }

        Token parse_time_literal() {
            int start_line = current_line;
            int start_col  = current_column;
            advance(); // 'T'
            advance(); // '#'
            std::string literal = "T#";
            bool digit_found = false;
            while (std::isdigit((unsigned char)peek())) {
                digit_found = true;
                literal.push_back(advance());
            }
            if (!digit_found) {
                return Token(TOK_INVALID, "T# sem dígitos", start_line, start_col);
            }
            while (std::isalpha((unsigned char)peek())) {
                literal.push_back(advance());
            }
            return Token(TOK_TIME_LITERAL, literal, start_line, start_col);
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
                    advance();
                    break;
                }
                str.push_back(advance());
            }
            return Token(TOK_STRING_LITERAL, str, start_line, start_col);
        }

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

        Token parse_date_time_literal() {
            int start_line = current_line;
            int start_col  = current_column;
            // Consome os caracteres 'D', 'T' e '#'
            advance(); // Consome 'D'
            advance(); // Consome 'T'
            advance(); // Consome '#'
            std::string literal = "DT#";
            bool valid = false;
            // Aceita dígitos, hífens e dois pontos
            while (std::isdigit((unsigned char)peek()) || peek() == '-' || peek() == ':') {
                literal.push_back(advance());
                valid = true;
            }
            if (!valid) {
                return Token(TOK_INVALID, "DT# sem dígitos válidos", start_line, start_col);
            }
            return Token(TOK_TIME_LITERAL, literal, start_line, start_col);
        }



        Token next_token() {
        skip_whitespace();
        char c = peek();
        if (c == '\0') {
            return Token(TOK_EOF, "EOF", current_line, current_column);
        }
        if (c == '(' && (pos+1 < input.size()) && input[pos+1] == '*') {
            skip_comment();
            return next_token();
        }
        // NOVA VERIFICAÇÃO: se o literal iniciar com "DT#" (ignorando o case)
        if (std::toupper(c) == 'D' && pos + 2 < input.size() &&
            std::toupper(input[pos+1]) == 'T' && input[pos+2] == '#') {
            return parse_date_time_literal();
        }
        // Verificação para literais iniciados por "T#" (não "DT#")
        if ((c == 'T' || c == 't') && pos+1 < input.size() && input[pos+1] == '#') {
            return parse_time_literal();
        }
        // Agora verifica se é um identificador
        if (std::isalpha((unsigned char)c) || c == '_') {
            int start_line = current_line;
            int start_col  = current_column;
            std::string ident = parse_identifier();
            if (ident.empty()) {
                return Token(TOK_INVALID, std::string(1, advance()), start_line, start_col);
            }
            TokenId tid = classify_keyword(ident);
            return Token(tid, ident, start_line, start_col);
        }
        if (std::isdigit((unsigned char)c) || ((c == '-' || c == '+')
            && pos+1 < input.size() && std::isdigit((unsigned char)input[pos+1]))) {
            return parse_number();
        }
        if (c == '"' || c == '\'') {
            return parse_string(c);
        }
        if (c == '.') {
            return parse_range_or_dot();
        }
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
                advance();
                return Token(TOK_LPAREN, "(", start_line, start_col);
            case ')':
                advance();
                return Token(TOK_RPAREN, ")", start_line, start_col);
            case '[':
                advance();
                return Token(TOK_LBRACKET, "[", start_line, start_col);
            case ']':
                advance();
                return Token(TOK_RBRACKET, "]", start_line, start_col);
            case '+':
                advance();
                return Token(TOK_PLUS, "+", start_line, start_col);
            case '-':
                advance();
                return Token(TOK_MINUS, "-", start_line, start_col);
            case '*':
                advance();
                return Token(TOK_MULTIPLY, "*", start_line, start_col);
            case '/':
                advance();
                return Token(TOK_DIVIDE, "/", start_line, start_col);
            case '=':
                advance();
                return Token(TOK_IGUAL, "=", start_line, start_col);
            case '>':
                advance();
                if (peek() == '=') {
                    advance();
                    return Token(TOK_MAIOR_IGUAL, ">=", start_line, start_col);
                }
                return Token(TOK_MAIOR, ">", start_line, start_col);
            case '<':
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
            default:
                char inv = advance();
                return Token(TOK_INVALID, std::string(1, inv), start_line, start_col);
        }
    }


        size_t getPos() const { return pos; }
        const std::string& getInput() const { return input; }
    };

    // Tabela de símbolos simples
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
    // Estrutura para armazenar informações de tipos
    // ----------------------------------------------------
    enum UserTypeKind {
        UTYPE_BASE,
        UTYPE_ENUM,
        UTYPE_SUBRANGE,
        UTYPE_ARRAY,
        UTYPE_STRUCT,
        UTYPE_DERIVED
    };

    struct UserTypeInfo {
        UserTypeKind kind;
        std::string name;
        std::string baseName;
        double initValueNumeric;
        std::string initValueString;
        bool hasInitValue = false;
        std::vector<std::string> enumValues;
        std::string enumInit;
        long subrangeLow;
        long subrangeHigh;
        long arrayLow;
        long arrayHigh;
        std::string arrayElementType;
        struct Field {
            std::string name;
            std::string type;
            std::string initValue;
        };
        std::vector<Field> structFields;
        std::vector<std::pair<std::string, std::string>> derivedInits;
    };

    // ----------------------------------------------------
    // Tabela de tipos do usuário
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
        SymbolTable symtab;
        TypeTable typetab;
        std::vector<std::string> code;

        void parseProgram();
        void parseVarBlock();
        bool isVarBlockStart();
        bool isVarDeclarationStart();
        void parseVarDeclaration(bool isConstBlock, bool isRetainBlock);
        VarType mapBasicType(TokenId id);
        VarType parseTypeAndGetVarType();
        void parseStructDeclaration();
        void parseValue();
        void parseNumber();
        void parseInstructions();
        bool isInstructionStart();
        void parseInstruction();
        void parseAssignment();

        // Expressões para geração de IR
        std::string parseExpressionToIR();
        std::string parseTermToIR();
        std::string parseFactorToIR();
        std::string parseParenExpressionToIR();
        // Nova função para operadores bitwise
        std::string parseBitwiseExpressionToIR();

        // Condições
        void parseIfStatement();
        void parseCaseStatement();
        void parseForStatement();
        void parseWhileStatement();
        void parseRepeatStatement();
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
            symtab.declareVar("DT", TYPE_DATE_AND_TIME);
        }
        void parse();
    };

    // Avança para o próximo token
    void Parser::advance() {
        current_token = lexer.next_token();
    }

    void Parser::parser_error(const Token &tok, const std::string &msg) {
        std::cerr << "Erro de sintaxe na linha " << tok.line << ", coluna " << tok.column
                  << ": " << msg << " (encontrado: " << tok.value << ")\n";
        const std::string &input = lexer.getInput();
        size_t filePos = lexer.getPos();
        size_t line_start = input.rfind('\n', filePos);
        if (line_start == std::string::npos) { line_start = 0; } else { line_start++; }
        size_t line_end = input.find('\n', filePos);
        if (line_end == std::string::npos) { line_end = input.size(); }
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
            case TOK_INT:           return TYPE_INT;
            case TOK_UINT:          return TYPE_UINT;
            case TOK_REAL:          return TYPE_REAL;
            case TOK_BYTE:          return TYPE_BYTE;
            case TOK_BOOL:          return TYPE_BOOL;
            case TOK_STRING:        return TYPE_STRING;
            case TOK_DATE_AND_TIME: return TYPE_DATE_AND_TIME;
            case TOK_TIME:          return TYPE_TIME;
            case TOK_DWORD:         return TYPE_DWORD;
            default:                return TYPE_UNKNOWN;
        }
    }

    VarType Parser::parseTypeAndGetVarType() {
        if (check(TOK_STRING)) {
            VarType t = TYPE_STRING;
            advance();
            if (check(TOK_LBRACKET)) {
                advance();
                if (!check(TOK_INT_LITERAL)) {
                    parser_error(current_token, "Dimensão de STRING esperada (STRING[50])");
                }
                int dim = std::stoi(current_token.value);
                advance();
                expect(TOK_RBRACKET);
            }
            return t;
        }
        else if (check(TOK_TIME)) {
            advance();
            return TYPE_TIME;
        }
        else if (check(TOK_BOOL) || check(TOK_INT) || check(TOK_REAL) || check(TOK_BYTE)
              || check(TOK_DATE_AND_TIME) || check(TOK_DWORD) || check(TOK_WORD)
              || check(TOK_CONSTANT) || check(TOK_UINT))
        {
            VarType t = mapBasicType(current_token.id);
            advance();
            return t;
        }
        else if (check(TOK_ARRAY)) {
            advance();
            expect(TOK_LBRACKET);
            parseNumber();
            expect(TOK_DOT_DOT);
            parseNumber();
            expect(TOK_RBRACKET);
            expect(TOK_OF);
            VarType elemType = parseTypeAndGetVarType();
            if (elemType == TYPE_INT)    return TYPE_ARRAY_INT;
            if (elemType == TYPE_UINT)   return TYPE_ARRAY_UINT;
            if (elemType == TYPE_REAL)   return TYPE_ARRAY_REAL;
            if (elemType == TYPE_BOOL)   return TYPE_ARRAY_BOOL;
            if (elemType == TYPE_STRING) return TYPE_ARRAY_STRING;
            return TYPE_UNKNOWN;
        }
        else if (check(TOK_STRUCT)) {
            advance();
            while (check(TOK_IDENTIFIER)) {
                parseStructDeclaration();
            }
            expect(TOK_END_STRUCT);
            return TYPE_UNKNOWN;
        }
        else {
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
        if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL) ||
            check(TOK_HEX_LITERAL) || check(TOK_STRING_LITERAL) ||
            check(TOK_TRUE) || check(TOK_FALSE) ||
            check(TOK_OCTAL_LITERAL) || check(TOK_BINARY_LITERAL) ||
            check(TOK_UINT_LITERAL) || check(TOK_TIME_LITERAL))
        {
            advance();
        } else {
            parser_error(current_token, "Valor inválido na atribuição");
        }
    }

    void Parser::parseNumber() {
        if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL) || check(TOK_HEX_LITERAL)
         || check(TOK_OCTAL_LITERAL) || check(TOK_BINARY_LITERAL)) {
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

    void Parser::parseVarDeclaration(bool isConstBlock, bool isRetainBlock) {
        Token varName = current_token;
        expect(TOK_IDENTIFIER);
        expect(TOK_COLON);
        VarType varType = parseTypeAndGetVarType();
        bool initialized = false;
        if (check(TOK_ATRIBUICAO)) {
            advance();
            if (check(TOK_LBRACKET)) {
                // array init (não implementado neste exemplo)
            } else {
                Token lit = current_token;
                if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL)
                 || check(TOK_STRING_LITERAL) || check(TOK_TRUE) || check(TOK_FALSE)
                 || check(TOK_HEX_LITERAL) || check(TOK_OCTAL_LITERAL) || check(TOK_BINARY_LITERAL)
                 || check(TOK_UINT_LITERAL) || check(TOK_TIME_LITERAL))
                {
                    advance();
                    std::string temp = newTemp();
                    code.push_back("LOAD_IMM " + lit.value + " " + temp);
                    code.push_back("STORE " + temp + " " + varName.value);
                    initialized = true;
                } else {
                    parser_error(current_token, "Valor inválido na atribuição");
                }
            }
        }
        expect(TOK_SEMICOLON);
        symtab.declareVar(varName.value, varType);
        if (isConstBlock) {
            std::cout << "Variável/Constante '" << varName.value << "' declarada como CONSTANT.\n";
        }
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
        return check(TOK_IDENTIFIER)
            || check(TOK_IF)
            || check(TOK_FOR)
            || check(TOK_WHILE)
            || check(TOK_CASE)
            || check(TOK_REPEAT);
    }

    void Parser::parseInstructions() {
        while (isInstructionStart()) {
            parseInstruction();
        }
    }

    void Parser::parseInstruction() {
        if (check(TOK_IDENTIFIER)) {
            parseAssignment();
        }
        else if (check(TOK_IF)) {
            parseIfStatement();
        }
        else if (check(TOK_CASE)) {
            parseCaseStatement();
        }
        else if (check(TOK_FOR)) {
            parseForStatement();
        }
        else if (check(TOK_WHILE)) {
            parseWhileStatement();
        }
        else if (check(TOK_REPEAT)) {
            parseRepeatStatement();
        }
        else {
            parser_error(current_token, "Instrução inválida");
        }
    }

    // ==================================================
    // Nova função para parsear expressões com operadores bitwise
    // Suporta os exemplos:
    //   xBitwise := xBitwise AND 16#F0;
    //   xBitwise := xBitwise OR 16#0F;
    //   xBitwise := NOT xBitwise;
    // ==================================================
    std::string Parser::parseBitwiseExpressionToIR() {
        if (check(TOK_NOT)) {
            advance();
            std::string operand = parseBitwiseExpressionToIR();
            std::string temp = newTemp();
            code.push_back("BITWISE_NOT " + operand + " " + temp);
            return temp;
        }
        std::string left = parseExpressionToIR();
        while (check(TOK_AND) || check(TOK_OR)) {
            Token op = current_token;
            advance();
            std::string right = parseExpressionToIR();
            std::string temp = newTemp();
            if (op.id == TOK_AND) {
                code.push_back("BITWISE_AND " + left + " " + right + " " + temp);
            } else if (op.id == TOK_OR) {
                code.push_back("BITWISE_OR " + left + " " + right + " " + temp);
            }
            left = temp;
        }
        return left;
    }

    void Parser::parseAssignment() {
        Token varName = current_token;
        expect(TOK_IDENTIFIER);
        std::vector<std::string> indices;
        while (check(TOK_LBRACKET)) {
            advance();
            std::string idxIR = parseExpressionToIR();
            expect(TOK_RBRACKET);
            indices.push_back(idxIR);
        }
        expect(TOK_ATRIBUICAO);
        std::string rhs = parseBitwiseExpressionToIR();
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
        if (check(TOK_TIME_LITERAL)) {
            Token lit = current_token;
            advance();
            std::string temp = newTemp();
            code.push_back("LOAD_TIME " + lit.value + " " + temp);
            return temp;
        }
        while (check(TOK_MULTIPLY) || check(TOK_DIVIDE) || check(TOK_MOD)) {
            Token op = current_token;
            advance();
            std::string right = parseFactorToIR();
            std::string temp = newTemp();
            std::string instr = (op.id == TOK_MULTIPLY) ? "MUL" : (op.id == TOK_DIVIDE) ? "DIV" : "MOD";
            code.push_back(instr + " " + left + " " + right + " " + temp);
            left = temp;
        }
        return left;
    }

    // tanto aritméticas quanto condições (relacionais).
    std::string Parser::parseParenExpressionToIR() {
        expect(TOK_LPAREN); // Consome '('
        // Parseia a expressão aritmética inicial
        std::string left = parseExpressionToIR();
        // Se houver um operador relacional, processa-o junto com o lado direito
        while (check(TOK_IGUAL) || check(TOK_DIFERENTE) ||
               check(TOK_MENOR) || check(TOK_MENOR_IGUAL) ||
               check(TOK_MAIOR) || check(TOK_MAIOR_IGUAL)) {
            Token op = current_token;
            advance(); // Consome o operador relacional
            std::string right = parseExpressionToIR();
            std::string temp = newTemp();
            std::string instr;
            switch(op.id) {
                case TOK_IGUAL:       instr = "COMP_EQ"; break;
                case TOK_DIFERENTE:   instr = "COMP_NE"; break;
                case TOK_MENOR:       instr = "COMP_LT"; break;
                case TOK_MENOR_IGUAL: instr = "COMP_LE"; break;
                case TOK_MAIOR:       instr = "COMP_GT"; break;
                case TOK_MAIOR_IGUAL: instr = "COMP_GE"; break;
                default:              instr = "COMP_UNKNOWN"; break;
            }
            code.push_back(instr + " " + left + " " + right + " " + temp);
            left = temp;
               }
        expect(TOK_RPAREN); // Consome ')'
        return left;
    }

    std::string Parser::parseFactorToIR() {
        if (check(TOK_LPAREN)) {
            return parseParenExpressionToIR();
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
                if (varType != TYPE_ARRAY_INT && varType != TYPE_ARRAY_REAL &&
                    varType != TYPE_ARRAY_BOOL && varType != TYPE_ARRAY_STRING) {
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
        // Novo tratamento para TOK_TIME_LITERAL (incluindo DT# e T#)
        else if (check(TOK_TIME_LITERAL)) {
            Token lit = current_token;
            advance();
            std::string temp = newTemp();
            code.push_back("LOAD_TIME " + lit.value + " " + temp);
            return temp;
        }
        else if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL) ||
                 check(TOK_HEX_LITERAL) || check(TOK_OCTAL_LITERAL) || check(TOK_BINARY_LITERAL)) {
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


    void Parser::parseForStatement() {
        expect(TOK_FOR);
        if (!check(TOK_IDENTIFIER)) {
            parser_error(current_token, "Identificador esperado após FOR");
        }
        Token loopVar = current_token;
        advance();
        expect(TOK_ATRIBUICAO);
        std::string initExpr = parseExpressionToIR();
        expect(TOK_TO);
        std::string finalExpr = parseExpressionToIR();
        std::string byExpr;
        if (check(TOK_BY)) {
            advance();
            byExpr = parseExpressionToIR();
        }
        expect(TOK_DO);
        parseInstructions();
        expect(TOK_END_FOR);
    }

    void Parser::parseWhileStatement() {
        expect(TOK_WHILE);
        std::string condIR = parseConditionToIR();
        expect(TOK_DO);
        parseInstructions();
        expect(TOK_END_WHILE);
    }

    void Parser::parseRepeatStatement() {
        expect(TOK_REPEAT);       // Consome o REPEAT
        // Parseia as instruções do bloco REPEAT
        while (!check(TOK_UNTIL)) {
            parseInstruction();
        }
        expect(TOK_UNTIL);        // Consome o UNTIL
        std::string cond = parseConditionToIR();  // Parse da condição
        expect(TOK_END_REPEAT);   // Consome o END_REPEAT
        // Geração simples de IR para o laço Repeat (pode ser aprimorada)
        code.push_back("REPEAT_LOOP " + cond);
    }


    // ==================================================
    // Modificação na função parseIfStatement para suportar ELSIF
    // ==================================================
    void Parser::parseIfStatement() {
        expect(TOK_IF);
        std::string cond = parseConditionToIR();
        expect(TOK_THEN);
        parseInstructions();
        // Suporte a cláusulas ELSIF
        while (check(TOK_ELSIF)) {
             advance(); // Consome 'ELSIF'
             std::string elsifCond = parseConditionToIR();
             expect(TOK_THEN);
             parseInstructions();
        }
        if (check(TOK_ELSE)) {
             advance();
             parseInstructions();
        }
        expect(TOK_END_IF);
    }

    void Parser::parseCaseStatement() {
        expect(TOK_CASE);
        std::string caseExpr = parseConditionToIR();
        expect(TOK_OF);
        while (check(TOK_INT_LITERAL) || check(TOK_IDENTIFIER)) {
            std::string caseLabel;
            if (check(TOK_INT_LITERAL) || check(TOK_IDENTIFIER)) {
                caseLabel = current_token.value;
                advance();
            }
            if (check(TOK_DOT_DOT)) {
                advance();
                if (!check(TOK_INT_LITERAL)) {
                    parser_error(current_token, "Esperado literal após '..' no label do case");
                }
                caseLabel += ".." + current_token.value;
                advance();
            }
            expect(TOK_COLON);
            parseInstructions();
        }
        if (check(TOK_ELSE)) {
             advance();
             parseInstructions();
        }
        expect(TOK_END_CASE);
    }


    // Alterado para usar parseTermToIR() em vez de parseExpressionToIR()
    // para os operandos, evitando que o token "=" seja erroneamente interpretado
    // como um fator.
    std::string Parser::parseComparisonToIR() {
        std::string left = parseTermToIR();
        if (check(TOK_IGUAL) || check(TOK_DIFERENTE) ||
            check(TOK_MENOR) || check(TOK_MENOR_IGUAL) ||
            check(TOK_MAIOR) || check(TOK_MAIOR_IGUAL))
        {
            Token op = current_token;
            parseRelationalOperator();   // Consome e valida o operador relacional
            std::string right = parseTermToIR(); // Parseia o operando direito utilizando parseTermToIR()
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
            code.push_back(instr + " " + left + " " + right + " " + temp);
            return temp;
        }
        return left;
    }


    std::string Parser::parseConditionToIR() {
        // Suporta NOT como operador unário lógico em condições
        if (check(TOK_NOT)) {
            advance();
            std::string cond = parseConditionToIR(); // Parse recursivo para a condição negada
            std::string temp = newTemp();
            code.push_back("LOGICAL_NOT " + cond + " " + temp);
            return temp;
        }
        // Caso contrário, parseia uma comparação
        std::string left = parseComparisonToIR();
        while (check(TOK_AND) || check(TOK_OR)) {
            Token op = current_token;
            advance();
            std::string right = parseComparisonToIR();
            std::string temp = newTemp();
            if (op.id == TOK_AND) {
                code.push_back("LOGICAL_AND " + left + " " + right + " " + temp);
            } else {
                code.push_back("LOGICAL_OR " + left + " " + right + " " + temp);
            }
            left = temp;
        }
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
        while (check(TOK_TYPE)) {
            parseTypeBlock();
        }
        while (isVarBlockStart()) {
            parseVarBlock();
        }
        parseInstructions();
        expect(TOK_END_PROGRAM);
    }

    void Parser::parseTypeBlock() {
        expect(TOK_TYPE);
        while (!check(TOK_END_TYPE) && current_token.id != TOK_EOF) {
            parseTypeDeclaration();
        }
        expect(TOK_END_TYPE);
    }

    void Parser::parseVarBlock() {
        expect(TOK_VAR);
        bool isConstBlock = false;
        bool isRetainBlock = false;
        if (check(TOK_CONSTANT)) {
            isConstBlock = true;
            advance();
        }
        if (check(TOK_RETAIN)) {
            isRetainBlock = true;
            advance();
        }
        while (isVarDeclarationStart()) {
            parseVarDeclaration(isConstBlock, isRetainBlock);
        }
        expect(TOK_END_VAR);
    }

    void Parser::parseTypeDeclaration() {
        if (!check(TOK_IDENTIFIER)) {
            parser_error(current_token, "Nome de tipo esperado");
        }
        std::string typeName = current_token.value;
        advance();
        expect(TOK_COLON);
        UserTypeInfo info;
        info.name = typeName;
        info.kind = UTYPE_BASE;
        if (check(TOK_LPAREN)) {
            advance();
            if (check(TOK_INT_LITERAL) || check(TOK_MINUS)) {
                parser_error(current_token, "Sintaxe de subrange incompleta (falta base type antes do '(').");
            }
            else {
                info.kind = UTYPE_ENUM;
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
        else if (check(TOK_INT) || check(TOK_REAL) || check(TOK_BOOL) || check(TOK_STRING) || check(TOK_BYTE)
            || check(TOK_UINT)) {
            info.baseName = current_token.value;
            info.kind = UTYPE_BASE;
            advance();
            if (check(TOK_LPAREN)) {
                info.kind = UTYPE_SUBRANGE;
                advance();
                if (!check(TOK_INT_LITERAL) && !check(TOK_MINUS)) {
                    parser_error(current_token, "Valor inteiro esperado para limite inferior do subrange");
                }
                bool neg = false;
                long lower = 0;
                if (check(TOK_MINUS)) { neg = true; advance(); }
                if (!check(TOK_INT_LITERAL)) { parser_error(current_token, "Valor inteiro esperado apos '-'"); }
                lower = std::stol(current_token.value);
                if (neg) lower = -lower;
                advance();
                expect(TOK_DOT_DOT);
                neg = false;
                long upper = 0;
                if (check(TOK_MINUS)) { neg = true; advance(); }
                if (!check(TOK_INT_LITERAL)) { parser_error(current_token, "Valor inteiro esperado para limite superior do subrange"); }
                upper = std::stol(current_token.value);
                if (neg) upper = -upper;
                advance();
                expect(TOK_RPAREN);
                info.subrangeLow = lower;
                info.subrangeHigh = upper;
            }
        }
        else if (check(TOK_ARRAY)) {
            info.kind = UTYPE_ARRAY;
            advance();
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
            if (!check(TOK_IDENTIFIER) &&
                !check(TOK_INT) && !check(TOK_REAL) && !check(TOK_BOOL) && !check(TOK_STRING) && !check(TOK_BYTE) &&
                !check(TOK_UINT)) {
                parser_error(current_token, "Tipo esperado após OF");
            }
            std::string arrType = current_token.value;
            advance();
            info.arrayLow = low;
            info.arrayHigh = high;
            info.arrayElementType = arrType;
        }
        else if (check(TOK_STRUCT)) {
            info.kind = UTYPE_STRUCT;
            advance();
            while (!check(TOK_END_STRUCT)) {
                if (!check(TOK_IDENTIFIER)) {
                    parser_error(current_token, "Nome de campo esperado");
                }
                UserTypeInfo::Field f;
                f.name = current_token.value;
                advance();
                expect(TOK_COLON);
                if (!check(TOK_IDENTIFIER) &&
                    !check(TOK_INT) && !check(TOK_REAL) && !check(TOK_BOOL) && !check(TOK_STRING) &&
                    !check(TOK_BYTE) && !check(TOK_UINT)) {
                    parser_error(current_token, "Tipo de campo esperado");
                }
                f.type = current_token.value;
                advance();
                if (check(TOK_ATRIBUICAO)) {
                    advance();
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
            info.kind = UTYPE_DERIVED;
            info.baseName = current_token.value;
            advance();
        }
        else {
            parser_error(current_token, "Tipo de dado não reconhecido");
        }
        if (check(TOK_ATRIBUICAO)) {
            advance();
            if (info.kind == UTYPE_DERIVED && check(TOK_LPAREN)) {
                advance();
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
            else if (info.kind == UTYPE_BASE || info.kind == UTYPE_SUBRANGE) {
                if (!check(TOK_INT_LITERAL) && !check(TOK_REAL_LITERAL)
                    && !check(TOK_STRING_LITERAL) && !check(TOK_TRUE) && !check(TOK_FALSE)) {
                    parser_error(current_token, "Valor literal inválido para init do tipo base/subrange");
                }
                info.hasInitValue = true;
                info.initValueString = current_token.value;
                if (check(TOK_INT_LITERAL) || check(TOK_REAL_LITERAL)) {
                    info.initValueNumeric = std::stod(current_token.value);
                }
                advance();
            }
            else if (info.kind == UTYPE_ENUM) {
                if (!check(TOK_IDENTIFIER)) {
                    parser_error(current_token, "Enumerador esperado para init do enum");
                }
                info.enumInit = current_token.value;
                advance();
            }
            else if (info.kind == UTYPE_ARRAY) {
                if (!check(TOK_LBRACKET)) {
                    parser_error(current_token, "Esperado '[' para init de array");
                }
            }
        }
        expect(TOK_SEMICOLON);
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
        std::string nomeArquivo = (argc > 1) ? argv[1]
                                             : "/home/kaynan/Documentos/Desenvolvimento/C++/Compilador-st-am/teste-1.st";
        std::ifstream file(nomeArquivo);
        if (!file) {
            std::cerr << "Não foi possível abrir o arquivo: " << nomeArquivo << "\n";
            return 1;
        }
        std::string code((std::istreambuf_iterator<char>(file)),
                         std::istreambuf_iterator<char>());
        Lexer lexer(code);
        {
            Token token = lexer.next_token();
            while (token.id != TOK_EOF) {
                std::cout << token.to_string() << "\n";
                token = lexer.next_token();
            }
        }
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

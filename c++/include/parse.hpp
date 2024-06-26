#include <string>
#include <vector>
#include <memory>
#include <stdexcept>
#include <cctype>

enum class ExprType {
    Var,
    IntLit,
    StringLit,
    App,
    Lambda,
    Let
};

struct Expr {
    ExprType type;
    std::string value;
    std::vector<std::unique_ptr<Expr>> children;

    Expr(ExprType t) : type(t) {}
};

struct Decl {
    std::string name;
    std::vector<std::string> params;
    std::unique_ptr<Expr> body;
};

struct Script {
    std::vector<Decl> declarations;
};

class Parser {
    std::string input;
    size_t pos = 0;

public:
    Parser(const std::string& input) : input(input) {}

    Script parse() {
        Script script;
        while (pos < input.length()) {
            script.declarations.push_back(parseDecl());
        }
        return script;
    }

private:
    Decl parseDecl() {
        Decl decl;
        decl.name = parseIdentifier();
        while (peek() != '=') {
            decl.params.push_back(parseIdentifier());
        }
        consume('=');
        decl.body = parseExpr();
        consume(';');
        return decl;
    }

    std::unique_ptr<Expr> parseExpr() {
        if (std::isalpha(peek())) {
            auto expr = std::make_unique<Expr>(ExprType::Var);
            expr->value = parseIdentifier();
            return expr;
        }
        throw std::runtime_error("Unsupported expression");
    }

    std::string parseIdentifier() {
        std::string ident;
        while (pos < input.length() && std::isalpha(input[pos])) {
            ident += input[pos++];
        }
        if (ident.empty()) {
            throw std::runtime_error("Expected identifier");
        }
        return ident;
    }

    char peek() const {
        return pos < input.length() ? input[pos] : '\0';
    }

    void consume(char expected) {
        if (peek() != expected) {
            throw std::runtime_error("Unexpected character");
        }
        pos++;
    }
};
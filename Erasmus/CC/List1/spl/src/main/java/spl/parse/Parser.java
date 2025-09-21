package spl.parse;

import java.util.ArrayList;
import java.util.List;

import spl.Spl;
import spl.scan.Token;

public class Parser {
    private static class ParseError extends RuntimeException {}

    private final List<Token> tokens;
    private int current = 0;

    public Parser(List<Token> tokens) { this.tokens = tokens; }

    public List<Declaration> parse() {
        List<Declaration> statements = new ArrayList<>();
        while (!isAtEnd()) {
            statements.add(declaration());
        }
        return statements;
    }

    private Declaration declaration() {
        try {
            if (match(Token.Type.VAR)) return varDeclaration();
            return statement();
        } catch (ParseError error) {
            synchronize();
            return null;
        }
    }

    private Declaration varDeclaration() {
        Token name = consume(Token.Type.ID, "Expect variable name.");
        Expression initializer = null;
        if (match(Token.Type.EQUAL)) initializer = expression();
        consume(Token.Type.SEMICOLON, "Expect ';' after variable declaration.");
        return new Declaration.Var(name, initializer);
    }

    private Statement statement() {
        if (match(Token.Type.PRINT)) return printStmt();
        if (match(Token.Type.LEFT_BRACE)) return new Statement.Block(block());
        if (match(Token.Type.IF)) return ifStmt();
        if (match(Token.Type.WHILE)) return whileStmt();
        return expressionStmt();
    }

    private Statement ifStmt() {
        consume(Token.Type.LEFT_PAREN, "Expect '(' after 'if'.");
        Expression condition = expression();
        consume(Token.Type.RIGHT_PAREN, "Expect ')' after if condition.");
        Statement thenBranch = statement();
        Statement elseBranch = null;
        if (match(Token.Type.ELSE)) elseBranch = statement();
        return new Statement.If(condition, thenBranch, elseBranch);
    }

    private Statement whileStmt() {
        consume(Token.Type.LEFT_PAREN, "Expect '(' after 'while'.");
        Expression condition = expression();
        consume(Token.Type.RIGHT_PAREN, "Expect ')' after condition.");
        Statement body = statement();
        return new Statement.While(condition, body);
    }

    private Statement printStmt() {
        Expression value = expression();
        consume(Token.Type.SEMICOLON, "Expect ';' after value.");
        return new Statement.Print(value);
    }

    private List<Declaration> block() {
        List<Declaration> statements = new ArrayList<>();
        while (!check(Token.Type.RIGHT_BRACE) && !isAtEnd()) {
            statements.add(declaration());
        }
        consume(Token.Type.RIGHT_BRACE, "Expect '}' after block.");
        return statements;
    }

    private Statement expressionStmt() {
        Expression expr = expression();
        consume(Token.Type.SEMICOLON, "Expect ';' after expression.");
        return expr;
    }

    // Expressions: assignment -> or
    private Expression expression() { return assignment(); }

    private Expression assignment() {
        Expression expr = or();
        if (match(Token.Type.EQUAL)) {
            Token equals = previous();
            Expression value = assignment(); // right-associative
            if (expr instanceof Expression.Variable) {
                Token name = ((Expression.Variable)expr).name;
                return new Expression.Assign(name, value);
            }
            // Error: invalid assignment target
            throw error(equals, "Invalid assignment target.");
        }
        return expr;
    }

    private Expression or() {
        Expression expr = and();
        while (match(Token.Type.OR)) {
            Token operator = previous();
            Expression right = and();
            expr = new Expression.Logical(expr, operator, right);
        }
        return expr;
    }

    private Expression and() {
        Expression expr = equality();
        while (match(Token.Type.AND)) {
            Token operator = previous();
            Expression right = equality();
            expr = new Expression.Logical(expr, operator, right);
        }
        return expr;
    }

    private Expression equality() {
        Expression expr = comparison();
        while (match(Token.Type.NOT_EQUAL, Token.Type.EQUAL_EQUAL)) {
            Token op = previous();
            Expression right = comparison();
            expr = new Expression.Binary(expr, op, right);
        }
        return expr;
    }

    private Expression comparison() {
        Expression expr = term();
        while (match(Token.Type.GREATER_THAN, Token.Type.GREATER_EQUAL, Token.Type.LESSER_THAN, Token.Type.LESSER_EQUAL)) {
            Token op = previous();
            Expression right = term();
            expr = new Expression.Binary(expr, op, right);
        }
        return expr;
    }

    private Expression term() {
        Expression expr = factor();
        while (match(Token.Type.MINUS, Token.Type.PLUS)) {
            Token op = previous();
            Expression right = factor();
            expr = new Expression.Binary(expr, op, right);
        }
        return expr;
    }

    private Expression factor() {
        Expression expr = unary();
        while (match(Token.Type.SLASH, Token.Type.STAR)) {
            Token op = previous();
            Expression right = unary();
            expr = new Expression.Binary(expr, op, right);
        }
        return expr;
    }

    private Expression unary() {
        if (match(Token.Type.NOT, Token.Type.MINUS)) {
            Token op = previous();
            Expression right = unary();
            return new Expression.Unary(op, right);
        }
        return primary();
    }

    private Expression primary() {
        if (match(Token.Type.FALSE)) return new Expression.Literal(false);
        if (match(Token.Type.TRUE)) return new Expression.Literal(true);
        if (match(Token.Type.INT, Token.Type.FLOAT, Token.Type.STRING)) return new Expression.Literal(previous().literal);
        if (match(Token.Type.ID)) return new Expression.Variable(previous());
        if (match(Token.Type.LEFT_PAREN)) {
            Expression expr = expression();
            consume(Token.Type.RIGHT_PAREN, "Expected ')' after expression.");
            return expr;
        }
        throw error(peek(), "Expect expression.");
    }

    // Helpers
    private boolean match(Token.Type... types) {
        for (Token.Type t : types) {
            if (check(t)) { advance(); return true; }
        }
        return false;
    }

    private boolean check(Token.Type type) {
        if (isAtEnd()) return false;
        return peek().type == type;
    }

    private Token advance() { if (!isAtEnd()) current++; return previous(); }
    private boolean isAtEnd() { return peek().type == Token.Type.EOF; }
    private Token peek() { return tokens.get(current); }
    private Token previous() { return tokens.get(current > 0 ? current - 1 : 0); }

    private Token consume(Token.Type type, String message) {
        if (check(type)) return advance();
        throw error(peek(), message);
    }

    private ParseError error(Token token, String message) {
        Spl.error(token.line, "Parse error at '" + token.lexeme + "': " + message);
        return new ParseError();
    }

    private void synchronize() {
        while (!isAtEnd()) {
            if (match(Token.Type.SEMICOLON)) return;
            advance();
        }
    }
}


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
        Token name = consume(Token.Type.ID, "Expected variable name.");
        Expression initializer = null;
        if (match(Token.Type.EQUAL)) initializer = expression();
        consume(Token.Type.SEMICOLON, "Expected ';' after variable declaration.");
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
        consume(Token.Type.LEFT_PAREN, "Expected '(' after 'if'.");
        Expression condition = expression();
        consume(Token.Type.RIGHT_PAREN, "Expected ')' after condition.");
        Statement thenBranch = statement();
        Statement elseBranch = null;
        if (match(Token.Type.ELSE)) elseBranch = statement();
        return new Statement.If(condition, thenBranch, elseBranch);
    }

    private Statement whileStmt() {
        consume(Token.Type.LEFT_PAREN, "Expected '(' after 'while'.");
        Expression condition = expression();
        consume(Token.Type.RIGHT_PAREN, "Expected ')' after condition.");
        Statement body = statement();
        return new Statement.While(condition, body);
    }

    private Statement printStmt() {
        Expression value = expression();
        consume(Token.Type.SEMICOLON, "Expected ';' after value.");
        return new Statement.Print(value);
    }

    private List<Declaration> block() {
        List<Declaration> declarations = new ArrayList<>();
        while (!check(Token.Type.RIGHT_BRACE) && !isAtEnd()) {
            declarations.add(declaration());
        }
        consume(Token.Type.RIGHT_BRACE, "Expected '}' after block.");
        return declarations;
    }

    private Statement expressionStmt() {
        Expression expr = expression();
        consume(Token.Type.SEMICOLON, "Expected ';' after expression.");
        return expr;
    }

    // Expressions: assignment -> or
    private Expression expression() { return assignment(); }

    private Expression assignment() {
        if (checkAhead(Token.Type.EQUAL)) {
            Token name = consume(Token.Type.ID, "Expected identifier as left operand of assignment.");
            advance();
            Expression value = assignment();
            return new Expression.Assign(name, value);
        }
        return or();
    }

    private Expression or() {
        Expression expr = and();
        while (check(Token.Type.OR)) {
            Token operator = advance();
            Expression right = and();
            expr = new Expression.Logical(expr, operator, right);
        }
        return expr;
    }

    private Expression and() {
        Expression expr = equality();
        while (check(Token.Type.AND)) {
            Token operator = advance();
            Expression right = equality();
            expr = new Expression.Logical(expr, operator, right);
        }
        return expr;
    }

    private Expression equality() {
        Expression expr = comparison();
        while (check(Token.Type.NOT_EQUAL, Token.Type.EQUAL_EQUAL)) {
            Token op = advance();
            Expression right = comparison();
            expr = new Expression.Logical(expr, op, right);
        }
        return expr;
    }

    private Expression comparison() {
        Expression expr = term();
        while (check(Token.Type.GREATER_THAN, Token.Type.GREATER_EQUAL, Token.Type.LESSER_THAN, Token.Type.LESSER_EQUAL)) {
            Token op = advance();
            Expression right = term();
            expr = new Expression.Logical(expr, op, right);
        }
        return expr;
    }

    private Expression term() {
        Expression expr = factor();
        while (check(Token.Type.MINUS, Token.Type.PLUS)) {
            Token op = advance();
            Expression right = factor();
            expr = new Expression.Binary(expr, op, right);
        }
        return expr;
    }

    private Expression factor() {
        Expression expr = unary();
        while (check(Token.Type.SLASH, Token.Type.STAR)) {
            Token op = advance();
            Expression right = unary();
            expr = new Expression.Binary(expr, op, right);
        }
        return expr;
    }

    private Expression unary() {
        if (check(Token.Type.NOT, Token.Type.MINUS)) {
            Token op = advance();
            Expression right = unary();
            return new Expression.Unary(op, right);
        }
        return primary();
    }

    private Expression primary() {
        if (check(Token.Type.INT, Token.Type.FLOAT, Token.Type.STRING)) return new Expression.Literal(advance().literal);
        if (check(Token.Type.ID)) return new Expression.Variable(advance());
        if (match(Token.Type.FALSE)) return new Expression.Literal(false);
        if (match(Token.Type.TRUE)) return new Expression.Literal(true);
        consume(Token.Type.LEFT_PAREN, "Expected expression.");
        Expression expr = expression();
        consume(Token.Type.RIGHT_PAREN, "Expected ')' after expression.");
        return expr;
    }

    private boolean match(Token.Type type) {
        if (check(type)) {
            advance();
            return true;
        }
        return false;
    }

    private boolean check(Token.Type... types) {
        for (Token.Type type : types) {
            if (peek().type == type) return true;
        }
        return false;
    }

    private boolean checkAhead(Token.Type type) {
        return peekAhead().type == type;
    }

    private Token advance() {
        return tokens.get(current++);
    }

    private boolean isAtEnd() {
        return peek().type == Token.Type.EOF;
    }

    private Token peek() {
        return tokens.get(current);
    }

    private Token peekAhead() {
        if (isAtEnd()) return peek();
        return tokens.get(current + 1);
    }

    private Token consume(Token.Type type, String message) throws ParseError {
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


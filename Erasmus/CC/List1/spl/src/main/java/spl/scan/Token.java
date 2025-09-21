package spl.scan;

/**
 * The class Token stores the essential data for a token object and
 * provides a custom toString() method.
 * 
 * @author Compiler Construction
 * @version Efterar 2025
 * 
 */
public class Token {
	public final Token.Type type;
	public final String lexeme;
	public final Object literal;
	public final int line;

	public Token(Token.Type type, String lexeme, Object literal, int line) {
		this.type = type;
		this.lexeme = lexeme;
		this.literal = literal;
		this.line = line;
	}

	public String toString() {
		return "<" + type + "," + lexeme + "> " + "Literal: " + literal + ", Line: " + line;
	}

	public static enum Type {
		// Single-character tokens
		LEFT_PAREN, 
		RIGHT_PAREN,
		LEFT_BRACE,
		RIGHT_BRACE,
		SEMICOLON,
		PLUS,
		MINUS,
		STAR,
		// One or two character tokens
		NOT,
		NOT_EQUAL,
		EQUAL,
		EQUAL_EQUAL,
		LESSER_THAN,
		GREATER_THAN,
		LESSER_EQUAL,
		GREATER_EQUAL,
		SLASH,
		AND,
		OR,
		// Literals
		INT,
		FLOAT,
		STRING,
		CHAR,
		// Keywords
		VAR,
		IF,
		ELSE,
		WHILE,
		PRINT,
		TRUE,
		FALSE,
		// Identifiers
		ID,
		// End-of-file
		EOF
	}
}

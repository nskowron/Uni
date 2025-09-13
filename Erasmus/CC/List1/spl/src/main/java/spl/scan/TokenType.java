package spl.scan;

public enum TokenType {
	// Single-character tokens
	LEFT_PAREN, 
	RIGHT_PAREN,
	LEFT_BRACE,
	RIGHT_BRACE,
	SEMICOLON,
	// One or two character tokens
	NOT,
	NOT_EQUAL,
	EQUAL,
	EQUAL_EQUAL,
	LESSER_THAN,
	GREATER_THAN,
	LESSER_EQUAL,
	GREATER_EQUAL,
	PLUS,
	MINUS,
	STAR,
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

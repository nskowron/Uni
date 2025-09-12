package spl.scan;

import static spl.scan.TokenType.*;

import spl.Spl;

import java.util.LinkedList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implements a hand-written scanner with error handling and character-wise reading. 
 * 
 * @author Compiler Construction
 * @version Efterar 2025
 * 
 */
public class Scanner {
	// Keyword-map
	private static final Map<String, TokenType> keywords;
	static {
		keywords = new HashMap<>();
		keywords.put("true", TokenType.TRUE);
		keywords.put("false", TokenType.FALSE);
        keywords.put("var", TokenType.VAR);
        keywords.put("if", TokenType.IF);
        keywords.put("else", TokenType.ELSE);
        keywords.put("while", TokenType.WHILE);
        keywords.put("print", TokenType.PRINT);
	}

	// In and output
	private final String source;
	private final List<Token> tokens = new LinkedList<>();

	// Scan state
	private int start = 0;
	private int current = 0;
	private int line = 1;

	public Scanner(String source) {
		this.source = source;
	}

	// Scan tokens
	public List<Token> scanTokens() {
		while (!isAtEnd()) {
			// We are at the beginning of the next lexeme.
			start = current;
			scanToken();
		}

		tokens.add(new Token(EOF, "", null, line));
		return tokens;
	}

	// Scan token
	private void scanToken() {
		char c = advance();
		switch (c) {
			case '{': tokens.add(new Token(LEFT_BRACE, null, null, line)); break;
			case '}': tokens.add(new Token(RIGHT_BRACE, null, null, line)); break;
			case '(': tokens.add(new Token(LEFT_PAREN, null, null, line)); break;
			case ')': tokens.add(new Token(RIGHT_PAREN, null, null, line)); break;
			case ';': tokens.add(new Token(SEMICOLON, null, null, line)); break;

			case '!': tokens.add(new Token(match('=') ? NOT_EQUAL : NOT, null, null, line)); break;
			case '<': tokens.add(new Token(match('=') ? LESSER_EQUAL : LESSER_THAN, null, null, line)); break;
			case '>': tokens.add(new Token(match('=') ? GREATER_EQUAL : GREATER_THAN, null, null, line)); break;
			case '=': tokens.add(new Token(match('=') ? EQUAL_EQUAL : EQUAL, null, null, line)); break;

			case '+': tokens.add(new Token(PLUS, null, null, line)); break;
			case '-': tokens.add(new Token(MINUS, null, null, line)); break;
			case '*': tokens.add(new Token(STAR, null, null, line)); break;
			case '/':
				if (match('/'))
					comment();
				else
					tokens.add(new Token(SLASH, null, null, line));
				break;

			case '&':
				if (match('&'))
					tokens.add(new Token(AND, null, null, line));
				else
					Spl.error(line, "Unexpected character: " + c);
				break;
			
			case '|':
				if (match('|'))
					tokens.add(new Token(OR, null, null, line));
				else
					Spl.error(line, "Unexpected character: " + c);
				break;

			case '"': string();
			case ' ':
			case '\t': break;
			case '\n': line++; break;
		}
	}

	private void identifier() {
	}

	private void number() {

	}

	private void comment() {
		while (!match('\n') && !isAtEnd()) {
			advance();
		}
		line++;
	}

	private void string() {
		while (!match('"')) {
			advance();
		}
		tokens.add(new Token(STRING, source.substring(start, current), null, line));
	}

	// advance to next character
	private char advance() {
		return source.charAt(current++);
	}

	// conditional advance
	private boolean match(char expected) {
		if (peek() == expected) {
			advance();
			return true;
		}
		return false;
	}

	// peek without advance
	private char peek() {
		if (isAtEnd())
			return '\0';
		return source.charAt(current);
	}


	private boolean isAtEnd() {
		return current >= source.length();
	}
}

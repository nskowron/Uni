package spl.scan;

import static spl.scan.Token.Type.*;

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
	private static final Map<String, Token.Type> keywords;
	static {
		keywords = new HashMap<>();
		keywords.put("true", Token.Type.TRUE);
		keywords.put("false", Token.Type.FALSE);
        keywords.put("var", Token.Type.VAR);
        keywords.put("if", Token.Type.IF);
        keywords.put("else", Token.Type.ELSE);
        keywords.put("while", Token.Type.WHILE);
        keywords.put("print", Token.Type.PRINT);
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
			case '{': tokens.add(new Token(LEFT_BRACE, "", null, line)); break;
			case '}': tokens.add(new Token(RIGHT_BRACE, "", null, line)); break;
			case '(': tokens.add(new Token(LEFT_PAREN, "", null, line)); break;
			case ')': tokens.add(new Token(RIGHT_PAREN, "", null, line)); break;
			case ';': tokens.add(new Token(SEMICOLON, "", null, line)); break;

			case '!': tokens.add(new Token(match('=') ? NOT_EQUAL : NOT, "", null, line)); break;
			case '<': tokens.add(new Token(match('=') ? LESSER_EQUAL : LESSER_THAN, "", null, line)); break;
			case '>': tokens.add(new Token(match('=') ? GREATER_EQUAL : GREATER_THAN, "", null, line)); break;
			case '=': tokens.add(new Token(match('=') ? EQUAL_EQUAL : EQUAL, "", null, line)); break;

			case '+': tokens.add(new Token(PLUS, "", null, line)); break;
			case '-': tokens.add(new Token(MINUS, "", null, line)); break;
			case '*': tokens.add(new Token(STAR, "", null, line)); break;
			case '/':
				if (match('/'))
					comment();
				else
					tokens.add(new Token(SLASH, "", null, line));
				break;

			case '&':
				if (match('&'))
					tokens.add(new Token(AND, "", null, line));
				else
					error(line, "Unexpected character: " + c);
				break;
			
			case '|':
				if (match('|'))
					tokens.add(new Token(OR, "", null, line));
				else
					error(line, "Unexpected character: " + c);
				break;

			case '"': string();
			case ' ':
			case '\t': break;
			case '\n': line++; break;

			default:
				if (Character.isDigit(c))
					number();
				else if (Character.isLetter(c) || c == '_')
					identifier();
				else
					error(line, "Unexpected character: " + c);
		}
	}

	// handle keywords and identifiers
	private void identifier() {
		while (Character.isLetterOrDigit(peek()) || peek() == '_') {
			advance();
		}
		String lexeme = source.substring(start, current);
		Token.Type keywordToken = keywords.get(lexeme);
		if (keywordToken != null)
			tokens.add(new Token(keywordToken, "", null, line));
		else
			tokens.add(new Token(ID, lexeme, null, line));
	}

	// handle ints and floats
	private void number() {
		while (Character.isDigit(peek())) {
			advance();
		}
		if (match('.')) {
			while (Character.isDigit(peek())) {
				advance();
			}
			tokens.add(new Token(FLOAT, source.substring(start, current), null, line));
		} else {
			tokens.add(new Token(INT, source.substring(start, current), null, line));
		}
	}

	// handle comments
	private void comment() {
		while (!match('\n') && !isAtEnd()) {
			advance();
		}
		line++;
	}

	// handle strings
	private void string() {
		while (!match('"')) {
			if (peek() == '\n') {
				error(line, "String ending with newline");
				return;
			}
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

	private void error(int line, String message) {
		Spl.error(line, "Syntax error: " + message);
	}
}

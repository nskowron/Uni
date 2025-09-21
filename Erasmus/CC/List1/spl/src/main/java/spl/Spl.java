package spl;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

import spl.parse.Declaration;
import spl.parse.Parser;
import spl.scan.Scanner;
import spl.scan.Token;

public class Spl {
	private static boolean error = false;

	// Expects a single file that comprises a SPL program as argument
	public static void main(String[] args) throws IOException {
		runFile(args[0]);
	}

	private static void runFile(String path) throws IOException {
		byte[] bytes = Files.readAllBytes(Paths.get(path));
		run(new String(bytes));
	}

	private static void run(String source) {
		Scanner scanner = new Scanner(source);
		List<Token> tokens = scanner.scanTokens();

		// print the tokens
		for (Token token : tokens) {
			System.out.println(token);
		}

		Parser parser = new Parser(tokens);
		List<Declaration> statements = parser.parse();
	}

	public static void error(int line, String message) {
		error = true;
		report(line, "", message);
	}

	private static void report(int line, String where, String message) {
		System.err.println("[line " + line + "] Error" + where + ": " + message);
	}

}

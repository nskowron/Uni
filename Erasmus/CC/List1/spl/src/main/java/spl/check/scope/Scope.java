package spl.check.scope;

import java.util.HashMap;

import spl.scan.Token;

public class Scope {
    private final Scope parent;
    private final HashMap<String, Token> symbols; // variables only for now

    public Scope(Scope parent) {
        this.parent = parent;
        this.symbols = new HashMap<>();
    }

    public boolean add(String symbol, Token token) {
        return !symbols.containsKey(symbol) && symbols.put(symbol, token) != null;
    }

    public boolean contains(String symbol) {
        return symbols.containsKey(symbol) || (parent != null && parent.contains(symbol));
    }

    public Token get(String symbol) {
        
    }
}

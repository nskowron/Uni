package spl.parse;

import spl.scan.Token;

public abstract class Declaration {
    public interface Visitor<T> extends Statement.Visitor<T> {
        T visitVarDecl(Var decl);
    }

    public abstract <T> T accept(Visitor<T> visitor);

    public static final class Var extends Declaration {
        public final Token name; public final Expression initializer;
        public Var(Token name, Expression initializer) { this.name = name; this.initializer = initializer; }
        public <T> T accept(Visitor<T> visitor) { return visitor.visitVarDecl(this); }
    }
}

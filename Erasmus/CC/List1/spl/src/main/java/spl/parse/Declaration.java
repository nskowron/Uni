package spl.parse;

import spl.scan.Token;

public abstract class Declaration {
    public interface Visitor extends Statement.Visitor {
        void visitVarDecl(Var decl);
    }

    public abstract void accept(Visitor visitor);

    public static final class Var extends Declaration {
        public final Token name; public final Expression initializer;
        public Var(Token name, Expression initializer) { this.name = name; this.initializer = initializer; }
        public void accept(Visitor visitor) { visitor.visitVarDecl(this); }
    }
}

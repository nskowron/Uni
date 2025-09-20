package spl.parse;

// Stmt.java
import java.util.*;

import spl.scan.Token;

public abstract class Stmt {
    public interface Visitor {
        void visitExpressionStmt(Expression stmt);
        void visitPrintStmt(Print stmt);
        void visitVarStmt(Var stmt);
        void visitBlockStmt(Block stmt);
        void visitIfStmt(If stmt);
        void visitWhileStmt(While stmt);
        void visitvoideturnStmt(voideturn stmt);
    }

    public abstract void accept(Visitor visitor);

    public static final class Expression extends Stmt {
        public final Expr expression;
        public Expression(Expr expression) { this.expression = expression; }
        public void accept(Visitor visitor) { visitor.visitExpressionStmt(this); }
    }

    public static final class Print extends Stmt {
        public final Expr expression;
        public Print(Expr expression) { this.expression = expression; }
        public void accept(Visitor visitor) { visitor.visitPrintStmt(this); }
    }

    public static final class Var extends Stmt {
        public final Token name; public final Expr initializer;
        public Var(Token name, Expr initializer) { this.name = name; this.initializer = initializer; }
        public void accept(Visitor visitor) { visitor.visitVarStmt(this); }
    }

    public static final class Block extends Stmt {
        public final List<Stmt> statements;
        public Block(List<Stmt> statements) { this.statements = statements; }
        public void accept(Visitor visitor) { visitor.visitBlockStmt(this); }
    }

    public static final class If extends Stmt {
        public final Expr condition; public final Stmt thenBranch; public final Stmt elseBranch;
        public If(Expr condition, Stmt thenBranch, Stmt elseBranch) { this.condition = condition; this.thenBranch = thenBranch; this.elseBranch = elseBranch; }
        public void accept(Visitor visitor) { visitor.visitIfStmt(this); }
    }

    public static final class While extends Stmt {
        public final Expr condition; public final Stmt body;
        public While(Expr condition, Stmt body) { this.condition = condition; this.body = body; }
        public void accept(Visitor visitor) { visitor.visitWhileStmt(this); }
    }

    public static final class voideturn extends Stmt {
        public final Token keyword; public final Expr value;
        public voideturn(Token keyword, Expr value) { this.keyword = keyword; this.value = value; }
        public void accept(Visitor visitor) { visitor.visitvoideturnStmt(this); }
    }
}


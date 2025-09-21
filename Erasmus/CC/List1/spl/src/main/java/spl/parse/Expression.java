package spl.parse;

import spl.scan.Token;

public abstract class Expression extends Statement {
    public interface Visitor {
        void visitAssignExpr(Assign expr);
        void visitBinaryExpr(Binary expr);
        void visitLogicalExpr(Logical expr);
        void visitUnaryExpr(Unary expr);
        void visitLiteralExpr(Literal expr);
        void visitVariableExpr(Variable expr);
    }

    public static final class Assign extends Expression {
        public final Token name;
        public final Expression value;
        public Assign(Token name, Expression value) { this.name = name; this.value = value; }
        public void accept(Declaration.Visitor visitor) { visitor.visitAssignExpr(this); }
    }

    public static final class Binary extends Expression {
        public final Expression left; public final Token operator; public final Expression right;
        public Binary(Expression left, Token operator, Expression right) { this.left = left; this.operator = operator; this.right = right; }
        public void accept(Declaration.Visitor visitor) { visitor.visitBinaryExpr(this); }
    }

    public static final class Logical extends Expression {
        public final Expression left; public final Token operator; public final Expression right;
        public Logical(Expression left, Token operator, Expression right) { this.left = left; this.operator = operator; this.right = right; }
        public void accept(Declaration.Visitor visitor) { visitor.visitLogicalExpr(this); }
    }

    public static final class Unary extends Expression {
        public final Token operator; public final Expression right;
        public Unary(Token operator, Expression right) { this.operator = operator; this.right = right; }
        public  void accept(Declaration.Visitor visitor) { visitor.visitUnaryExpr(this); }
    }

    public static final class Literal extends Expression {
        public final Object value;
        public Literal(Object value) { this.value = value; }
        public void accept(Declaration.Visitor visitor) { visitor.visitLiteralExpr(this); }
    }

    public static final class Variable extends Expression {
        public final Token name;
        public Variable(Token name) { this.name = name; }
        public void accept(Declaration.Visitor visitor) { visitor.visitVariableExpr(this); }
    }
}


package spl.parse;

import java.util.List;

import spl.scan.Token;

public abstract class Expr {
    public interface Visitor {
        void visitAssignExpr(Assign expr);
        void visitBinaryExpr(Binary expr);
        void visitLogicalExpr(Logical expr);
        void visitUnaryExpr(Unary expr);
        void visitLiteralExpr(Literal expr);
        void visitGroupingExpr(Grouping expr);
        void visitVariableExpr(Variable expr);
        void visitCallExpr(Call expr);
    }

    public abstract void accept(Visitor visitor);

    public static final class Assign extends Expr {
        public final Token name;
        public final Expr value;
        public Assign(Token name, Expr value) { this.name = name; this.value = value; }
        public void accept(Visitor visitor) { visitor.visitAssignExpr(this); }
    }

    public static final class Binary extends Expr {
        public final Expr left; public final Token operator; public final Expr right;
        public Binary(Expr left, Token operator, Expr right) { this.left = left; this.operator = operator; this.right = right; }
        public void accept(Visitor visitor) { visitor.visitBinaryExpr(this); }
    }

    public static final class Logical extends Expr {
        public final Expr left; public final Token operator; public final Expr right;
        public Logical(Expr left, Token operator, Expr right) { this.left = left; this.operator = operator; this.right = right; }
        public void accept(Visitor visitor) { visitor.visitLogicalExpr(this); }
    }

    public static final class Unary extends Expr {
        public final Token operator; public final Expr right;
        public Unary(Token operator, Expr right) { this.operator = operator; this.right = right; }
        public  void accept(Visitor visitor) { visitor.visitUnaryExpr(this); }
    }

    public static final class Literal extends Expr {
        public final Object value;
        public Literal(Object value) { this.value = value; }
        public void accept(Visitor visitor) { visitor.visitLiteralExpr(this); }
    }

    public static final class Grouping extends Expr {
        public final Expr expression;
        public Grouping(Expr expression) { this.expression = expression; }
        public void accept(Visitor visitor) { visitor.visitGroupingExpr(this); }
    }

    public static final class Variable extends Expr {
        public final Token name;
        public Variable(Token name) { this.name = name; }
        public void accept(Visitor visitor) { visitor.visitVariableExpr(this); }
    }

    public static final class Call extends Expr {
        public final Expr callee; public final Token paren; public final List<Expr> arguments;
        public Call(Expr callee, Token paren, List<Expr> arguments) { this.callee = callee; this.paren = paren; this.arguments = arguments; }
        public void accept(Visitor visitor) { visitor.visitCallExpr(this); }
    }
}


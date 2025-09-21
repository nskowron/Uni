package spl.parse;

import java.util.List;

import spl.scan.Token;

public class ASTPrinter implements Declaration.Visitor<Void> {
    private final StringBuilder builder = new StringBuilder();
    private final StringBuilder prefix = new StringBuilder();

    public String print(List<Declaration> program) {
        builder.append("PROGRAM\n");
        for (int i = 0; i < program.size(); i++) {
            printDeclaration(program.get(i), i == program.size() - 1);
        }
        return builder.toString();
    }

    @Override
    public Void visitVarDecl(Declaration.Var decl) {
        builder.append("VAR\n");
        if (decl.initializer != null) {
            printToken(decl.name, false);
            printDeclaration(decl.initializer, true);
        } else {
            printToken(decl.name, true);
        }
        return null;
    }

    @Override
    public Void visitIfStmt(Statement.If stmt) {
        builder.append("IF\n");
        printDeclaration(stmt.condition, false);
        if (stmt.elseBranch != null) {
            printDeclaration(stmt.thenBranch, false);
            printDeclaration(stmt.elseBranch, true);
        } else {
            printDeclaration(stmt.thenBranch, true);
        }
        return null;
    }

    @Override
    public Void visitPrintStmt(Statement.Print stmt) {
        builder.append("PRINT\n");
        printDeclaration(stmt.expression, true);
        return null;
    }

    @Override
    public Void visitBlockStmt(Statement.Block stmt) {
        builder.append("BLOCK\n");
        for (int i = 0; i < stmt.statements.size(); i++) {
            printDeclaration(stmt.statements.get(i), i == stmt.statements.size() - 1);
        }
        return null;
    }

    @Override
    public Void visitWhileStmt(Statement.While stmt) {
        builder.append("WHILE\n");
        printDeclaration(stmt.condition, false);
        printDeclaration(stmt.body, true);
        return null;
    }

    @Override
    public Void visitAssignExpr(Expression.Assign expr) {
        builder.append("ASSIGN\n");
        printToken(expr.name, false);
        printDeclaration(expr.value, true);
        return null;
    }

    @Override
    public Void visitBinaryExpr(Expression.Binary expr) {
        builder.append("BINARY\n");
        printDeclaration(expr.left, false);
        printToken(expr.operator, false);
        printDeclaration(expr.right, true);
        return null;
    }

    @Override
    public Void visitLogicalExpr(Expression.Logical expr) {
        builder.append("LOGICAL\n");
        printDeclaration(expr.left, false);
        printToken(expr.operator, false);
        printDeclaration(expr.right, true);
        return null;
    }

    @Override
    public Void visitUnaryExpr(Expression.Unary expr) {
        builder.append("UNARY\n");
        printToken(expr.operator, false);
        printDeclaration(expr.right, true);
        return null;
    }

    @Override
    public Void visitLiteralExpr(Expression.Literal expr) {
        builder.append(expr.value.lexeme)
               .append("\n");
        return null;
    }

    @Override
    public Void visitVariableExpr(Expression.Variable expr) {
        builder.append(expr.name.lexeme)
               .append("\n");
        return null;
    }

    private void printToken(Token token, boolean isLast) {
        builder.append(prefix)
               .append(isLast ? "└── " : "├── ")
               .append(token.lexeme)
               .append("\n");
    }

    private void printDeclaration(Declaration decl, boolean isLast) {
        builder.append(prefix);
        if (isLast) {
            builder.append("└── ");
            prefix.append("    ");
        } else {
            builder.append("├── ");
            prefix.append("│   ");
        }
        decl.accept(this);
        prefix.setLength(prefix.length() - 4);
    }
}

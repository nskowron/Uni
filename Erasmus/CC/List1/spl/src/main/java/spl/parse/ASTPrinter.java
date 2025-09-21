package spl.parse;

import java.util.List;

public class ASTPrinter implements Declaration.Visitor<Void> {
    private final StringBuilder builder = new StringBuilder();
    private String prefix = "";
    private boolean isLast = true;

    public String print(List<Declaration> program) {
        builder.setLength(0);
        for (int i = 0; i < program.size(); i++) {
            isLast = (i == program.size() - 1);
            printDeclaration(program.get(i), "", isLast);
        }
        return builder.toString();
    }

    private void printDeclaration(Declaration decl, String prefix, boolean isLast) {
        String prevPrefix = this.prefix;
        boolean prevIsLast = this.isLast;
        this.prefix = prefix;
        this.isLast = isLast;

        builder.append(prefix)
               .append(isLast ? "└── " : "├── ");
        decl.accept(this);

        this.prefix = prevPrefix;
        this.isLast = prevIsLast;
    }

    private void printExpression(Expression expr, String prefix, boolean isLast) {
        String prevPrefix = this.prefix;
        boolean prevIsLast = this.isLast;
        this.prefix = prefix;
        this.isLast = isLast;

        builder.append(prefix)
               .append(isLast ? "└── " : "├── ");
        expr.accept(this);

        this.prefix = prevPrefix;
        this.isLast = prevIsLast;
    }

    // Declaration visitor methods
    @Override
    public Void visitIfStmt(Statement.If stmt) {
        builder.append("ifStmt\n");
        printExpression(stmt.condition, prefix + (isLast ? "    " : "│   "), false);
        printDeclaration(stmt.thenBranch, prefix + (isLast ? "    " : "│   "), stmt.elseBranch == null);
        if (stmt.elseBranch != null) {
            printDeclaration(stmt.elseBranch, prefix + (isLast ? "    " : "│   "), true);
        }
        return null;
    }

    @Override
    public Void visitVarDecl(Declaration.Var stmt) {
        builder.append("varDecl(").append(stmt.name.lexeme).append(")\n");
        if (stmt.initializer != null) {
            printExpression(stmt.initializer, prefix + (isLast ? "    " : "│   "), true);
        }
        return null;
    }

    // ...implement other Declaration visitor methods...

    // Expression visitor methods
    @Override
    public Void visitBinaryExpr(Expression.Binary expr) {
        builder.append("binaryExpr(").append(expr.operator.lexeme).append(")\n");
        printExpression(expr.left, prefix + (isLast ? "    " : "│   "), false);
        printExpression(expr.right, prefix + (isLast ? "    " : "│   "), true);
        return null;
    }

    @Override
    public Void visitLiteralExpr(Expression.Literal expr) {
        builder.append("literal(").append(expr.value).append(")\n");
        return null;
    }

    // ...implement other Expression visitor methods...
}

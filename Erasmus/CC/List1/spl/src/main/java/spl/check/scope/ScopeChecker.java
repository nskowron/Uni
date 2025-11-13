package spl.check.scope;

import java.util.List;
import java.util.Map;

import spl.Spl;
import spl.parse.Declaration;

public class ScopeChecker implements Declaration.Visitor<Void> {
    private final Scope scope = new Scope(null);
    private final boolean ok = true;

    public boolean check(List<Declaration> program) {
        for (Declaration declaration : program) {
            declaration.accept(this);
        }
        return ok;
    }

    @Override
    public Void visitVarDecl(Declaration.Var decl) {
        if (!scope.add(decl.name)) {
            Spl.error(decl.name.line, "Redeclaration of " + decl.name.lexeme + " - first defined in line: " + );
        }
        return null;
    }
}

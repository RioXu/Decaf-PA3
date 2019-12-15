package decaf.frontend.symbol;

import decaf.frontend.scope.LambdaScope;
import decaf.frontend.tree.Pos;
import decaf.frontend.type.FunType;

/**
 * Lambda symbol, representing a lambda definition.
 */
public final class LambdaSymbol extends Symbol {

    public FunType type;

    /**
     * Associated formal scope of the lambda parameters.
     */
    public final LambdaScope scope;

    public LambdaSymbol(String name, FunType type, LambdaScope scope, Pos pos) {
        super(name, type, pos);
        this.type = type;
        this.scope = scope;
        scope.setOwner(this);
    }

    @Override
    public boolean isLambdaSymbol() {
        return true;
    }

    @Override
    protected String str() {
        return String.format("function lambda@%s : %s", pos.toString(), type.toString());
    }

}

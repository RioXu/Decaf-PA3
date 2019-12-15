package decaf.frontend.scope;

import decaf.frontend.symbol.LambdaSymbol;

/**
 * Lambda scope: stores parameter variable symbols. It is owned by a lambda symbol.
 */
public class LambdaScope extends Scope {

    public LambdaScope() {
        super(Kind.FORMAL);
    }

    public LambdaScope(Scope parent) {
        super(Kind.FORMAL);
		((LocalScope) parent).nested.add(this);
    }

    public LambdaSymbol getOwner() {
        return ownerLambda;
    }

    public void setOwner(LambdaSymbol owner) {
        this.ownerLambda = owner;
    }

    @Override
    public boolean isLambdaScope() {
        return true;
    }

    /**
     * Get the local scope associated with the lambda body.
     *
     * @return local scope
     */
    public LocalScope nestedLocalScope() {
        return nestedLocalScope;
    }

    /**
     * Set the local scope.
     *
     * @param scope local scope
     */
    void setNestedLocalScope(LocalScope scope) {
        nestedLocalScope = scope;
    }

    private LambdaSymbol ownerLambda;

    private LocalScope nestedLocalScope;
}

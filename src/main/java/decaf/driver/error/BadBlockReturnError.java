package decaf.driver.error;

import decaf.frontend.tree.Pos;

public class BadBlockReturnError extends DecafError {

    public BadBlockReturnError(Pos pos) {
        super(pos);
    }

    @Override
    protected String getErrMsg() {
        return "incompatible return types in blocked expression";
    }

}

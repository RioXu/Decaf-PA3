package decaf.driver.error;

import decaf.frontend.tree.Pos;

/**
 * string is not a callable type.
 * PA2
 */
public class NotCallableError extends DecafError {

    private String name;

    public NotCallableError(Pos pos, String name) {
        super(pos);
        this.name = name;
    }

    @Override
    protected String getErrMsg() {
        return name + " is not a callable type";
    }

}

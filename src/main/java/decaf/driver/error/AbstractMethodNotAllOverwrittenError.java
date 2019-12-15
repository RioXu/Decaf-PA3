package decaf.driver.error;

import decaf.frontend.tree.Pos;

/**
 * example：function 'gotoMars' expects 1 argument(s) but 3 given<br>
 * PA2
 */
public class AbstractMethodNotAllOverwrittenError extends DecafError {

    private String clazz;

    public AbstractMethodNotAllOverwrittenError(Pos pos, String clazz) {
        super(pos);
        this.clazz = clazz;
    }

    @Override
    protected String getErrMsg() {
        return "'" + clazz + "' is not abstract and does not override all abstract methods";
    }
}

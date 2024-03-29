package decaf.driver.error;

import decaf.frontend.tree.Pos;


public class AssignLocalInLambdaError extends DecafError {

    public AssignLocalInLambdaError(Pos pos) {
        super(pos);
    }

    @Override
    protected String getErrMsg() {
        return "cannot assign value to captured variables in lambda expression";
    }
}

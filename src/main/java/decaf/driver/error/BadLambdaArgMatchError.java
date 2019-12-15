package decaf.driver.error;

import decaf.frontend.tree.Pos;

public class BadLambdaArgMatchError extends DecafError {

    private int expectNum;

    private int givenNum;

    public BadLambdaArgMatchError(Pos pos, int expectNum, int givenNum) {
        super(pos);
        this.expectNum = expectNum;
        this.givenNum = givenNum;
    }

    @Override
    protected String getErrMsg() {
        return "lambda expression expects " + expectNum + 
        " argument(s) but " + givenNum + " given";
    }
}

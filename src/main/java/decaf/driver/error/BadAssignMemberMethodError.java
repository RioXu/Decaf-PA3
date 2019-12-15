package decaf.driver.error;

import decaf.frontend.tree.Pos;


public class BadAssignMemberMethodError extends DecafError {

	private final String name;
	
    public BadAssignMemberMethodError(Pos pos, String name) {
        super(pos);
		this.name = name;
    }

    @Override
    protected String getErrMsg() {
        return "cannot assign value to class member method '" + name + "'";
    }
}

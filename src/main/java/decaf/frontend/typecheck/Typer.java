package decaf.frontend.typecheck;

import decaf.driver.Config;
import decaf.driver.Phase;
import decaf.driver.error.*;
import decaf.frontend.scope.*;
import decaf.frontend.symbol.*;
import decaf.frontend.tree.Pos;
import decaf.frontend.tree.Tree;
import decaf.frontend.type.*;
import decaf.lowlevel.log.IndentPrinter;
import decaf.printing.PrettyScope;

import java.util.*;

import javax.lang.model.util.ElementScanner6;


/**
 * The typer phase: type check abstract syntax tree and annotate nodes with
 * inferred (and checked) types.
 */
public class Typer extends Phase<Tree.TopLevel, Tree.TopLevel> implements TypeLitVisited {

    public Typer(Config config) {
        super("typer", config);
    }

    public Stack<VarSymbol> lambdaDefStack = new Stack<>();

    @Override
    public Tree.TopLevel transform(Tree.TopLevel tree) {
        var ctx = new ScopeStack(tree.globalScope);
        tree.accept(this, ctx);
        return tree;
    }

    @Override
    public void onSucceed(Tree.TopLevel tree) {
        if (config.target.equals(Config.Target.PA2)) {
            var printer = new PrettyScope(new IndentPrinter(config.output));
            printer.pretty(tree.globalScope);
            printer.flush();
        }
    }

    @Override
    public void visitTopLevel(Tree.TopLevel program, ScopeStack ctx) {
        for (var clazz : program.classes) {
            clazz.accept(this, ctx);
        }
    }

    @Override
    public void visitClassDef(Tree.ClassDef clazz, ScopeStack ctx) {
        ctx.open(clazz.symbol.scope);
        for (var field : clazz.fields) {
            field.accept(this, ctx);
        }
        ctx.close();
    }

    @Override
    public void visitMethodDef(Tree.MethodDef method, ScopeStack ctx) {
        ctx.open(method.symbol.scope);
        if (method.body == null) {
            ctx.close();
            return;
        }
        method.body.accept(this, ctx);
        if (!method.symbol.type.returnType.isVoidType() && !method.body.returns) {
            issue(new MissingReturnError(method.body.pos));
        }
        ctx.close();
    }

    /**
     * To determine if a break statement is legal or not, we need to know if we are
     * inside a loop, i.e. loopLevel {@literal >} 1?
     * <p>
     * Increase this counter when entering a loop, and decrease it when leaving a
     * loop.
     */
    private int loopLevel = 0;

    @Override
    public void visitBlock(Tree.Block block, ScopeStack ctx) {
        ctx.open(block.scope);
        block.retTypeList.clear();
        for (var stmt : block.stmts) {
            stmt.accept(this, ctx);
            if(!stmt.retTypeList.isEmpty())
            {
                block.updateRetTypes(stmt.retTypeList);
            }
        }
        ctx.close();
        block.returns = !block.stmts.isEmpty() && block.stmts.get(block.stmts.size() - 1).returns;
    }

    @Override
    public void visitAssign(Tree.Assign stmt, ScopeStack ctx) {

        if(TypeUtil.isInLambda())
        {
            if(stmt.lhs instanceof Tree.VarSel)
            {
                var varsel = (Tree.VarSel)stmt.lhs;
                if(!varsel.receiver.isPresent())
                {
                    var localSymbol = ctx.findLocalSymbol(varsel.name, stmt.pos);
                    var lookupSymbol = ctx.lookupBefore(varsel.name, stmt.pos);
                    if(lookupSymbol.isPresent()&&lookupSymbol.get().isVarSymbol())
                    {
                        var varSymbol = (VarSymbol)lookupSymbol.get();
                        if(localSymbol.isEmpty()&&varSymbol.isLocalVar())
                        {
                            issue(new AssignLocalInLambdaError(stmt.pos));
                        }
                    }
                }
            }
        }
        stmt.lhs.accept(this, ctx);
        stmt.rhs.accept(this, ctx);
        var lt = stmt.lhs.type;
        var rt = stmt.rhs.type;

        if (lt.noError() && (lt.isFuncType() || !rt.subtypeOf(lt))) {
            if(lt.isFuncType())
            {
                var varsel = (Tree.VarSel)stmt.lhs;
                if(varsel.methodSymbol!=null&&varsel.methodSymbol.isClassMember)
                    issue(new BadAssignMemberMethodError(stmt.pos, varsel.name.toString()));
            }
            else
                issue(new IncompatBinOpError(stmt.pos, lt.toString(), "=", rt.toString()));
        }
    }

    @Override
    public void visitExprEval(Tree.ExprEval stmt, ScopeStack ctx) {
        stmt.expr.accept(this, ctx);
    }

    @Override
    public void visitIf(Tree.If stmt, ScopeStack ctx) {
        stmt.retTypeList.clear();
        checkTestExpr(stmt.cond, ctx);
        stmt.trueBranch.accept(this, ctx);
        stmt.falseBranch.ifPresent(b -> b.accept(this, ctx));
        // if-stmt returns a value iff both branches return
        stmt.returns = stmt.trueBranch.returns && stmt.falseBranch.isPresent() && stmt.falseBranch.get().returns;
        if(!stmt.trueBranch.retTypeList.isEmpty())
            stmt.updateRetTypes(stmt.trueBranch.retTypeList);
        if(stmt.falseBranch.isPresent()&&!stmt.falseBranch.get().retTypeList.isEmpty())
            stmt.updateRetTypes(stmt.falseBranch.get().retTypeList);
    }

    @Override
    public void visitWhile(Tree.While loop, ScopeStack ctx) {
        checkTestExpr(loop.cond, ctx);
        loopLevel++;
        loop.body.accept(this, ctx);
        loopLevel--;
        loop.retTypeList.clear();
        if(!loop.body.retTypeList.isEmpty())
            loop.updateRetTypes(loop.body.retTypeList);

    }

    @Override
    public void visitFor(Tree.For loop, ScopeStack ctx) {
        ctx.open(loop.scope);
        loop.init.accept(this, ctx);
        checkTestExpr(loop.cond, ctx);
        loop.update.accept(this, ctx);
        loopLevel++;
        loop.retTypeList.clear();
        for (var stmt : loop.body.stmts) {
            stmt.accept(this, ctx);
            loop.updateRetTypes(stmt.retTypeList);
        }
        loopLevel--;
        ctx.close();
    }

    @Override
    public void visitBreak(Tree.Break stmt, ScopeStack ctx) {
        if (loopLevel == 0) {
            issue(new BreakOutOfLoopError(stmt.pos));
        }
    }

    @Override
    public void visitReturn(Tree.Return stmt, ScopeStack ctx) {
        stmt.retTypeList.clear();
        var expected = ctx.currentMethod().type.returnType;
        stmt.expr.ifPresent(e -> e.accept(this, ctx));
        var actual = stmt.expr.map(e -> e.type).orElse(BuiltInType.VOID);
        if (!TypeUtil.isInLambda()&&actual.noError() && !actual.subtypeOf(expected)) {
            issue(new BadReturnTypeError(stmt.pos, expected.toString(), actual.toString()));
        }
        stmt.returns = stmt.expr.isPresent();
        stmt.retTypeList.add(actual);
    }

    @Override
    public void visitPrint(Tree.Print stmt, ScopeStack ctx) {
        int i = 0;
        for (var expr : stmt.exprs) {
            expr.accept(this, ctx);
            i++;
            if (expr.type.noError() && !expr.type.isBaseType()) {
                issue(new BadPrintArgError(expr.pos, Integer.toString(i), expr.type.toString()));
            }
        }
    }

    private void checkTestExpr(Tree.Expr expr, ScopeStack ctx) {
        expr.accept(this, ctx);
        if (expr.type.noError() && !expr.type.eq(BuiltInType.BOOL)) {
            issue(new BadTestExpr(expr.pos));
        }
    }

    // Expressions

    @Override
    public void visitIntLit(Tree.IntLit that, ScopeStack ctx) {
        that.type = BuiltInType.INT;
    }

    @Override
    public void visitBoolLit(Tree.BoolLit that, ScopeStack ctx) {
        that.type = BuiltInType.BOOL;
    }

    @Override
    public void visitStringLit(Tree.StringLit that, ScopeStack ctx) {
        that.type = BuiltInType.STRING;
    }

    @Override
    public void visitNullLit(Tree.NullLit that, ScopeStack ctx) {
        that.type = BuiltInType.NULL;
    }

    @Override
    public void visitReadInt(Tree.ReadInt readInt, ScopeStack ctx) {
        readInt.type = BuiltInType.INT;
    }

    @Override
    public void visitReadLine(Tree.ReadLine readStringExpr, ScopeStack ctx) {
        readStringExpr.type = BuiltInType.STRING;
    }

    @Override
    public void visitUnary(Tree.Unary expr, ScopeStack ctx) {
        expr.operand.accept(this, ctx);
        var t = expr.operand.type;
        if (t.noError() && !compatible(expr.op, t)) {
            // Only report this error when the operand has no error, to avoid nested errors
            // flushing.
            issue(new IncompatUnOpError(expr.pos, Tree.opStr(expr.op), t.toString()));
        }

        // Even when it doesn't type check, we could make a fair guess based on the
        // operator kind.
        // Let's say the operator is `-`, then one possibly wants an integer as the
        // operand.
        // Once he/she fixes the operand, according to our type inference rule, the
        // whole unary expression
        // must have type int! Thus, we simply _assume_ it has type int, rather than
        // `NoType`.
        expr.type = resultTypeOf(expr.op);
    }

    public boolean compatible(Tree.UnaryOp op, Type operand) {
        return switch (op) {
        case NEG -> operand.eq(BuiltInType.INT); // if e : int, then -e : int
        case NOT -> operand.eq(BuiltInType.BOOL); // if e : bool, then !e : bool
        };
    }

    public Type resultTypeOf(Tree.UnaryOp op) {
        return switch (op) {
        case NEG -> BuiltInType.INT;
        case NOT -> BuiltInType.BOOL;
        };
    }

    @Override
    public void visitBinary(Tree.Binary expr, ScopeStack ctx) {
        expr.lhs.accept(this, ctx);
        expr.rhs.accept(this, ctx);
        var t1 = expr.lhs.type;
        var t2 = expr.rhs.type;
        if (t1.noError() && t2.noError() && !compatible(expr.op, t1, t2)) {
            issue(new IncompatBinOpError(expr.pos, t1.toString(), Tree.opStr(expr.op), t2.toString()));
        }
        expr.type = resultTypeOf(expr.op);
    }

    public boolean compatible(Tree.BinaryOp op, Type lhs, Type rhs) {
        if (op.compareTo(Tree.BinaryOp.ADD) >= 0 && op.compareTo(Tree.BinaryOp.MOD) <= 0) { // arith
            // if e1, e2 : int, then e1 + e2 : int
            return lhs.eq(BuiltInType.INT) && rhs.eq(BuiltInType.INT);
        }

        if (op.equals(Tree.BinaryOp.AND) || op.equals(Tree.BinaryOp.OR)) { // logic
            // if e1, e2 : bool, then e1 && e2 : bool
            return lhs.eq(BuiltInType.BOOL) && rhs.eq(BuiltInType.BOOL);
        }

        if (op.equals(Tree.BinaryOp.EQ) || op.equals(Tree.BinaryOp.NE)) { // eq
            // if e1 : T1, e2 : T2, T1 <: T2 or T2 <: T1, then e1 == e2 : bool
            return lhs.subtypeOf(rhs) || rhs.subtypeOf(lhs);
        }

        // compare
        // if e1, e2 : int, then e1 > e2 : bool
        return lhs.eq(BuiltInType.INT) && rhs.eq(BuiltInType.INT);
    }

    public Type resultTypeOf(Tree.BinaryOp op) {
        if (op.compareTo(Tree.BinaryOp.ADD) >= 0 && op.compareTo(Tree.BinaryOp.MOD) <= 0) { // arith
            return BuiltInType.INT;
        }
        return BuiltInType.BOOL;
    }

    @Override
    public void visitNewArray(Tree.NewArray expr, ScopeStack ctx) {
            expr.elemType.accept(this, ctx);
            expr.length.accept(this, ctx);
            var et = expr.elemType.type;
            var lt = expr.length.type;

            if (et.isVoidType()) {
                issue(new BadArrElementError(expr.elemType.pos));
                expr.type = BuiltInType.ERROR;
            } else {
                expr.type = new ArrayType(et);
            }

            if (lt.noError() && !lt.eq(BuiltInType.INT)) {
                issue(new BadNewArrayLength(expr.length.pos));
            }
    }

    @Override
    public void visitNewClass(Tree.NewClass expr, ScopeStack ctx) {
        var clazz = ctx.lookupClass(expr.clazz.name);
        if (clazz.isPresent()) {
            if (clazz.get().isAbstract()) {
                issue(new AbstractNewedError(expr.pos, clazz.get().name));
                expr.type = BuiltInType.ERROR;
            }
            expr.symbol = clazz.get();
            expr.type = expr.symbol.type;
        } else {
            issue(new ClassNotFoundError(expr.pos, expr.clazz.name));
            expr.type = BuiltInType.ERROR;
        }
    }

    @Override
    public void visitThis(Tree.This expr, ScopeStack ctx) {
        if (ctx.currentMethod().isStatic()) {
            issue(new ThisInStaticFuncError(expr.pos));
        }
        expr.type = ctx.currentClass().type;
    }

    private boolean allowClassNameVar = false;

    @Override
    public void visitVarSel(Tree.VarSel expr, ScopeStack ctx) {
        if (expr.receiver.isEmpty()) {
            // Variable, which should be complicated since a legal variable could refer to a
            // local var,
            // a visible member var, and a class name.
            var symbol = ctx.lookupBefore(expr.name, localVarDefPos.orElse(expr.pos));
            if (symbol.isPresent()) {
                if (symbol.get().isVarSymbol()) {
                    var var = (VarSymbol) symbol.get();
                    for (var lambdaDef : lambdaDefStack) {
                        if (var.name.equals(lambdaDef.name)) {
                            expr.type = BuiltInType.ERROR;
                            issue(new UndeclVarError(expr.pos, expr.name));
                            return;
                        }
                    }
                    expr.varSymbol = var;
                    expr.type = var.type;

                    if (var.isMemberVar()) {
                        if (ctx.currentMethod().isStatic()) {
                            issue(new RefNonStaticError(expr.pos, ctx.currentMethod().name, expr.name));
                        } else {
                            expr.setThis();
                        }
                    }
                    // expr.methodSymbol may be promblematic
                    if (var.type.isFuncType()) {
                        var funType = (FunType) var.type;
                        expr.methodSymbol = new MethodSymbol(expr.name, funType, null, expr.pos, new Tree.Modifiers(),
                                null);
                    }
                    return;
                }

                if (symbol.get().isClassSymbol() && allowClassNameVar) { // special case: a class name
                    var clazz = (ClassSymbol) symbol.get();
                    expr.type = clazz.type;
                    expr.isClassName = true;
                    return;
                }

                if (symbol.get().isMethodSymbol()) {
                    var methodSymbol = (MethodSymbol) symbol.get();
                    expr.type = methodSymbol.type;
                    methodSymbol.isClassMember = true;
                    expr.methodSymbol = methodSymbol;
                    if (ctx.currentMethod().isStatic() && !methodSymbol.isStatic()) {
                        issue(new RefNonStaticError(expr.pos, ctx.currentMethod().name, expr.name));
                    } else {
                        expr.setThis();
                    }
                    return;
                }
            }
            else
            {
                expr.type = BuiltInType.ERROR;
                issue(new UndeclVarError(expr.pos, expr.name));
                return;
            }

        }

        // has receiver
        var receiver = expr.receiver.get();
        allowClassNameVar = true;
        receiver.accept(this, ctx);
        allowClassNameVar = false;
        var rt = receiver.type;
        expr.type = BuiltInType.ERROR;

        if (receiver instanceof Tree.Call2) {
            var recv = (Tree.Call2) receiver;
            if (recv.type.hasError())
                return;
            expr.type = recv.type;
            if (rt.isClassType()) {
                var ct = (ClassType) rt;
                var field = ctx.getClass(ct.name).scope.lookup(expr.name);
                if (field.isPresent() && field.get().isMethodSymbol()) {
                    var method = (MethodSymbol) field.get();
                    method.isClassMember = true;
                    expr.methodSymbol = method;
                    expr.type = method.type;
                    if (!method.isStatic()) {
                        expr.type = BuiltInType.ERROR;
                        issue(new NotClassFieldError(expr.pos, expr.name, ct.toString()));
                    }
                } else if (ctx.currentClass().type.subtypeOf(ct)) {
                    var varSymbol = (VarSymbol) field.get();
                    expr.type = varSymbol.type;
                    expr.varSymbol = varSymbol;
                }
                else if (field.isEmpty()) {
                    issue(new FieldNotFoundError(expr.pos, expr.name, ct.toString()));
                }else
                    issue(new NotClassFieldError(expr.pos, expr.name, ct.toString()));
                return;
            }

        }

        if (receiver instanceof Tree.VarSel) {
            var v1 = (Tree.VarSel) receiver;
            if (v1.isClassName) {
                // special case like MyClass.foo: report error cannot access field 'foo' from
                // 'class : MyClass'
                var ct = (ClassType) rt;
                var field = ctx.getClass(ct.name).scope.lookup(expr.name);
                if (field.isPresent()) {
                    if (field.get().isMethodSymbol()) {
                        var method = (MethodSymbol) field.get();
                        method.isClassMember = true;
                        expr.methodSymbol = method;
                        expr.type = method.type;
                        if (!method.isStatic()) {
                            expr.type = BuiltInType.ERROR;
                            issue(new NotClassFieldError(expr.pos, expr.name, ct.toString()));
                        }
                    } else
                        issue(new NotClassFieldError(expr.pos, expr.name, ctx.getClass(v1.name).type.toString()));

                } else {
                    issue(new FieldNotFoundError(expr.pos, expr.name, ct.toString()));
                }

                return;
            }
        }



        if (!rt.noError()) {
            return;
        }

        if (!rt.isClassType()) {
            if (rt.isArrayType()) {
                if (expr.name.equals("length")) {
                    var formal = new FormalScope();
                    var funType = new FunType(BuiltInType.INT, new ArrayList<Type>());
                    var method = new MethodSymbol("length", funType, formal, expr.pos, new Tree.Modifiers(), null);
                    // formal.setOwner(method);
                    method.isClassMember = true;
                    expr.methodSymbol = method;
                    expr.hasLength = true;
                    expr.type = method.type;
                } else {
                    issue(new NotClassFieldError(expr.pos, expr.name, rt.toString()));
                }
                return;
            }
            issue(new NotClassFieldError(expr.pos, expr.name, rt.toString()));
            return;
        }

        // receiver is of classtype
        var ct = (ClassType) rt;
        var field = ctx.getClass(ct.name).scope.lookup(expr.name);
        if (field.isPresent() && field.get().isVarSymbol()) {
            var var = (VarSymbol) field.get();
            if (var.isMemberVar()) {
                expr.varSymbol = var;
                expr.type = var.type;
                if (!ctx.currentClass().type.subtypeOf(var.getOwner().type)) {
                    // member vars are protected
                    expr.type = BuiltInType.ERROR;
                    issue(new FieldNotAccessError(expr.pos, expr.name, ct.toString()));
                }
            }
        } else if (field.isEmpty()) {
            issue(new FieldNotFoundError(expr.pos, expr.name, ct.toString()));
        } else {
            // field is method
            var symbol = new MethodSymbol(ct.name, null, null, expr.pos, new Tree.Modifiers(), null);
            expr.methodSymbol = symbol;
            var method = (MethodSymbol) field.get();
            method.isClassMember = true;
            expr.methodSymbol = method;
            expr.type = method.type;
        }
    }

    @Override
    public void visitIndexSel(Tree.IndexSel expr, ScopeStack ctx) {
        expr.array.accept(this, ctx);
        expr.index.accept(this, ctx);
        var at = expr.array.type;
        var it = expr.index.type;

        if (!at.isArrayType()) {
            if(!at.hasError())
                issue(new NotArrayError(expr.array.pos));
            expr.type = BuiltInType.ERROR;
            return;
        }

        expr.type = ((ArrayType) at).elementType;
        if (!it.eq(BuiltInType.INT)) {
            issue(new SubNotIntError(expr.pos));
        }
    }

    @Override
    public void visitCall2(Tree.Call2 call, ScopeStack ctx) {
        call.type = BuiltInType.ERROR;
        var receiver = call.expr;
        receiver.accept(this, ctx);
        Type rt = receiver.type;
        if (receiver instanceof Tree.VarSel) {
            var v1 = (Tree.VarSel) receiver;
            if (!rt.isFuncType()) // (receiver.)?var
            {
                if (v1.hasLength) {
                    if (!call.args.isEmpty())
                        issue(new BadLengthArgError(call.pos, call.args.size()));
                    call.type = BuiltInType.INT;
                    call.isArrayLength = true;
                    return;
                }
            } 
            else // (receiver.)?method(args)
            {
                var methodSymbol = v1.methodSymbol;
                call.type = methodSymbol.type.returnType;
                call.symbol = methodSymbol;

                funcCall(call, ctx, true);
                return;
            }
        }  else if (receiver instanceof Tree.Call2) {
            if (rt.isFuncType()) {
                var v2 = (Tree.Call2) receiver;
                var rt2 = (FunType) v2.type;
                call.type = rt2.returnType;

                for (var arg : call.args) {
                    arg.accept(this, ctx);
                }
                recursiveCall(call, rt2);
                return;
            } else if (rt.noError())
                issue(new NotCallableError(call.pos, rt.toString()));
        }else if (receiver.kind == Tree.Kind.LAMBDA) {
            if (rt.isFuncType()) {
                FunType funType = (FunType) rt;
                call.type = funType.returnType;

                for (var arg : call.args) {
                    arg.accept(this, ctx);
                }
                lambdaCall(call, funType);
                return;
            }
        }
        if (rt.noError())
            issue(new NotCallableError(call.pos, rt.toString()));
    }

    private void funcCall(Tree.Call2 call, ScopeStack ctx, boolean requireStatic) {
        MethodSymbol methodSymbol = (MethodSymbol) call.symbol;
        for (var arg : call.args) {
            arg.accept(this, ctx);
        }
        if (methodSymbol.type.arity() != call.args.size()) {
            issue(new BadArgCountError(call.pos, methodSymbol.name, methodSymbol.type.arity(), call.args.size()));
        }

        var iter1 = methodSymbol.type.argTypes.iterator();
        var iter2 = call.args.iterator();
        for (int i = 1; iter1.hasNext() && iter2.hasNext(); i++) {
            Type t1 = iter1.next();
            Tree.Expr e = iter2.next();
            Type t2 = e.type;
            if (t2.noError() && !t2.subtypeOf(t1)) {
                issue(new BadArgTypeError(e.pos, i, t2.toString(), t1.toString()));
            }
        }
    }

    private void lambdaCall(Tree.Call2 call, FunType funType) {
        if (funType.arity() != call.args.size()) {
            issue(new BadLambdaArgMatchError(call.pos, funType.arity(), call.args.size()));
        }
        var iter1 = funType.argTypes.iterator();
        var iter2 = call.args.iterator();
        for (int i = 1; iter1.hasNext() && iter2.hasNext(); i++) {
            Type t1 = iter1.next();
            Tree.Expr e = iter2.next();
            Type t2 = e.type;
            if (t2.noError() && !t2.subtypeOf(t1)) {
                issue(new BadArgTypeError(e.pos, i, t2.toString(), t1.toString()));
            }
        }
    }

    private void recursiveCall(Tree.Call2 call, FunType rt) {
        if (rt.arity() != call.args.size()) {
            issue(new BadLambdaArgMatchError(call.pos, rt.arity(), call.args.size()));
        }
        var iter1 = rt.argTypes.iterator();
        var iter2 = call.args.iterator();
        for (int i = 1; iter1.hasNext() && iter2.hasNext(); i++) {
            Type t1 = iter1.next();
            Tree.Expr e = iter2.next();
            Type t2 = e.type;
            if (t2.noError() && !t2.subtypeOf(t1)) {
                issue(new BadArgTypeError(e.pos, i, t2.toString(), t1.toString()));
            }
        }
    }

    @Override
    public void visitLambda1(Tree.Lambda1 lambda, ScopeStack ctx) {
        boolean status = TypeUtil.isInLambda();
        TypeUtil.setInLambda(true);
        var lambdaSymbol = (LambdaSymbol) lambda.symbol;
        ctx.open(lambdaSymbol.scope);
        var funType = (FunType) lambda.type;
        lambda.obj.accept(this, ctx);
        funType.returnType = lambda.obj.type;
        lambda.type = new FunType(funType.returnType, funType.argTypes);
        lambda.symbol.type = (FunType) lambda.type;
        ctx.close();
        TypeUtil.setInLambda(status);
    }

    @Override
    public void visitLambda2(Tree.Lambda2 lambda, ScopeStack ctx) {
        boolean status = TypeUtil.isInLambda();
        TypeUtil.setInLambda(true);
        var lambdaSymbol = (LambdaSymbol) lambda.symbol;
        ctx.open(lambdaSymbol.scope);
        var funType = (FunType) lambda.type;
        // process block
        funType.returnType = BuiltInType.ERROR;
        lambda.block.accept(this, ctx);
        if (!lambda.block.retTypeList.isEmpty()) {
            var retTypeList = new ArrayList<Type>(lambda.block.retTypeList);
            TypeUtil.hasError = false;
            var upType = TypeUtil.getUpperType(retTypeList);
            if (!lambda.block.returns && !upType.isVoidType())
                issue(new MissingReturnError(lambda.block.pos));
            if(TypeUtil.hasError||upType==null||upType.hasError())
            {
                issue(new BadBlockReturnError(lambda.block.pos));
            }
            if(upType!=null)
                funType.returnType = upType;
        }
        else
        {
            funType.returnType = BuiltInType.VOID;
        }


        lambda.type = new FunType(funType.returnType, funType.argTypes);
        lambda.symbol.type = (FunType) lambda.type;
        ctx.close();
        TypeUtil.setInLambda(status);
    }

    @Override
    public void visitClassTest(Tree.ClassTest expr, ScopeStack ctx) {
        expr.obj.accept(this, ctx);
        expr.type = BuiltInType.BOOL;

        if (!expr.obj.type.isClassType()) {
            issue(new NotClassError(expr.obj.type.toString(), expr.pos));
        }
        var clazz = ctx.lookupClass(expr.is.name);
        if (clazz.isEmpty()) {
            issue(new ClassNotFoundError(expr.pos, expr.is.name));
        } else {
            expr.symbol = clazz.get();
        }
    }

    @Override
    public void visitClassCast(Tree.ClassCast expr, ScopeStack ctx) {
        expr.obj.accept(this, ctx);

        if (!expr.obj.type.isClassType()) {
            issue(new NotClassError(expr.obj.type.toString(), expr.pos));
        }

        var clazz = ctx.lookupClass(expr.to.name);
        if (clazz.isEmpty()) {
            issue(new ClassNotFoundError(expr.pos, expr.to.name));
            expr.type = BuiltInType.ERROR;
        } else {
            expr.symbol = clazz.get();
            expr.type = expr.symbol.type;
        }
    }

    @Override
    public void visitLocalVarDef(Tree.LocalVarDef stmt, ScopeStack ctx) {
        if (stmt.initVal.isEmpty())
            return;

        var initVal = stmt.initVal.get();
        if (initVal.kind == Tree.Kind.LAMBDA) {
            boolean isConflict = false;
            for (var lambdaDef : lambdaDefStack) {
                if (stmt.symbol.name.equals(lambdaDef.name)) {
                    isConflict = true;
                }
            }
            if (!isConflict) {

                lambdaDefStack.push(stmt.symbol);
            }
            initVal.accept(this, ctx);
            if (!isConflict) {
                int index = 0;
                for (var lambdaDef : lambdaDefStack) {
                    if (stmt.symbol.name.equals(lambdaDef.name)) {
                        lambdaDefStack.remove(index);
                        break;
                    }
                    index++;
                }
            }
        } 
        else {
            localVarDefPos = Optional.ofNullable(stmt.id.pos);
            initVal.accept(this, ctx);
            localVarDefPos = Optional.empty();
        }

        var lt = stmt.symbol.type;
        var rt = initVal.type;
        if (lt == null) // var type
        {
            if (!rt.isVoidType()) {
                stmt.symbol.type = rt;
            } else {
                issue(new BadVarTypeError(stmt.symbol.pos, stmt.symbol.name));
                stmt.symbol.type = BuiltInType.ERROR;
            }
            return;
        }

        if (lt.noError() && !rt.subtypeOf(lt)) {
            issue(new IncompatBinOpError(stmt.assignPos, lt.toString(), "=", rt.toString()));
        }
    }

    // Only usage: check if an initializer cyclically refers to the declared
    // variable, e.g. var x = x + 1
    private Optional<Pos> localVarDefPos = Optional.empty();
}

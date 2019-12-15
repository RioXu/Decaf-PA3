package decaf.frontend.typecheck;

import decaf.driver.Config;
import decaf.driver.Phase;
import decaf.driver.error.*;
import decaf.frontend.scope.*;
import decaf.frontend.symbol.*;
import decaf.frontend.tree.Tree;
import decaf.frontend.tree.Tree.MethodDef;
import decaf.frontend.type.BuiltInType;
import decaf.frontend.type.ClassType;
import decaf.frontend.type.FunType;
import decaf.frontend.type.Type;

import java.util.*;

/**
 * The namer phase: resolve all symbols defined in the abstract syntax tree and
 * store them in symbol tables (i.e. scopes).
 */
public class Namer extends Phase<Tree.TopLevel, Tree.TopLevel> implements TypeLitVisited {

    public Namer(Config config) {
        super("namer", config);
    }
    public Stack<Tree.LocalVarDef> lambdaDefStack = new Stack<>();

    @Override
    public Tree.TopLevel transform(Tree.TopLevel tree) {
        tree.globalScope = new GlobalScope();
        var ctx = new ScopeStack(tree.globalScope);
        tree.accept(this, ctx);
        return tree;
    }

    @Override
    public void visitTopLevel(Tree.TopLevel program, ScopeStack ctx) {
        var classes = new TreeMap<String, Tree.ClassDef>();
        // Check conflicting definitions. If any, ignore the redefined ones.
        for (var clazz : program.classes) {
            var earlier = classes.get(clazz.name);
            if (earlier != null) {
                issue(new DeclConflictError(clazz.pos, clazz.name, earlier.pos));
            } else {
                classes.put(clazz.name, clazz);
            }
        }

        // Make sure the base class exists. If not, ignore the inheritance.
        for (var clazz : classes.values()) {
            clazz.parent.ifPresent(p -> {
                if (classes.containsKey(p.name)) { // good
                    clazz.superClass = classes.get(p.name);
                } else { // bad
                    issue(new ClassNotFoundError(clazz.pos, p.name));
                    clazz.parent = Optional.empty();
                }
            });
        }

        // Make sure any inheritance does not form a cycle.
        checkCycles(classes);
        // If so, return with errors.
        if (hasError())
            return;

        // So far, class inheritance is well-formed, i.e. inheritance relations form a
        // forest of trees. Now we need to
        // resolve every class definition, make sure that every member (variable/method)
        // is well-typed.
        // Realizing that a class type can be used in the definition of a class member,
        // either a variable or a method,
        // we shall first know all the accessible class types in the program. These
        // types are wrapped into
        // `ClassSymbol`s. Note that currently, the associated `scope` is empty because
        // member resolving has not
        // started yet. All class symbols are stored in the global scope.
        for (var clazz : classes.values()) {
            createClassSymbol(clazz, ctx.global);
        }

        // Now, we can resolve every class definition to fill in its class scope table.
        // To check if the overriding
        // behaves correctly, we should first resolve super class and then its
        // subclasses.
        for (var clazz : classes.values()) {
            clazz.accept(this, ctx);
        }

        // Finally, let's locate the main class, whose name is 'Main', and contains a
        // method like:
        // static void main() { ... }
        boolean found = false;
        for (var clazz : classes.values()) {
            if (clazz.name.equals("Main") && !clazz.isAbstract()) {
                var symbol = clazz.symbol.scope.find("main");
                if (symbol.isPresent() && symbol.get().isMethodSymbol()) {
                    var method = (MethodSymbol) symbol.get();
                    if (method.isStatic() && method.type.returnType.isVoidType() && method.type.arity() == 0) {
                        method.setMain();
                        program.mainClass = clazz.symbol;
                        clazz.symbol.setMainClass();
                        found = true;
                    }
                }
            }
        }
        if (!found) {
            issue(new NoMainClassError());
        }
    }

    /**
     * Check if class inheritance form cycle(s).
     *
     * @param classes a map between class names to their definitions
     */
    private void checkCycles(Map<String, Tree.ClassDef> classes) {
        var visitedTime = new TreeMap<String, Integer>();
        for (var clazz : classes.values()) {
            visitedTime.put(clazz.name, 0);
        }

        var time = 1; // nodes in the same inheritance path/chain have the same time
        Tree.ClassDef from = null;
        for (var node : classes.keySet()) {
            if (visitedTime.get(node) != 0) { // already done, skip
                continue;
            }

            // visit from this node
            while (true) {
                if (visitedTime.get(node) == 0) { // not visited yet
                    visitedTime.put(node, time);
                    var clazz = classes.get(node);
                    if (clazz.parent.isPresent()) {
                        // continue to visit its parent
                        node = clazz.parent.get().name;
                        from = clazz;
                    } else
                        break;
                } else if (visitedTime.get(node) == time) { // find a cycle
                    issue(new BadInheritanceError(from.pos));
                    break;
                } else { // this node is visited earlier, also done
                    break;
                }
            }
            time++;
        }
    }

    /**
     * Create a class symbol and declare in the global scope.
     *
     * @param clazz  class definition
     * @param global global scope
     */
    private void createClassSymbol(Tree.ClassDef clazz, GlobalScope global) {
        if (global.containsKey(clazz.name))
            return;

        if (clazz.parent.isPresent()) {
            createClassSymbol(clazz.superClass, global);
            var base = global.getClass(clazz.parent.get().name);
            var type = new ClassType(clazz.name, base.type);
            var scope = new ClassScope(base.scope);
            var symbol = new ClassSymbol(clazz.name, base, type, scope, clazz.pos, clazz.modifiers);
            global.declare(symbol);
            clazz.symbol = symbol;
        } else {
            var type = new ClassType(clazz.name);
            var scope = new ClassScope();
            var symbol = new ClassSymbol(clazz.name, type, scope, clazz.pos, clazz.modifiers);
            global.declare(symbol);
            clazz.symbol = symbol;
        }
    }

    @Override
    public void visitClassDef(Tree.ClassDef clazz, ScopeStack ctx) {
        if (clazz.resolved)
            return;

        if (clazz.hasParent()) {
            clazz.superClass.accept(this, ctx);
            clazz.abstractMethods.clear();
            clazz.abstractMethods.addAll(clazz.superClass.abstractMethods);
        }

        ctx.open(clazz.symbol.scope);
        for (var field : clazz.fields) {
            field.accept(this, ctx);
            if (field.kind == Tree.Kind.METHOD_DEF) {
                var method = (MethodDef) field;
                if (method.isAbstract() && !method.override) {
                    clazz.abstractMethods.add(method.name);
                } else if (!method.isStatic() && method.override) {
                    clazz.abstractMethods.remove(method.name);
                }

            }
        }
        ctx.close();
        if (!clazz.abstractMethods.isEmpty() && !clazz.isAbstract())
            issue(new AbstractMethodNotAllOverwrittenError(clazz.pos, clazz.name));
        clazz.resolved = true;
    }

    @Override
    public void visitVarDef(Tree.VarDef varDef, ScopeStack ctx) {
        varDef.typeLit.accept(this, ctx);

        var earlier = ctx.findConflict(varDef.name);
        if (earlier.isPresent()) {
            if (earlier.get().isVarSymbol() && earlier.get().domain() != ctx.currentScope()) {
                issue(new OverridingVarError(varDef.pos, varDef.name));
            } else {
                issue(new DeclConflictError(varDef.pos, varDef.name, earlier.get().pos));
            }
            return;
        }

        if (varDef.typeLit.type.eq(BuiltInType.VOID)) {
            issue(new BadVarTypeError(varDef.pos, varDef.name));
            return;
        }

        if (varDef.typeLit.type.noError()) {
            var symbol = new VarSymbol(varDef.name, varDef.typeLit.type, varDef.pos);
            ctx.declare(symbol);
            varDef.symbol = symbol;
        }
    }

    @Override
    public void visitMethodDef(Tree.MethodDef method, ScopeStack ctx) {
        var earlier = ctx.findConflict(method.name);
        if (earlier.isPresent()) {
            if (earlier.get().isMethodSymbol()) { // may be overriden
                var suspect = (MethodSymbol) earlier.get();
                var formal = new FormalScope();
                typeMethod(method, formal, ctx);
                if (suspect.domain() != ctx.currentScope() && !suspect.isStatic() && !method.isStatic()) {
                    // Only non-static methods can be overriden, but the type signature must be
                    // equivalent.

                    if (method.type.subtypeOf(suspect.type)) { // override success
                        if (method.isAbstract() && !suspect.isAbstract()) {
                            issue(new DeclConflictError(method.pos, method.name, suspect.pos));
                            return;
                        }
                        var symbol = new MethodSymbol(method.name, method.type, formal, method.pos, method.modifiers,
                                ctx.currentClass());
                        method.override = true;
                        ctx.declare(symbol);
                        method.symbol = symbol;
                        if (!method.isAbstract()) {
                            ctx.open(formal);
                            method.body.accept(this, ctx);
                            ctx.close();
                        }

                    } else {
                        issue(new BadOverrideError(method.pos, method.name, suspect.owner.name));
                    }

                    return;
                }
            }

            issue(new DeclConflictError(method.pos, method.name, earlier.get().pos));
            return;
        }

        var formal = new FormalScope();
        typeMethod(method, formal,ctx);
        var symbol = new MethodSymbol(method.name, method.type, formal, method.pos, method.modifiers,
                ctx.currentClass());
        method.override = false;
        ctx.declare(symbol);
        method.symbol = symbol;
        if (method.body != null)
        {
            ctx.open(formal);
            method.body.accept(this, ctx);
            ctx.close();
        }

    }

    private void typeLambda1(Tree.Lambda1 lambda, ScopeStack ctx, LambdaScope lambdaFormal) {
        ctx.open(lambdaFormal);
        var argTypes = new ArrayList<Type>();
        for (var paramDef : lambda.params) {
            paramDef.accept(this, ctx);
            argTypes.add(paramDef.typeLit.type);
        }
        lambda.type = new FunType(null, argTypes);
        ctx.close();
    }
    private void typeLambda2(Tree.Lambda2 lambda, ScopeStack ctx, LambdaScope lambdaFormal) {
        ctx.open(lambdaFormal);
        var argTypes = new ArrayList<Type>();
        for (var paramDef : lambda.params) {
            paramDef.accept(this, ctx);
            argTypes.add(paramDef.typeLit.type);
        }
        lambda.type = new FunType(null, argTypes);
        ctx.close();
    }

    private void typeMethod(Tree.MethodDef method, FormalScope formal,ScopeStack ctx) {
        method.returnType.accept(this, ctx);
        ctx.open(formal);
        if (!method.isStatic())
            ctx.declare(VarSymbol.thisVar(ctx.currentClass().type, method.id.pos));
        var argTypes = new ArrayList<Type>(); 
        for (var param : method.params) {
            param.accept(this, ctx);
            argTypes.add(param.typeLit.type);
        }
        method.type = new FunType(method.returnType.type, argTypes);
        ctx.close();
    }

    @Override
    public void visitBlock(Tree.Block block, ScopeStack ctx) {
        block.scope = new LocalScope(ctx.currentScope());
        ctx.open(block.scope);
        for (var stmt : block.stmts) {
            stmt.accept(this, ctx);
        }
        ctx.close();
    }

    @Override
    public void visitLocalVarDef(Tree.LocalVarDef def, ScopeStack ctx) {
        var earlier = ctx.findConflict(def.name);
        if (def.typeLit != null) // var type
        {
            def.typeLit.accept(this, ctx);
        }
        if (earlier.isPresent()) {
            issue(new DeclConflictError(def.pos, def.name, earlier.get().pos));
            return;
        }


        if(def.initVal.isPresent())
        {
            if(def.initVal.get().kind !=  Tree.Kind.LAMBDA)
            {
                //System.out.println("initval is normal");
                def.initVal.get().accept(this, ctx);
                
            }
            else  //initval not lambda
            {
                boolean isConflict = false;
                for(var lambdaDef: lambdaDefStack)
                {
                    if(def.name.equals(lambdaDef.name))
                    {
                        isConflict = true;
                        issue(new DeclConflictError(def.pos, def.name, lambdaDef.pos));
                    }
                }
                if(!isConflict)
                {

                    lambdaDefStack.push(def);
                }
                def.initVal.get().accept(this, ctx);
                if(!isConflict)
                {
                    int index = 0;
                    for(var lambdaDef: lambdaDefStack)
                    {
                        if(def.name.equals(lambdaDef.name))
                        {
                            lambdaDefStack.remove(index);
                            break;
                        }
                        index++;
                    }
                }
                else  //conflict
                    return;
            }
        }
        // check if this symbol conflicts with outer scope symbols.
        for(var lambdaDef: lambdaDefStack)
        {
            if(def.name.equals(lambdaDef.name))
            {
                issue(new DeclConflictError(def.pos, def.name, lambdaDef.pos));
                return;
            }
        }
        if (def.typeLit!=null&&def.typeLit.type.eq(BuiltInType.VOID)) {
            issue(new BadVarTypeError(def.pos, def.name));
            return;
        }
        if (def.typeLit == null) // var type
        {
            var symbol = new VarSymbol(def.name, null, def.id.pos);
            ctx.declare(symbol);
            def.symbol = symbol;
        }
        else
        {
            if (def.typeLit.type.noError()) {
                //System.out.println("symbol name is "+def.name);
                var symbol = new VarSymbol(def.name, def.typeLit.type, def.id.pos);
                //System.out.println(symbol.toString());
                ctx.declare(symbol);
                def.symbol = symbol;
            }
        }

    }
    @Override
    public void visitLambda1(Tree.Lambda1 lambda, ScopeStack ctx)
    {
        var lambdaFormal = new LambdaScope(ctx.currentScope());
        typeLambda1(lambda, ctx, lambdaFormal);
        var symbol = new LambdaSymbol("", (FunType)lambda.type, lambdaFormal, lambda.pos);
        ctx.declare(symbol);
        lambda.symbol = symbol;
        ctx.open(lambdaFormal);
        var lambdaLocal = new LocalScope(lambdaFormal);
        ctx.open(lambdaLocal);
        lambda.obj.accept(this, ctx);
        ctx.close(); //close lambdaLocal
        ctx.close(); //close lambdaFormal
    }

    @Override
    public void visitLambda2(Tree.Lambda2 lambda, ScopeStack ctx)
    {
        var lambdaFormal = new LambdaScope(ctx.currentScope());
        typeLambda2(lambda, ctx, lambdaFormal);
        var symbol = new LambdaSymbol("", (FunType)lambda.type, lambdaFormal, lambda.pos);
        ctx.declare(symbol);
        lambda.symbol = symbol;
        ctx.open(lambdaFormal);
        lambda.block.accept(this, ctx);
        ctx.close(); //close lambdaFormal
    }

    @Override
    public void visitFor(Tree.For loop, ScopeStack ctx) {
        loop.scope = new LocalScope(ctx.currentScope());
        ctx.open(loop.scope);
        loop.init.accept(this, ctx);
        for (var stmt : loop.body.stmts) {
            stmt.accept(this, ctx);
        }
        ctx.close();
    }

    @Override
    public void visitIf(Tree.If stmt, ScopeStack ctx) {
        stmt.trueBranch.accept(this, ctx);
        stmt.falseBranch.ifPresent(b -> b.accept(this, ctx));
    }

    @Override
    public void visitWhile(Tree.While loop, ScopeStack ctx) {
        loop.body.accept(this, ctx);
    }

    @Override
    public void visitCall2(Tree.Call2 stmt, ScopeStack ctx) {
        stmt.expr.accept(this, ctx);
        for (var expression : stmt.args)
            expression.accept(this, ctx);
    }

    @Override
    public void visitIndexSel(Tree.IndexSel stmt, ScopeStack ctx) {
        stmt.array.accept(this, ctx);
        stmt.index.accept(this, ctx);
    }

    @Override
    public void visitUnary(Tree.Unary stmt, ScopeStack ctx) {
        stmt.operand.accept(this, ctx);
    }

    @Override
    public void visitBinary(Tree.Binary stmt, ScopeStack ctx) {
        stmt.lhs.accept(this, ctx);
        stmt.rhs.accept(this, ctx);
    }

    @Override
    public void visitNewArray(Tree.NewArray stmt, ScopeStack ctx) {
        stmt.length.accept(this, ctx);
    }

    @Override
    public void visitClassTest(Tree.ClassTest stmt, ScopeStack ctx) {
        stmt.obj.accept(this, ctx);
    }

    @Override
    public void visitClassCast(Tree.ClassCast stmt, ScopeStack ctx) {
        stmt.obj.accept(this, ctx);
    }

    @Override
    public void visitAssign(Tree.Assign stmt, ScopeStack ctx) {
        stmt.lhs.accept(this, ctx);
        stmt.rhs.accept(this, ctx);
    }

    @Override
    public void visitExprEval(Tree.ExprEval stmt, ScopeStack ctx) {
        stmt.expr.accept(this, ctx);
    }

    @Override
    public void visitReturn(Tree.Return stmt, ScopeStack ctx) {
        if (stmt.expr.isPresent())
            stmt.expr.get().accept(this, ctx);
    }

    @Override
    public void visitPrint(Tree.Print stmt, ScopeStack ctx) {
        for (var expr : stmt.exprs)
            expr.accept(this, ctx);
    }
}

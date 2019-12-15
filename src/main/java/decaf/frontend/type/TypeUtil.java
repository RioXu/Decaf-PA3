package decaf.frontend.type;
import javax.swing.plaf.ButtonUI;
import java.util.*;

public class TypeUtil
{
    public static boolean hasError = false;
    public static boolean inLambda = false;
    public static void setInLambda(boolean newStatus)
    {
        inLambda = newStatus;
    }
    public static boolean isInLambda()
    {
        return inLambda;
    }
    //get the upper bound of types in typeList
    public static Type getUpperType(List<Type> typeList) {
        Type returnType = BuiltInType.NULL;
        for(var type:typeList)
        {
            if(type.noError()&&!type.isNullType())
            {
                returnType = type;
                break;
            }
        }
        if(returnType.eq(BuiltInType.NULL)) return returnType;

        if (returnType.isArrayType()||returnType.isBaseType() || returnType.isVoidType()) {
            for(var type:typeList)
            {
                if(!type.eq(returnType))
                {
                    hasError = true;
                    return BuiltInType.ERROR;
                }
            }
            return returnType;
        }

        if (returnType.isClassType()) {
            var retClassType = (ClassType) returnType;
            /*
            for(var type:typeList)
            {
                while(retClassType.superType.isPresent()&&!type.subtypeOf(retClassType))
                {
                    retClassType = retClassType.superType.get();
                }
                if(!type.subtypeOf(retClassType))
                {
                    hasError = true;
                    return BuiltInType.ERROR;
                }
            }
             */
            while(true)
            {
                boolean accept = true;
                for(var type: typeList)
                {
                    if(!type.subtypeOf(retClassType))
                    {
                        accept = false;
                        break;
                    }
                }
                if(!accept)
                {
                    if(retClassType.superType.isEmpty())
                    {
                        hasError = true;
                        return BuiltInType.ERROR;
                    }
                    else
                        retClassType = retClassType.superType.get();
                }
                else
                    return retClassType;
            }
        }

        if (returnType.isFuncType()) {
            var retFuncType = (FunType) returnType;
            for(var type:typeList)
            {
                if(!type.isFuncType())
                {
                    hasError = true;
                    return BuiltInType.ERROR;
                }
                var curFuncType = (FunType)type;
                if(curFuncType.argTypes.size()!=retFuncType.argTypes.size())
                {
                    hasError = true;
                    return BuiltInType.ERROR;
                }
            }
            //get params of FunType
            ArrayList<Type> paramTypes = new ArrayList<>();
            for (int i = 0; i < retFuncType.argTypes.size(); i++)
            {
                ArrayList<Type> paramsAtThisPos = new ArrayList<>();
                for(var type: typeList)
                {
                    var funcType = (FunType)type;
                    paramsAtThisPos.add(funcType.argTypes.get(i));
                }
                paramTypes.add(getLowerType(paramsAtThisPos));
            }
            //get returnType of FunType
            ArrayList<Type> paramsAtThisPos = new ArrayList<>();
            for(var type: typeList)
            {
                var funcType = (FunType)type;
                paramsAtThisPos.add(funcType.returnType);
            }
            Type rType = getUpperType(paramsAtThisPos);
            return new FunType(rType, paramTypes);
        }
        return BuiltInType.NULL;

    }

    public static Type getLowerType(List<Type> typeList) {
        Type returnType = BuiltInType.NULL;
        for(var type:typeList)
        {
            if(type.noError()&&!type.isNullType())
            {
                returnType = type;
                break;
            }
        }
        if(returnType.eq(BuiltInType.NULL)) return BuiltInType.NULL;

        if (returnType.isVoidType()||returnType.isBaseType() || returnType.isArrayType()) {
            for(var type:typeList)
            {
                if(!type.eq(returnType))
                {
                    hasError = true;
                    return BuiltInType.ERROR;
                }
            }
            return returnType;
        }

        if (returnType.isClassType())
        {
            for(var type:typeList) {
                boolean accept = true;
                for (var typeOther : typeList) {
                    if (!type.subtypeOf(typeOther)) {
                        accept = false;
                        break;
                    }
                }
                if (accept) return type;
            }
            hasError = true;
            return BuiltInType.ERROR;
        }

        if (returnType.isFuncType()) {
            var retFuncType = (FunType) returnType;
            for(var type:typeList)
            {
                if(!type.isFuncType())
                {
                    hasError = true;
                    return BuiltInType.ERROR;
                }
                var curFuncType = (FunType)type;
                if(curFuncType.argTypes.size()!=retFuncType.argTypes.size())
                {
                    hasError = true;
                    return BuiltInType.ERROR;
                }
            }
            int size = retFuncType.argTypes.size();
            //get params of FunType
            ArrayList<Type> paramTypes = new ArrayList<>();
            for (int i = 0; i < size; i++)
            {
                ArrayList<Type> thisParamTypes = new ArrayList<>();
                for(var type: typeList)
                {
                    var funcType = (FunType)type;
                    thisParamTypes.add(funcType.argTypes.get(i));
                }
                paramTypes.add(getUpperType(thisParamTypes));
            }
            //get returnType of FunType
            ArrayList<Type> thisParamTypes = new ArrayList<>();
            for(var type: typeList)
            {
                var funcType = (FunType)type;
                thisParamTypes.add(funcType.returnType);
            }
            Type rType = getLowerType(thisParamTypes);
            return new FunType(rType, paramTypes);
        }
        return null;
    }
}
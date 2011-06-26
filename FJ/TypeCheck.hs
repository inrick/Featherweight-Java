module FJ.TypeCheck (runTypeCheck) where

import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec

import FJ.Auxiliary
import FJ.Data
import FJ.Parser

-- {{{ Type checking functions

-- Check the whole class table that consists of the parsed class table plus
-- the special class Object.
tcheckClassTable :: ClassTable -> Bool
tcheckClassTable ct = tcheckClassTable1 ct'
    where
        ct'         = object:ct
        object      = Class "Object" "" [] (Constructor "Object" [] [] []) []

-- Check the whole class table.
tcheckClassTable1 :: ClassTable -> Bool
tcheckClassTable1 ct
    | not classesAreUnique = error "The classes are not unique."
    | not classTableOK     = error "Some class failed the type check."
    | otherwise            = True
    where
        classNames = map cname ct

        -- Check that all the classes in the class table are unique.
        classesAreUnique = nub classNames == classNames

        -- Check that all the classes in the class table are OK.
        classTableOK     = all ((==) True) $ map (tcheckClass ct) ct

        -- Would also like to check that there are no circular class
        -- dependencies here.


-- Check that the class is OK.
tcheckClass :: ClassTable -> Class -> Bool
tcheckClass ct c = classHeadOK && fieldsOK && constructorOK && methodsOK
    where
        classHeadOK   = tcheckClassHead   ct c
        fieldsOK      = tcheckFields      ct c
        constructorOK = tcheckConstructor ct c
        methodsOK     = tcheckMethods     ct c

-- Check that the superclass exists
tcheckClassHead :: ClassTable -> Class -> Bool
tcheckClassHead ct c =
    if cname c == "Object"
        then True -- We know Object is OK
    -- Check that the superclass exists (and is not the same as the class
    -- itself) in the given class table. That the classes in the class table
    -- are unique was already checked in tcheckClassTable1 which is why we can
    -- allow ourselves to have "x /= c" below.
        else not $ null [x | x <- ct, x /= c, cname x == csuperClass c]

-- Check that the fields are given a unique name and that their type exists.
tcheckFields :: ClassTable -> Class -> Bool
tcheckFields ct c = fieldsAreUnique && fieldsOK
    where
        -- Check that all the types of the fields exist in the class table.
        fieldsOK =
            if all ((==) True) $ map (classExists ct . ftype) (cfields c)
                then True
                else error $ "Some field in class " ++ (cname c) ++
                             " is of a type which does not exist. T-Class failed."
        
        -- Check that all the fields have unique names all through the superclasses.
        fieldsAreUnique =
            if nubBy (\x -> \y -> fname x == fname y) (fields ct c) == fields ct c
                then True
                else error $ "The class " ++ (cname c) ++ " contains fields with duplicate names \
                             \(possibly a duplicate of a field in a superclass). T-Class failed."

-- Part of (T-Class). (Everything but M* OK IN C.)
tcheckConstructor :: ClassTable -> Class -> Bool
tcheckConstructor ct c = argumentsOK && assignmentsOK && superParametersOK
    where
        -- For easy access to the constructor.
        cons = cconstructor c

        -- Partition the arguments by if they are part of the current class or part of some superclass.
        (classFields, superClassFields) = partition (\x -> x `elem` (cfields c)) (kargs cons)

        -- Check that the assignments are OK in the sense that the fields that are assigned to
        -- have the same name as the argument to the constructor and the same name as the fields
        -- in the class.
        assignmentsOK =
            if map fname classFields == map fst (kassignments cons) &&
                all ((==) True) (map (\(x,y) -> x == y) (kassignments cons))
                then True
                else error $ "Something wrong with the assignments in the constructor of class "
                             ++ (cname c) ++ ". T-Class failed."

        -- Check that all the types in the arguments exist and that the names of the parameters
        -- correspond to the names in the class and its superclasses.
        argumentsOK =
            if kargs cons == fields ct c
                then True
                else error $ "The arguments to the constructor in class " ++ (cname c) ++
                             " are incorrect. T-Class failed."

        -- Check that the parameters to the super constructor correspond to the fields that 
        -- exist in the superclasses.
        superParametersOK =
            if map fname superClassFields == ksuperParam cons
                then True
                else error $ "The parameters to super in class " ++ (cname c) ++ " are incorrect. \
                             \T-Class failed."

-- (T-Method)
tcheckMethods :: ClassTable -> Class -> Bool
tcheckMethods ct c = methodsAreUnique && argsOK && mtypeOK && mexprsOK
    where
        -- For easy access to the methods.
        methods = cmethods c

        -- Run the method head and expression checkers on all methods in the class.
        mtypeOK = all ((==) True) $ map mheadOK methods
        mexprsOK = all ((==) True) $ map expressionOK methods

        -- Check that the methods have unique names.
        methodsAreUnique = if nub methodNames == methodNames
                               then True
                               else error $ "Class " ++ (cname c) ++ " contains duplicate methods. \
                                            \T-Class failed."
            where
                methodNames = map mname methods

        -- Check that the arguments have unique names.
        argsOK = if map nub methodArgNames == methodArgNames
                     then True
                     else error $ "Methods are not allowed to have several arguments with the same \
                                  \name (error in class " ++ (cname c) ++ "). T-Method failed."
            where
                methodArgNames = map (map fname . margs) methods

        -- Check that the method head is OK, i.e. the "if mtype ..."-part of T-Method.
        -- The implication [P -> Q] in T-Method is rewritten as [(not P) or Q].
        mheadOK m =
            if (sclassTypeSig == Nothing) || fromJust sclassTypeSig == (map ftype (margs m), mtype m)
                then True
                else error $ "Method " ++ (mname m) ++ " in class " ++ (cname c) ++ " does not match \
                             \the types of its namesake in some superclass. T-Method failed."
            where
                sclassTypeSig = typeSig (superClassOf ct c)
                -- Find the type signature of the method in the superclass, if it exists.
                typeSig c'
                    | cname c' == "Object" = Nothing
                    | mfound /= Nothing    = Just (map ftype (margs result), mtype result)
                    | otherwise            = typeSig (superClassOf ct c')
                    where
                        mfound = find (\x -> mname m == mname x) (cmethods c')
                        result = fromJust mfound

        -- Check the expression part of T-Method, i.e. if the expression has type E0
        -- and the method is supposed to return type C0, check that E0 <: C0.
        expressionOK (Method mt mn ma me) =
           if isSubClass ct (getClass ct (exprType ct m' me)) (getClass ct mt)
               then True
               else error $ "Expression in " ++ mn ++ " in class " ++ (cname c) ++
                            " is not a subtype of " ++ mt ++ ". T-Method failed."
            where
                -- Prepend "this" to the method arguments, as specified by the typing rules.
                m' = Method mt mn (this : ma) me
                this = Field (cname c) "this"

-- Expression typing:
exprType :: ClassTable -> Method -> Expr -> Type
-- (T-Var)
exprType ct m (ExprVar var) =
    if foundVar /= Nothing
        then ftype $ fromJust foundVar
        else error $ "Variable " ++ var ++ " does not exist in the environment of method "
                     ++ (mname m) ++ ". T-Var failed."
    where
        foundVar = find (\x -> fname x == var) $ margs m

-- (T-Field)
exprType ct m (ExprField e0expr e0name) =
    if foundField /= Nothing
        then ftype $ fromJust foundField
        else error $ "Type error in method " ++ (mname m) ++ ". T-Field failed."
    where
        foundField = find (\x -> fname x == e0name) $ fields ct (getClass ct (exprType ct m e0expr))
-- (T-Invk)
exprType ct m (ExprMethod e0expr e0name e0exprs) =
    if subTypesOK
        then retType
        else error $ "Type error in method " ++ (mname m) ++ ". T-Invk failed."
    where
        (argTypes, retType) = methtype ct (getClass ct (exprType ct m e0expr)) e0name
        exprTypes = map (exprType ct m) e0exprs
        subTypesOK = length exprTypes == length argTypes &&
                      all ((==) True) (zipWith (isSubClass ct) (map (getClass ct) exprTypes) (map (getClass ct) argTypes))

-- (T-New)
exprType ct m (ExprNew newType e0exprs) =
    if subTypesOK
        then newType
        else error $ "Type error in method " ++ (mname m) ++ ". T-New failed."
    where
        classFieldTypes = map ftype $ fields ct $ getClass ct newType
        exprTypes = map (exprType ct m) e0exprs
        subTypesOK = length exprTypes == length classFieldTypes &&
                      all ((==) True) (zipWith (isSubClass ct) (map (getClass ct) exprTypes) (map (getClass ct) classFieldTypes))

-- (T-*Cast)
-- While all the casts should return castType as the type of the expression,
-- the code below is just to show how it is possible to distinguish between them.
-- One might want to inform when a stupid cast takes place, for example.
-- Otherwise, the function could just take the form:
-- exprType ct m (ExprCast castType _) = castType
exprType ct m (ExprCast castType e0expr)
    | isSubClass ct class1 class2 = castType -- T-UCast
    | isSubClass ct class2 class1 = castType -- T-DCast
    | otherwise                   = castType -- T-SCast
    where
        class1 = getClass ct (exprType ct m e0expr)
        class2 = getClass ct castType

-- }}}

-- {{{

runTypeCheck :: SourceName -> IO ()
runTypeCheck inputFile =
    do { result <- parseFromFile classTableParser inputFile
       ; case result of
             Left err -> print err
             Right res ->
                 do putStrLn $ if typesOK
                                   then inputFile ++ " passed the type check."
                                   else inputFile ++ " did NOT pass the type check."
                        where
                            typesOK = tcheckClassTable res
       }

-- }}}


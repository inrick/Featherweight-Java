module FJ.Auxiliary where

import Data.List
import Data.Maybe

import FJ.Data

-- {{{ Auxiliary functions

-- Checks if a class with the given name exist in the given class table.
classExists :: ClassTable -> Name -> Bool
classExists ct clname = any (\c -> clname == cname c) ct

-- Find and return the superclass of a given class.
superClassOf :: ClassTable -> Class -> Class
superClassOf ct c
    | superClass == Nothing = error "The superclass does not exist in the given class table."
    | otherwise             = fromJust superClass
    where
        superClass = find (\x -> cname x == csuperClass c) ct

-- Check if c1 <: c2 in the given class table.
isSubClass :: ClassTable -> Class -> Class -> Bool
isSubClass ct c1 c2
    | c1 == c2             = True
    -- If c1 is Object the alternatives have been exhausted and c1 </: c2.
    | cname c1 == "Object" = False
    | otherwise            = isSubClass ct (superClassOf ct c1) c2

-- Find and return the class with the given name (if it exists in the class table).
getClass :: ClassTable -> Name -> Class
getClass ct cn
    | c /= Nothing = fromJust c
    | otherwise    = error $ "No class with name " ++ cn ++ " exists in the given class table."
    where
        c = find (\x -> cname x == cn) ct

-- Field lookup
-- Returns a list of all the fields in the given class and its superclasses.
fields :: ClassTable -> Class -> [Field]
fields ct c
    | cname c == "Object" = [] -- As specified in [1], Object does not contain any fields.
    -- Note the importance of the order of the fields returned below.
    | otherwise           = fields ct (superClassOf ct c) ++ cfields c

-- Method type lookup
-- mtype(m,C) as found in [1].
methtype :: ClassTable -> Class -> Name -> ([Type], Type)
methtype = mcombine mtype ftype

-- Method body lookup
-- mbody(m,C) as found in [1].
-- Not actually used for the typing rules though.
mbody :: ClassTable -> Class -> Name -> ([Name], Expr)
mbody = mcombine mexpr fname

-- Since methtype and mbody are so similar, mcombine was written to combine them.
mcombine :: (Method -> a) -> (Argument -> b) -> ClassTable -> Class -> Name -> ([b], a)
mcombine mfunc afunc ct c m
    -- If this point is ever reached something is wrong with the given code.
    | cname c == "Object" = error "Object does not contain any methods."
    -- If the method name given is in the class return what is needed from there,
    -- otherwise search for the method in the superclass.
    | method /= Nothing   = (map afunc (margs theMethod), mfunc theMethod)
    | otherwise           = mcombine mfunc afunc ct (superClassOf ct c) m
    where
        -- Try to find the method with the given name in the class.
        method    = find (\x -> m == mname x) (cmethods c)
        -- If it was found it is given by theMethod.
        theMethod = fromJust method

-- }}}


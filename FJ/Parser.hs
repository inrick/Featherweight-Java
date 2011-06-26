module FJ.Parser (classTableParser) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Text.ParserCombinators.Parsec.Expr as E

import FJ.Data

-- {{{ Some things needed from Parsec.Token

lexer :: P.TokenParser ()
lexer = P.makeTokenParser fjDef

whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme lexer
symbol     = P.symbol lexer
parens     = P.parens lexer
semi       = P.semi lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
braces     = P.braces lexer
reservedOp = P.reservedOp lexer

-- }}}

-- {{{ Parser related

-- Use Parsec.Language:s LanguageDef
fjDef :: LanguageDef st
fjDef = LanguageDef
        { commentStart = "/*" -- While comments are not actually part of Featherweight Java,
        , commentEnd   = "*/" -- when Parsec offers the convenience, why not use it?
        , commentLine  = "//"
        , nestedComments = False
        , identStart   = letter
        , identLetter  = alphaNum <|> char '_'
        , opStart      = oneOf "."
        , opLetter     = oneOf "."
        , reservedNames = ["class", "extends", "super", "return"
                          , "new", "Object", "this"
                          ]
        , reservedOpNames = ["."]
        , caseSensitive = True
        }

-- Class head:
-- class C extends D { <class body> }
classHeadParser =
    do { reserved "class"
       ; className <- identifier -- className may not be Object.
       ; reserved "extends"
       ; classSuperClass <- typeParser -- But the superclass may be Object.
       ; (classFields, classConstructor, classMethods) <- braces classBodyParser
       ; return $ Class className classSuperClass classFields classConstructor classMethods
       }
    <?> "class head"

-- Class body:
-- C* f*; <constructor> M*
-- (Note that * denotes zero or more occurrences.)
classBodyParser =
    do { classFields <- many (try fieldParser) <|> return []
       ; classConstructor <- constructorParser
       ; classMethods <- many methodParser
       ; return (classFields, classConstructor, classMethods)
       }
    <?> "class body"
 
-- Field:
-- C f;
fieldParser =
    do { fieldType <- typeParser
       ; fieldName <- identifier
       ; semi
       ; return $ Field fieldType fieldName
       }
    <?> "field"

-- Type:
-- C
-- May be either Object or some identifier.
typeParser = identifier
         <|> do { reserved "Object"; return "Object" }
         <?> "type"

-- Constructor:
-- C(D* g*, C* f*) { <body> }
constructorParser =
    do { constructorName <- identifier
       ; constructorArgs <- parens argumentParser
       ; (superParams, constructorAssignments) <- braces constructorBodyParser
       ; return $ Constructor constructorName constructorArgs superParams constructorAssignments
       }
    <?> "constructor"

-- Constructor body:
-- super(g*);
-- this.f* = f*;
constructorBodyParser =
    do { reserved "super"
       ; superParams <- parens $ sepBy identifier $ symbol ","
       ; semi
       ; constructorAssignments <- many assignmentParser
       ; return (superParams, constructorAssignments)
       }

-- Assignment:
-- this.f* = f*;
--
-- It would be possible to check if classField == constructorArgument here,
-- but it is instead done later for consistency.
assignmentParser =
    do { reserved "this"
       ; symbol "."
       ; classField <- identifier
       ; symbol "="
       ; constructorArgument <- identifier
       ; semi
       ; return (classField, constructorArgument)
       }
    <?> "assignment"

-- Method:
-- C m(C* x*) { return e; }
methodParser =
    do { methodType <- typeParser;
       ; methodName <- identifier;
       ; methodArgs <- parens argumentParser
       ; expression <- braces (do { reserved "return"
                                  ; expr <- exprParser
                                  ; semi
                                  ; return expr
                                  })
       ; return $ Method methodType methodName methodArgs expression
       }
    <?> "method"

-- Argument:
-- C* f*
argumentParser = sepBy p $ symbol ","
    where
        p = do { argumentType <- typeParser
               ; argumentName <- identifier
               ; return $ Field argumentType argumentName -- type Argument = Field
               }
            <?> "argument"

-- Expression:
-- x | e.f | e.m(e*) | new C(e*) | (C)e
--
-- The idea is to parse this using Parsec's buildExpressionParser
-- with "." as a binary operator. We can then distinguish the expression
-- to the right of "." and initialize the appropriate type of Expr.
exprParser = E.buildExpressionParser table term
    <?> "expression"

table :: E.OperatorTable Char () Expr
table = [[E.Infix (reservedOp "." >> return makeExpr) E.AssocLeft]]
    where
        makeExpr e1 (ExprMethod _ e2name e2exprs) = ExprMethod e1 e2name e2exprs
        makeExpr e1 (ExprVar e2name) = ExprField e1 e2name
        makeExpr _ _ = error "Something is terribly wrong with some expression."

-- Begin with the distinguished "new ..." or "(C)" and proceed with the options
-- that begin with an identifier.
term  = exprNewParser
    <|> try exprCastParser -- try is used since (whatever) can also be (expression).
    <|> parens exprParser
    <|> try exprMethodParser -- try is used since it is possible that the identifier is
                             -- not followed by (e*), i.e. it should be a variable/field.
    <|> exprOtherParser
    <?> "expression"

-- Parse "new C(e*)"
exprNewParser =
    do { reserved "new"
       ; exprType <- typeParser
       ; exprs <- parens $ sepBy exprParser $ symbol ","
       ; return $ ExprNew exprType exprs
       }

-- Parse "(C) e"
exprCastParser =
    do { castType <- parens typeParser
       ; expr <- exprParser
       ; return $ ExprCast castType expr
       }

-- Parse "m(e*)"
-- Known problem: this makes m(e*) valid syntax even if it does not occur
-- to the right of an expression.
exprMethodParser = 
    do { method <- identifier
       ; exprs <- parens $ sepBy exprParser $ symbol ","
       ; return $ ExprMethod (ExprVar "_") method exprs
       }

-- Parse an identifier and make sense of it later (if it should be ExprVar or ExprField).
exprOtherParser =
    do { raw <- identifier <|> do { reserved "this"; return "this" }
       ; return $ ExprVar raw
       }


-- Parse and return the whole class table.
classTableParser =
    do { whiteSpace
       ; ct <- many classHeadParser
       ; eof
       ; return ct
       }

-- }}}


module FJ.Data where

-- {{{ Data structures and types

data Class = Class
    { cname        :: Name
    , csuperClass  :: Name
    , cfields      :: [Field]
    , cconstructor :: Constructor
    , cmethods     :: [Method]
    }
    deriving (Eq, Show)

data Field = Field
    { ftype :: Type
    , fname :: Name
    }
    deriving (Eq, Show)

data Method = Method
    { mtype :: Type -- Not to be confused with mtype in [1].
    , mname :: Name
    , margs :: [Argument]
    , mexpr :: Expr
    }
    deriving (Eq, Show)

data Constructor = Constructor
    { kname         :: Name
    , kargs         :: [Argument]
    , ksuperParam   :: [Parameter]
    , kassignments  :: [Assignment]
    }
    deriving (Eq, Show)

data Expr =
    ExprVar    { ename  :: Name }
  | ExprField  { eexpr  :: Expr
               , ename  :: Name
               }
  | ExprMethod { eexpr  :: Expr
               , ename  :: Name
               , eexprs :: [Expr]
               }
  | ExprNew    { etype  :: Type
               , eexprs :: [Expr]
               }
  | ExprCast   { etype  :: Type
               , eexpr  :: Expr
               }
    deriving (Eq, Show)

type Name        = String
type Type        = Name
type Argument    = Field
type Parameter   = Name
type Assignment  = (Name, Name)
type ClassTable  = [Class]

-- }}}


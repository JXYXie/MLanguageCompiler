

module AbsM where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Prog = Prog1 Block
  deriving (Eq, Ord, Show, Read)

data Block = Block1 [Decl] Prog_body
  deriving (Eq, Ord, Show, Read)

data Decl = Decl1 Var_decl | Decl2 Fun_decl
  deriving (Eq, Ord, Show, Read)

data Var_decl = Var_decl1 Ident Array_dims Type
  deriving (Eq, Ord, Show, Read)

data Type = Type1 | Type2 | Type3
  deriving (Eq, Ord, Show, Read)

data Array_dims = Array_dims1 Expr Array_dims | Array_dims2
  deriving (Eq, Ord, Show, Read)

data Fun_decl = Fun_decl1 Ident Param_list Type Fun_block
  deriving (Eq, Ord, Show, Read)

data Fun_block = Fun_block1 [Decl] Fun_body
  deriving (Eq, Ord, Show, Read)

data Param_list = Param_list1 [Param]
  deriving (Eq, Ord, Show, Read)

data Param = Param1 Basic_decl
  deriving (Eq, Ord, Show, Read)

data Basic_decl = Basic_decl1 Ident Basic_array_dims Type
  deriving (Eq, Ord, Show, Read)

data Basic_array_dims
    = Basic_array_dims1 Basic_array_dims | Basic_array_dims2
  deriving (Eq, Ord, Show, Read)

data Prog_body = Prog_body1 [Stmt]
  deriving (Eq, Ord, Show, Read)

data Fun_body = Fun_body1 [Stmt] Expr
  deriving (Eq, Ord, Show, Read)

data Stmt
    = Stmt1 Expr Stmt Stmt
    | Stmt2 Expr Stmt
    | Stmt3 Identifier
    | Stmt4 Identifier Expr
    | Stmt5 Expr
    | Stmt6 Block
  deriving (Eq, Ord, Show, Read)

data Identifier = Identifier1 Ident Array_dims
  deriving (Eq, Ord, Show, Read)

data Expr = Expr1 Expr Bint_term | Expr2 Bint_term
  deriving (Eq, Ord, Show, Read)

data Bint_term
    = Bint_term1 Bint_term Bint_factor | Bint_term2 Bint_factor
  deriving (Eq, Ord, Show, Read)

data Bint_factor
    = Bint_factor1 Bint_factor
    | Bint_factor2 Int_expr Compare_op Int_expr
    | Bint_factor3 Int_expr
  deriving (Eq, Ord, Show, Read)

data Compare_op
    = Compare_op1
    | Compare_op2
    | Compare_op3
    | Compare_op4
    | Compare_op5
  deriving (Eq, Ord, Show, Read)

data Int_expr
    = Int_expr1 Int_expr Add_op Int_term | Int_expr2 Int_term
  deriving (Eq, Ord, Show, Read)

data Add_op = Add_op1 | Add_op2
  deriving (Eq, Ord, Show, Read)

data Int_term
    = Int_term1 Int_term Mul_op Int_factor | Int_term2 Int_factor
  deriving (Eq, Ord, Show, Read)

data Mul_op = Mul_op1 | Mul_op2
  deriving (Eq, Ord, Show, Read)

data Int_factor
    = Int_factor1 Expr
    | Int_factor2 Ident Basic_array_dims
    | Int_factor3 Expr
    | Int_factor4 Expr
    | Int_factor5 Expr
    | Int_factor6 Ident Mod_list
    | Int_factor7 Integer
    | Int_factor8 Double
    | Int_factor9
    | Int_factor10
    | Int_factor11 Int_factor
  deriving (Eq, Ord, Show, Read)

data Mod_list = Mod_list1 [Arg] | Mod_list2 Array_dims
  deriving (Eq, Ord, Show, Read)

data Arg = Arg1 Expr
  deriving (Eq, Ord, Show, Read)


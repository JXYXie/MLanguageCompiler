{-  CPSC 411 Assignment 4 AST file
    Author: Xin Yan (Jack) Xie
    Apr 7, 2018
    This file contains all the production rules of the AST for the M+ language
-}
module AST where

data M_Prog = M_Prog ([M_Decl],[M_Stmt])
            deriving (Eq, Show, Ord, Read)

data M_Decl = M_Var (String,[M_Expr],M_Type)
            | M_Fun (String,[(String,Int,M_Type)],M_Type,[M_Decl],[M_Stmt])
            deriving (Eq, Show, Ord, Read)
            
data M_Stmt = M_Ass (String,[M_Expr],M_Expr)
            | M_While (M_Expr,M_Stmt)
            | M_Cond (M_Expr,M_Stmt,M_Stmt)
            | M_Read (String,[M_Expr])
            | M_Print (M_Expr)
            | M_Return (M_Expr)
            | M_Block ([M_Decl],[M_Stmt])
            deriving (Eq, Show, Ord, Read)

data M_Type = M_Int
            | M_Real
            | M_Bool
            deriving (Eq, Show, Ord, Read)

data M_Expr = M_Ival Integer
            | M_Rval Float
            | M_Bval Bool
            | M_Size (String,Int)
            | M_Id (String,[M_Expr])
            | M_App (M_Op,[M_Expr])
            deriving (Eq, Show, Ord, Read)
            
data M_Op = M_Fn String
                | M_Add
                | M_Sub
                | M_Mul
                | M_Div
                | M_Neg
                | M_LT
                | M_GT
                | M_LE
                | M_GE
                | M_EQ
                | M_Not
                | M_And
                | M_Or
                | M_Float
                | M_Floor
                | M_Ceil
                deriving (Eq, Show, Ord, Read)
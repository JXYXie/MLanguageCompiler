{-  CPSC 411 Assignment 5 Intermediate Representation file
    Author: Xin Yan (Jack) Xie
    Apr 7, 2018
-}
module IR where

import AST
import ST


data I_Prog  = IPROG ([I_FunBody],Int,[(Int,[I_Expr])],[I_Stmt])
            deriving (Eq, Show, Ord, Read)
            
data I_FunBody = IFUN (String,[I_FunBody],Int,Int,[(Int,[I_Expr])],[I_Stmt])
            deriving (Eq, Show, Ord, Read)
            
data I_Stmt = IASS (Int,Int,[I_Expr],I_Expr)
            | IWHILE (I_Expr,I_Stmt)
            | ICOND (I_Expr,I_Stmt,I_Stmt)
            | IREAD_F (Int,Int,[I_Expr])
            | IREAD_I (Int,Int,[I_Expr])
            | IREAD_B (Int,Int,[I_Expr])
            | IPRINT_F I_Expr
            | IPRINT_I I_Expr
            | IPRINT_B I_Expr
            | IRETURN I_Expr
            | IBLOCK ([I_FunBody],Int,[(Int,[I_Expr])],[I_Stmt])
            deriving (Eq, Show, Ord, Read)
           
data I_Expr = IINT Int
            | IREAL Float
            | IBOOL Bool
            | IID (Int,Int,[I_Expr])   
            | IAPP (I_Op,[I_Expr])
            | ISIZE (Int,Int,Int)
            deriving (Eq, Show, Ord, Read)
           
data I_Op = ICALL (String,Int)
           | IADD_F | IMUL_F | ISUB_F | IDIV_F | INEG_F
           | ILT_F  | ILE_F  | IGT_F  | IGE_F  | IEQ_F
           | IADD | IMUL | ISUB | IDIV | INEG
           | ILT  | ILE  | IGT  | IGE  | IEQ 
           | INOT | IAND | IOR | IFLOAT | ICEIL |IFLOOR
            deriving (Eq, Show, Ord, Read)
    
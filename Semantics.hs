{-  CPSC 411 Assignment 4 semantics checker
    Author: Xin Yan (Jack) Xie
    Apr 7, 2018
-}
module Semantics where

import AST
import ST
import IR
import GenIR

-- Type checking to match proper data types for different operations
checkType :: M_Op -> M_Expr -> ST -> Bool
checkType op e st = (case op of
    M_Add -> elem eType [M_Int, M_Real]
    M_Mul -> elem eType [M_Int, M_Real]
    M_Sub -> elem eType [M_Int, M_Real]
    M_Div -> elem eType [M_Int, M_Real]
    M_Neg -> elem eType [M_Int, M_Real]
    M_LT -> elem eType [M_Int, M_Real]
    M_LE -> elem eType [M_Int, M_Real]
    M_GT -> elem eType [M_Int, M_Real]
    M_GE -> elem eType [M_Int, M_Real]
    M_EQ -> elem eType [M_Int, M_Real]
    M_Not -> eType == M_Bool
    M_And -> eType == M_Bool
    M_Or -> eType == M_Bool
    M_Float -> eType == M_Int
    M_Floor -> eType == M_Real 
    M_Ceil -> eType == M_Real)
        where
            eType = getType e st

checkSameType :: ST -> String -> M_Expr -> Bool
checkSameType st name expr = v
    where
        sym_i_desc1 = look_up st name
        type1 = getTypeID sym_i_desc1        
        type2 = getTypeExpr st expr
        
        v1 = checkExpr expr st
        v2 = type1 == type2
        v = v1 && v2
        
-- Type getter (returns the type of the relevant expression)
getType :: M_Expr -> ST -> M_Type
getType e st = case e of
    M_Ival _ -> M_Int
    M_Rval _ -> M_Real
    M_Bval _ -> M_Bool
    M_Size (str, x) -> M_Int
    M_Id (str,exs) -> t
        where
            I_VARIABLE (_,_,t,_) = look_up st str
    
    M_App (op, e:exs) -> case op of 
        M_Fn str -> t
            where 
            I_FUNCTION (_,_,_,t) = look_up st str
        M_Add -> getType e st
        M_Mul -> getType e st
        M_Sub -> getType e st
        M_Div -> getType e st
        M_Neg -> getType e st
        M_LT -> M_Bool
        M_LE -> M_Bool
        M_GT -> M_Bool
        M_GE -> M_Bool
        M_EQ -> M_Bool
        M_Not -> M_Bool
        M_And -> M_Bool
        M_Or -> M_Bool
        M_Float -> M_Real
        M_Floor -> M_Int 
        M_Ceil -> M_Int

checkAllType :: ST -> [M_Expr] -> Bool
checkAllType st [] = True
checkAllType st (e:exs) = (case notSame of
        [] -> True
        _ -> False)
    where
        t1 = getType e st
        notSame = [x | x <- exs, not ((getType x st) == t1)]      
        
checkInts :: ST -> [M_Expr] -> Bool
checkInts st exs = (case not_ints of
    [] -> True
    _ -> False)
    where
        not_ints = [ x | x <- exs, not (checkInt st x)]
        
checkInt :: ST -> M_Expr -> Bool
checkInt st e = case e of
    M_Ival _ -> True                        -- Only return true for integer types
    M_Rval _ -> False
    M_Bval _ -> False
    M_Size (str, x) -> True
    M_Id (str,exs) -> checkInts st exs 
    M_App (_, exs) -> checkInts st exs   

checkExpr :: M_Expr -> ST -> Bool
checkExpr exp st = case exp of 
            M_Ival _ -> True
            M_Rval _ -> True
            M_Bval _ -> True
            M_Size (str, x) -> True
            M_Id (str,exs) -> checkInts st exs                   
            M_App (op, exs) -> (case op of
                M_Fn str -> v
                    where
                      --  I_FUNCTION (Int,String,[(M_Type,Int)],M_Type)
                        I_FUNCTION (_,_,f_args,_) = look_up st str
                        f_types = map (\(a_type, _) -> a_type) f_args
                        in_args = exs
                        in_types = map (\a -> getType a st) in_args 
                        v = in_types == f_types
                        e:rest = exs
                _ -> v
                    where
                        e:rest = exs
                        v1 = checkAllType st exs
                        v2 = checkType op e st
                        v = v1 && v2)
                    
checkProg :: M_Prog -> Bool
checkProg (M_Prog (decls, stmts)) = v
   where
     st = new_scope L_PROG []
     (n, st1, v1) = checkDecls decls (1, st)    
     v2 = checkStmts stmts (n,st1)
     v = v1 && v2

checkDecls :: [M_Decl] -> (Int, ST) -> (Int, ST, Bool)
checkDecls [] (n,st) = (n, st, True)
checkDecls d_list@(decl:decls) (n,st) = (n2,st2,v)
     where 
        varList = filter (\a -> is_var a) d_list
        funList = filter (\a -> not (is_var a)) d_list       
        (decl':decls') = varList ++ funList
        (n1,st1,v1) = checkDecl decl' (n,st)
        (n2,st2,v2) = checkDecls decls' (n1,st1)
        v = v1 && v2
        
checkDecl :: M_Decl -> (Int, ST) -> (Int, ST, Bool)
checkDecl decl (n,st) = case decl of 
    M_Var _ -> checkVar decl (n,st)
    M_Fun _ -> checkFun decl (n,st)
    
checkVar :: M_Decl -> (Int, ST) -> (Int, ST, Bool)
checkVar (M_Var (name, arr_exprs, typ)) (n, st) = (n', st', v)
    where       
        v = checkInts st arr_exprs
        dim = length arr_exprs
        (n', st') = insert n st (VARIABLE (name, typ, dim))

checkFun :: M_Decl -> (Int, ST) -> (Int, ST, Bool)
checkFun (M_Fun (name, args, ret_type, decls, stmts)) (n, st) = (n2, st1, v)
    where       
        sym_args = map (\(nam, dim, typ) -> (typ, dim)) args
        (n1, st1) = insert n st (FUNCTION (name, sym_args, ret_type))
        
        st2 = new_scope (L_FUN ret_type) st1
        
        (st3,v1) = checkArgs n1 st2 args
        (n2, st4, v2) = checkDecls decls (n1,st3) 
        v3 = checkStmts stmts (n2,st4) 
        v = v1 && v2 && v3 
        
    
checkArgs :: Int -> ST -> [(String,Int,M_Type)] -> (ST, Bool)
checkArgs n st [] = (st, True)
checkArgs n st (arg:rest) = (st'',v)
    where
        (st',v1) = checkArg n st arg
        (st'',v2) = checkArgs n st' rest
        v = v1 && v2
        
checkArg :: Int -> ST -> (String,Int,M_Type) -> (ST, Bool)
checkArg n st (name, dim, typ) = (st', True)
    where
        (n, st') = insert n st (ARGUMENT (name, typ, dim))
        
checkStmts :: [M_Stmt] -> (Int,ST) -> Bool
checkStmts [] (n,st) = True
checkStmts (stmt:rest) (n,st) = v
    where
        (n1, st1, v1) = checkStmt stmt (n,st)
        v2 = checkStmts rest (n1,st1)
        v = v1 && v2
            
checkStmt :: M_Stmt -> (Int,ST) -> (Int, ST, Bool)
checkStmt stmt (n,st) = case stmt of
    M_Ass (name, arrs, exp) -> (case (look_up st name) of
        I_VARIABLE (_,_,v_type,_) -> (n,st,v)
        I_FUNCTION _ -> (n,st,False))
        where   
            I_VARIABLE (_,_,v_type,_) = look_up st name 
            v1 = checkExpr exp st
            eType = getType exp st
            v2 = v_type == eType
            v = v1 && v2
    M_While (exp, stmt) -> (n',st', exp' && stmt')
        where
            exp' = checkExpr exp st
            (n',st',stmt') = checkStmt stmt (n,st)  
    M_Cond (e, s1, s2) -> (n'', st'', (eType == M_Bool) && e' && s1' && s2')
        where
            eType = getType e st
            e' = checkExpr e st
            (n',st', s1')   = checkStmt s1 (n,st)
            (n'', st'', s2') = checkStmt s2 (n',st')
    M_Read (name, arrs) -> (case typ of
            M_Int  -> (n,st, True)
            M_Bool -> (n,st, True)
            M_Real -> (n,st, True))
        where
            (I_VARIABLE (lvl, off, typ, _)) = look_up st name
            arrs' = transExprs arrs st
            loc = (lvl, off, arrs')
            
    M_Print (e) -> (case e of 
        M_Ival v -> (n,st, True)
        M_Rval v -> (n,st, True)
        M_Bval v -> (n,st, True)     
        M_Size v -> (n,st, True)     
        M_App v  -> (n,st, (checkExpr e st))
        M_Id (v, exs)  -> (n,st, (checkInts st exs)))
    M_Block (decls, stmts) -> (n', st, v)
        where  
            st' = new_scope L_BLK st
            (n', st'', v1) = checkDecls decls (n,st')
            v2 = checkStmts stmts (n',st'')
            v = v1 && v2
    M_Return e -> (n,st,v)
        where
            rType = checkReturn st              --get return type of the symbol table
            eType = getType e st
            e' = checkExpr e st
            v = e' && (eType == rType)
            
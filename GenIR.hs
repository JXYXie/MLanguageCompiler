{-  CPSC 411 Assignment 4 IR generator
    Author: Xin Yan (Jack) Xie
    Apr 7, 2018
    Converts the generated AST into IR
-}
module GenIR where

import ST
import IR
import AST

--Transforms the M_Prog into I_Prog
transProgIR :: M_Prog -> I_Prog
-- M_prog (declarations, statements) = IPROG (functions, local variable length, arrays, statements)
transProgIR (M_Prog (decls, stmts)) = IPROG (funcs, numvars, arrays, stmts')
  where
    st = new_scope L_PROG []
    (funcs, numvars, arrays, st') = transDeclsIR decls (1, st)
    stmts' = transStmtsIR stmts st'
    
--Transforms M_Decl of a function into I_FunBody
transDeclIR :: M_Decl -> (Int, ST) -> ([I_FunBody], Int, [(Int, [I_Expr])], (Int, ST))
transDeclIR decl (n,st) = case decl of
    M_Var _ -> transVarIR decl (n,st)       -- If we are declaring a variable
    M_Fun _ -> transFunIR decl (n,st)       -- If we are declaring a function
    
transDeclsIR :: [M_Decl] -> (Int, ST) -> ([I_FunBody], Int, [(Int, [I_Expr])], (Int, ST))
transDeclsIR [] (n,st) = ([], 0, [], (n, st))
transDeclsIR (decl:decls) (n,st) = (funcs, numvars, arrays, (n'', st''))
     where
        varList = filter (\a -> is_var a) (decl:decls)
        funList = filter (\a -> not (is_var a)) (decl:decls)
        (decl':decls') = varList ++ funList
        (funcs1, numvars1, arrays1, (n', st')) = transDeclIR decl' (n,st)
        (funcs2, numvars2, arrays2, (n'', st'')) = transDeclsIR decls' (n',st')
        funcs = funcs1 ++ funcs2
        numvars = numvars1 + numvars2
        arrays = arrays1 ++ arrays2

-- Same as above but for variable declarations instead
transVarIR :: M_Decl -> (Int, ST) -> ([I_FunBody], Int, [(Int, [I_Expr])], (Int, ST))
transVarIR (M_Var (name, arr_exprs, typ)) (n, st) = ([], 1, array, (n', st'))
    where
        dim = length arr_exprs
        (n', st') = insert n st (VARIABLE (name, typ, dim))
        array = case arr_exprs of
            [] -> []
            _ -> [ (offset, arr_exprs')]
                where
                    (I_VARIABLE (level, offset, _type, dim)) = look_up st' name
                    arr_exprs' = transExprs arr_exprs st'
                    
transFunIR :: M_Decl -> (Int, ST) -> ([I_FunBody], Int, [(Int, [I_Expr])], (Int, ST))
transFunIR (M_Fun (name, args, ret_type, decls, stmts)) (n, st) = ([func], 0, [], (n4, st2))
    where
        sym_args = map (\(nam, dim, typ) -> (typ, dim)) args
        (n2, st2) = insert n st (FUNCTION (name, sym_args, ret_type))
        st3 = new_scope (L_FUN ret_type) st2
        (n3, st4) = insertArgs args (n2, st3)
        num_args = length args
        I_FUNCTION (st_level, st_label, st_args, st_type) = look_up st4 name
        (dec_funcs, dec_num_vars, dec_arrays, (n4, st5)) = transDeclsIR decls (n3,st4)
        stmts' = transStmtsIR stmts (n4, st5)

        func = IFUN (st_label, dec_funcs, dec_num_vars, length st_args, dec_arrays, stmts')
        
-- Insert the arguments
insertArgs :: [(String, Int, M_Type)] -> (Int, ST) -> (Int, ST)
insertArgs [] (n,st) = (n, st)
insertArgs (a:rest) (n,st) = (n'', st'')
    where
        (n', st') = insertArg a (n,st)
        (n'', st'') = insertArgs rest (n',st')

insertArg :: (String, Int, M_Type) -> (Int, ST) -> (Int, ST)
insertArg (name, dim, typ) (n,st) = (n', st')
    where
        (n', st') = insert n st (ARGUMENT (name, typ, dim))
        
-- [M_Stmt] into [I_Stmt]
transStmtsIR :: [M_Stmt] -> (Int,ST) -> [I_Stmt]
transStmtsIR [] (n,st) = []
transStmtsIR (stmt:rest) (n,st) = st3
    where
        ((n', st'), st1) = transStmtIR stmt (n,st)
        st2 = transStmtsIR rest (n',st')
        st3 = st1:st2
-- M_Stmt into I_Stmt
transStmtIR :: M_Stmt -> (Int,ST) -> ((Int, ST), I_Stmt)
transStmtIR stmt (n,st) = case stmt of
    M_Ass (name, arrs, exp) -> ((n,st), IASS (lvl, off, arrs', exp'))
        where
            (I_VARIABLE (lvl, off, _, _)) = look_up st name
            arrs' = transExprs arrs st
            exp' = transExpr exp st
    M_While (exp, stmt) -> ((n',st'), IWHILE (exp', stmt'))
        where
            exp' = transExpr exp st
            ((n',st'),stmt') = transStmtIR stmt (n,st)
    M_Cond (e, s1, s2) -> ((n'', st''), ICOND (e', s1', s2'))
        where
            e' = transExpr e st
            ((n',st'), s1')   = transStmtIR s1 (n,st)
            ((n'', st''), s2') = transStmtIR s2 (n',st')
    M_Read (name, arrs) -> (case typ of
            M_Int  -> ((n,st), IREAD_I loc)
            M_Bool -> ((n,st), IREAD_B loc)
            M_Real -> ((n,st), IREAD_F loc))
        where
            (I_VARIABLE (lvl, off, typ, _)) = look_up st name
            arrs' = transExprs arrs st
            loc = (lvl, off, arrs')
    M_Print (e) -> (case e of
        M_Ival val -> ((n,st), IPRINT_I (IINT (fromIntegral val)))
        M_Rval val -> ((n,st), IPRINT_F (IREAL val))
        M_Bval val -> ((n,st), IPRINT_B (IBOOL val))
        M_Size val -> ((n,st), IPRINT_I (transExpr e st))
        M_App val  -> ((n,st), IPRINT_I (transExpr e st))
        M_Id val -> (case (transId e st) of
            M_Int -> ((n,st),IPRINT_I (transExpr e st))
            M_Real -> ((n,st),IPRINT_F (transExpr e st))
            M_Bool -> ((n,st),IPRINT_B (transExpr e st))))
    M_Block (decls, stmts) -> ((n', st), IBLOCK (dec_funcs, dec_num_vars, dec_arrays, stmts'))
        where
            st' = new_scope L_BLK st
            (dec_funcs, dec_num_vars, dec_arrays, (n', st'')) = transDeclsIR decls (n,st')
            stmts' = transStmtsIR stmts (n',st'')
    M_Return e -> ((n,st), IRETURN e')
        where
            e' = transExpr e st
            
-- [M_Expr] into [I_Expr]
transExprs :: [M_Expr] -> ST -> [I_Expr]
transExprs [] st = []
transExprs (e:es) st = ies
    where       
        ies = (transExpr e st):(transExprs es st)
-- M_Expr into I_Expr
transExpr :: M_Expr -> ST -> I_Expr
transExpr e st = case e of
    M_Ival val -> IINT (fromIntegral val) 
    M_Rval val -> IREAL val 
    M_Bval val -> IBOOL val 
    M_Size (str, dim) -> ISIZE (lvl, off, dim)
        where           
            (I_VARIABLE (lvl, off, _, d)) = look_up st str
    M_Id (str, es) -> IID (lvl, off, es')
        where
            (I_VARIABLE (lvl, off, _, dim)) = look_up st str
            es' = transExprs es st
    M_App (op, ess) -> IAPP (op', ess')
        where
            (e:es) = ess
            op' = transOpIR op e st
            ess' = transExprs (ess) st
            
-- M_ID into I_ID
transId :: M_Expr -> ST -> M_Type
transId (M_Id (name, arr_exps)) st = typ
    where
        I_VARIABLE (lvl, off, typ, dim) = look_up st name

-- Distinguish kind of ID (Function or Variable?)
getTypeID :: SYM_I_DESC -> M_Type
getTypeID sym = case sym of
    I_VARIABLE (_,_,t,_) -> t
    I_FUNCTION (_,_,_,t) -> t

-- Gets the type from M_Expr
getTypeExpr :: ST -> M_Expr -> M_Type
getTypeExpr st exp = case exp of
            M_Ival _ -> M_Int               -- Self explanatory
            M_Rval _ -> M_Real
            M_Bval _ -> M_Bool
            M_Size _ -> M_Int
            M_Id (str,exps) -> type'
                where 
                    desc = look_up st str   -- look up the ID in the symbol table
                    type' = getTypeID desc
            M_App (_, e:exps) -> getTypeExpr st e
            
-- M_Op into I_Op matching
transOpIR :: M_Op -> M_Expr -> ST -> I_Op
transOpIR op opnd st = case op of
    M_Fn str ->  ICALL  (label, lvl)                                -- If the operation is a function call
        where
            (I_FUNCTION (lvl, label, _, _)) = look_up st str        -- Look up the function in the symbol table
    M_Add -> (case opnd of                                             -- If the OP is addition
        M_Ival val -> IADD                                            -- if operand is an integer perform Integer addition
        M_Rval val -> IADD_F                                          -- If it is a real (floating point) perform float addition
        M_Size val -> IADD
        M_Id val -> (case (transId opnd st) of
            M_Int -> IADD
            M_Real -> IADD_F)
        M_App (op, opnd:exs) -> (case (getTypeExpr st opnd) of
            M_Int -> IADD
            M_Real -> IADD_F))
    M_Mul -> (case opnd of                                          -- Multiplication
        M_Ival val -> IMUL
        M_Rval val -> IMUL_F
        M_Size val -> IMUL
        M_Id val -> (case (transId opnd st) of
            M_Int -> IMUL
            M_Real -> IMUL_F)
        M_App (op, opnd:exs) -> (case (getTypeExpr st opnd) of
            M_Int -> IMUL
            M_Real -> IMUL_F))
    M_Sub -> (case opnd of                                          -- Subtraction
        M_Ival val -> ISUB
        M_Rval val -> ISUB_F
        M_Size val -> ISUB
        M_Id val -> (case (transId opnd st) of
            M_Int -> ISUB
            M_Real -> ISUB_F)
        M_App (op, opnd:exs) -> (case (getTypeExpr st opnd) of
            M_Int -> ISUB
            M_Real -> ISUB_F))  
    M_Div -> (case opnd of                                          -- Division
        M_Ival val -> IDIV
        M_Rval val -> IDIV_F
        M_Size val -> IDIV
        M_Id val -> (case (transId opnd st) of
            M_Int -> IDIV
            M_Real -> IDIV_F)
        M_App (op, opnd:exs) -> (case (getTypeExpr st opnd) of
            M_Int -> IDIV
            M_Real -> IDIV_F))
    M_Neg -> (case opnd of                                          -- Negation
        M_Ival val -> INEG
        M_Rval val -> INEG_F
        M_Size val -> INEG
        M_Id val -> (case (transId opnd st) of
            M_Int -> INEG
            M_Real -> INEG_F)
        M_App (op, opnd:exs) -> (case (getTypeExpr st opnd) of
            M_Int -> INEG
            M_Real -> INEG_F))
    M_LT -> (case opnd of                                           -- Less than
        M_Ival val -> ILT
        M_Rval val -> ILT_F
        M_Size val -> ILT
        M_Id val -> (case (transId opnd st) of
            M_Int -> ILT
            M_Real -> ILT_F)
        M_App (op, opnd:exs) -> (case (getTypeExpr st opnd) of
            M_Int -> ILT
            M_Real -> ILT_F))
    M_LE -> (case opnd of
        M_Ival val -> ILE
        M_Rval val -> ILE_F
        M_Size val -> ILE
        M_Id val -> (case (transId opnd st) of
            M_Int -> ILE
            M_Real -> ILE_F)
        M_App (op, opnd:exs) -> (case (getTypeExpr st opnd) of
            M_Int -> ILE
            M_Real -> ILE_F))
    M_GT -> (case opnd of
        M_Ival val -> IGT
        M_Rval val -> IGT_F
        M_Size val -> IGT
        M_Id val -> (case (transId opnd st) of
            M_Int -> IGT
            M_Real -> IGT_F)
        M_App (op, opnd:exs) -> (case (getTypeExpr st opnd) of
            M_Int -> IGT
            M_Real -> IGT_F))
    M_GE -> (case opnd of
        M_Ival val -> IGE
        M_Rval val -> IGE_F
        M_Size val -> IGE
        M_Id val -> (case (transId opnd st) of
            M_Int -> IGE
            M_Real -> IGE_F)
        M_App (op, opnd:exs) -> (case (getTypeExpr st opnd) of
            M_Int -> IGE
            M_Real -> IGE_F))
    M_EQ -> (case opnd of
        M_Ival val -> IEQ
        M_Rval val -> IEQ_F
        M_Size val -> IEQ
        M_Id val -> (case (transId opnd st) of
            M_Int -> IEQ
            M_Real -> IEQ_F)
        M_App (op, opnd:exs) -> (case (getTypeExpr st opnd) of
            M_Int -> IEQ
            M_Real -> IEQ_F))
    M_Not   -> INOT
    M_And   -> IAND
    M_Or    -> IOR
    M_Float -> IFLOAT
    M_Floor -> IFLOOR
    M_Ceil  -> ICEIL
    x -> error ("Invalid OP: transOpIR " ++ (show x))             --semantics error if the OP is not matched
    
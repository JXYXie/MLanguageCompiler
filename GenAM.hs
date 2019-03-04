{-  CPSC 411 Assignment 4 Stack machine code generation file
    Generates stack code from the Intermediate Representation
    Author: Xin Yan (Jack) Xie
    Apr 13, 2018
-}

module GenAM where
import IR

neg     = "NEG"
mul     = "MUL"
add     = "ADD"

-- Load instructions
loadI n = "\t" ++ "LOAD_I " ++ (show n)             -- Integer
loadR s = "\t" ++ "LOAD_R " ++ s                    -- Real
loadF s = "\t" ++ "LOAD_F " ++ (show s)             -- Floating point
loadB n = "\t" ++ "LOAD_B " ++ n                    -- Boolean
loadO n = "\t" ++ "LOAD_O " ++ (show n)             -- Offset
loadOS  = "\t" ++ "LOAD_OS"                         -- LOAD_OS

-- Read instructions
readI = "\t" ++ "READ_I"
readF = "\t" ++ "READ_F"
readB = "\t" ++ "READ_B"

-- Print instructions
printI = "\t" ++ "PRINT_I"
printF = "\t" ++ "PRINT_F"
printB = "\t" ++ "PRINT_B"

-- Store instructions
storeI s = "\t" ++ "STORE_I "  ++ s
storeR s = "\t" ++ "STORE_R "  ++ s
storeB s = "\t" ++ "STORE_B "  ++ s
storeO n = "\t" ++ "STORE_O " ++ (show n) 

-- Jump instructions
jump s  = "\t" ++ "JUMP " ++ s
jumpS   = "\t" ++ "JUMP_S"
jumpC s = "\t" ++ "JUMP_C " ++ s                    -- jump to code label

label s = "label" ++ (show s)
code_label s = (label s) ++ ":"

alloc n = "\t" ++ "ALLOC "  ++ (show n)
allocS  = "\t" ++ "ALLOC_S"

app s   = "\t" ++ "APP " ++ s
halt    = "\t" ++ "HALT"

-- Loads the dimension
loadDim :: Int -> Int -> [String]
loadDim m_a 1 = [loadR "%fp", loadO m_a, loadO 0]       -- Inital LOAD_R %fp with dimension of 1
loadDim m_a dim = [loadR "%fp", loadO m_a, loadO (dim-1)] ++ (loadDim m_a (dim-1)) ++ [app mul]     -- if dimension greater than 1

-- Converts I_Prog to stack code
genAM_Prog :: I_Prog -> String
genAM_Prog (IPROG (funbodies,vars,arrays,stmts)) = printStack prog
    where
        prog = init ++ array ++ body ++ exit ++ funs                        -- Inital stack code + Array + Code body + deallocation + functions
        init = [loadR "%sp", loadR "%sp", storeR "%fp", alloc vars, loadI (vars+2)]
        array       = genAM_Arrays vars arrays
        body        = sts
        (n1, sts)   = genAM_Stmts 1 stmts
        (n2, funs)  = genAM_Funs n1 funbodies
        exit        = [loadR "%fp", loadO (vars+1), app neg, allocS, halt]  -- Deallocates and halts

-- Array generation function to put elements in arrays on stack
genAM_Array :: Int -> Int -> (Int, [I_Expr]) -> [String]
genAM_Array _ 0 _ = error "Error: Improper array dimension:"    -- cant have a dimension of 0
genAM_Array m dims (m_a,e:[]) = s1 ++ s2 ++ s3          -- otherwise 
    where
        s1 = genAM_Expr e                               -- convert the expression
        s2 = (loadDim m_a dims) ++                      -- and put the array dimensions, and array size on stack
            [loadR "%fp", loadO (m+1),                  -- then dealloc stack counter
            loadI dims,                                 -- LOAD_I dimension
            loadR "%fp", loadO m_a, loadO dims,         -- LOAD_R %fp
            app add, app add,
            loadR "%fp", storeO (m+1)]
        s3 = [allocS]                                   -- ALLOC_S
genAM_Array m dims (m_a,(e:es)) = s1 ++ s2 ++ (genAM_Array m (dims+1) (m_a,es))
    where
        s1 = genAM_Expr e                               -- first dimension on stack
        s2 = [loadR "%sp", loadR "%fp", storeO m_a]     -- get the array pointer
        
-- Array generation recursive handler
genAM_Arrays :: Int -> [(Int, [I_Expr])] -> [String]
genAM_Arrays m [] = []                                  -- If array is empty then it is empty
genAM_Arrays m (a:rest) = (genAM_Array m 1 a) ++ (genAM_Arrays m rest)    -- Otherwise take the first element of array and put it on stack

-- Generating function bodies am code from I_FUN
genAM_Fun :: Int -> I_FunBody -> (Int, [String])
genAM_Fun x (IFUN (label, fb, vars, args, arrays, stmts)) = 
        (x2, lbl ++ init ++ array ++ stmts' ++ retVal ++ retPointer ++ exit ++ restore ++ morefun)
    where
        (x1, stmts') = genAM_Stmts x stmts
        n = args                        -- get the number of arguments
        m = vars                        -- and number of local variables
        
        lbl     = [label ++ ":"]                        -- print out the label
        init    = [loadR "%sp", storeR "%fp", alloc n]  -- initial LOAD_R %sp, STORE_R %fp and ALLOC instructions at the start of functions
        array   = genAM_Arrays m arrays                 -- get the array stack code
        retVal = [loadR "%fp", storeO (-(n+3))]         -- get the return value
        retPointer = [loadR "%fp", loadO 0, loadR "%fp", storeO (-(n+2))]   -- and the return pointer
        exit    = [alloc (-(m+1))]                      -- and then exits out the function with ALLOC
        restore = [storeR "%fp", alloc (-n), jumpS]     -- and return back to main calling code using STORE
        (x2,morefun) = genAM_Funs x1 fb

-- function code recursive call handler
genAM_Funs :: Int -> [I_FunBody] -> (Int, [String])
genAM_Funs n [] = (n, [])                               -- No function bodies
genAM_Funs n (f:fs) = (n2, s1 ++ s2)                    -- otherwise get the first function and generate the codes
    where 
        (n1, s1) = genAM_Fun n f
        (n2, s2) = genAM_Funs n1 fs

-- calculates static/access link
calcStatic :: Int -> [String] 
calcStatic 0 = [loadR "%fp"]
calcStatic n = (calcStatic (n-1)) ++ [loadO (-2)]

-- Transforms I_Expr (expression) to stack machine code
genAM_Expr :: I_Expr -> [String]
genAM_Expr e = case e of
    IINT x -> [loadI x]                 -- Int
    IREAL x -> [loadF x]                -- Floating
    IBOOL x -> (case x of               -- Boolean
        True -> [loadB "true"]
        False -> [loadB "false"])
    IID (lvls, offsets, es) -> calcStatic lvls ++ [loadO offsets]   -- get the identifier and calculate the static/access link and then LOAD_O (offset)
    ISIZE (lvl, offset, dim) -> loadFP ++ ld                        -- get the size of the array
        where
            loadFP = calcStatic lvl                                 -- first get the static/access link
            ld = [loadO offset, loadO dim]                          -- LOAD_O offset
            
    IAPP (op, es) -> (case op of
        ICALL (label,lvls) -> init ++ void ++ static ++ call
            where
                m = length es
                -- Caller code
                init    = genAM_Exprs es                            -- first convert the expressions IR into stack code 
                void  = [alloc 1]                                   -- void on stack
                static  = calcStatic lvls                           -- calculates the static/access link
                call    = [loadR "%fp", loadR "%cp", jump label]    -- set dynamic link, save the program counter, jump to the function code that is being called
        
        -- get the corresponding load instruction with the proper operation instruction
        IADD   -> load ++ [app "ADD" ]
        ISUB   -> load ++ [app "SUB"]
        IMUL   -> load ++ [app "MUL"]
        IDIV   -> load ++ [app "DIV"]
        INEG   -> load ++ [app "NEG"]
        IEQ    -> load ++ [app "EQ"]
        ILT    -> load ++ [app "LT"]
        ILE    -> load ++ [app "LE"]
        IGT    -> load ++ [app "GT"]
        IGE    -> load ++ [app "GE"]
        
        IADD_F -> load ++ [app "ADD_F"]
        ISUB_F -> load ++ [app "SUB_F"]
        IMUL_F -> load ++ [app "MUL_F"]
        IDIV_F -> load ++ [app "DIV_F"]
        INEG_F -> load ++ [app "NEG_F"]
        IEQ_F  -> load ++ [app "EQ_F" ]
        ILT_F  -> load ++ [app "LT_F" ]
        ILE_F  -> load ++ [app "LE_F" ]
        IGT_F  -> load ++ [app "GT_F" ]
        IGE_F  -> load ++ [app "GE_F" ]
        
        INOT   -> load ++ [app "NOT"]
        IAND   -> load ++ [app "AND"]
        IOR    -> load ++ [app "OR"]
        IFLOAT -> load ++ [app "FLOAT"]
        ICEIL  -> load ++ [app "CEIL"]
        IFLOOR -> load ++ [app "FLOOR"]
        )
            where
                load = genAM_Exprs es                       -- Find out the type of load instruction
                
-- expressions code recursive call handler
genAM_Exprs :: [I_Expr] -> [String]
genAM_Exprs [] = []                                         -- no expressions
genAM_Exprs (e:rest) = (genAM_Expr e)++(genAM_Exprs rest)   -- otherwise generate the stack code for an expression and iterate to next one

-- Convert statements into AM code
genAM_Stmt :: Int -> I_Stmt -> (Int, [String])
genAM_Stmt n s = case s of
    IASS (lvl,offset,es,e) -> (n, a ++ fp ++ b ++ c)
        where
            fp = calcStatic lvl
            a = genAM_Expr e
            b = genAM_Exprs es
            c = [storeO offset]
    -- While statement
    -- Generate the label number as well as the code label to jump to
    IWHILE (e,stmt) -> (n1, [code_label n] ++ (genAM_Expr e) ++ [jumpC (label n)] ++ exp ++ [jump (label n)])
        where
            (n1,exp) = genAM_Stmt (n+1) stmt
    -- If then else statement and their code label generation
    ICOND (e,s1,s2) -> (n2, (genAM_Expr e) ++ [jumpC (label n)] ++ exp1 ++ [jump (label (n+1))] ++
            [code_label n] ++ exp2 ++ [code_label (n+1)]) 
        where
            (n1,exp1)   = genAM_Stmt (n+2) s1
            (n2,exp2)   = genAM_Stmt n1 s2
    IREAD_F (lvl,offset,es) -> (n, [readF, loadR "%fp", storeO offset])
    IREAD_I (lvl,offset,es) -> (n, [readI, loadR "%fp", storeO offset])
    IREAD_B (lvl,offset,es) -> (n, [readB, loadR "%fp", storeO offset])
    IPRINT_F x -> (n, (genAM_Expr x) ++ [printF])
    IPRINT_I x -> (n, (genAM_Expr x) ++ [printI])
    IPRINT_B x -> (n, (genAM_Expr x) ++ [printB])
    IRETURN e -> (n, genAM_Expr e)
    IBLOCK (fbodies,vars,arrs,stmts) -> (n1, enter ++ body ++ exit)
        where
            m = vars
            enter = [loadR "%fp", alloc 1, loadR "%sp", storeR "%fp", 
                alloc m, loadI (m+2), allocS]
            (n1,body) = genAM_Stmts n stmts
            exit = [loadR "%fp", loadO (m+1), app neg, allocS]
    
-- statement code recursive handler
genAM_Stmts :: Int -> [I_Stmt] -> (Int,[String])
genAM_Stmts n [] = (n, [])
genAM_Stmts n (s:rest) = (n2, first++next)
    where
        (n1, first) = genAM_Stmt n s
        (n2, next) = genAM_Stmts n1 rest
        
-- Split up the list and print them into proper stack code format separated by newlines
printStack :: [String] -> String
printStack [] = ""
printStack (s:rest) = s ++ "\n" ++ (printStack rest)

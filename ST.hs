{-  CPSC 411 Assignment 4 Symbol Table file
    Author: Xin Yan (Jack) Xie
    Apr 7, 2018
    Part of this is take from Robin Cockett's website http://pages.cpsc.ucalgary.ca/~robin/class/411/M+/m+.symbol-table.spec
    contains Symbol Table datatype and its main functions
-}
module ST where

import AST

data SYM_DESC = ARGUMENT (String, M_Type,Int)               -- Argument(name, type, dimension)
              | VARIABLE (String, M_Type,Int)               -- Variable (name, type, dimension)
              | FUNCTION (String, [(M_Type,Int)], M_Type)   -- Function (function name,[(type,dimension)], return type)
              deriving (Eq, Show, Ord, Read)

data SYM_I_DESC = I_VARIABLE (Int, Int, M_Type, Int)
              | I_FUNCTION (Int,String,[(M_Type,Int)],M_Type)
              deriving (Eq, Show, Ord, Read)
              
data ScopeType = L_PROG
              | L_FUN M_Type
              | L_BLK
              deriving (Eq, Show, Ord, Read)
              
data SYM_VALUE = VAR_attr (Int,M_Type,Int)
              | FUN_attr (String, [(M_Type,Int)], M_Type)
              deriving (Eq, Show, Ord, Read)
              
              
data SYM_TABLE = Symbol_table (ScopeType, Int, Int, [(String,SYM_VALUE)]) --(Scopetype, num of variables , num of arguments, [(Name,SYM_VALUE)])
                deriving (Eq, Show, Ord, Read)

type ST = [SYM_TABLE]

empty:: ST
empty = []

new_scope :: ScopeType -> ST -> ST
new_scope t s = (Symbol_table(t,0,0,[])) : s

-- Symbol Table insert symbol function
insert :: Int -> ST -> SYM_DESC -> (Int,ST) 
insert n [] d = error "Symbol Table error: inserting symbol outside of scope."                      -- If symbol insertion is out of scope

insert n ((Symbol_table(sT, nL, nA, sL)):rest) (ARGUMENT(str, t, dim))                              -- Inserting an argument
       | (inTable str sL) = error ("Symbol Table error: " ++ str ++ " is already defined.")         -- check to see if it is in symbol table already
       | otherwise = (n, Symbol_table(sT, nL, nA+1, (str,VAR_attr(negate(nA + 4),t,dim)) : sL) : rest)  -- if not insert it

insert n ((Symbol_table(sT, nL, nA, sL)):rest) (VARIABLE (str,t,dim))                               -- Inserting a variable
       | (inTable str sL) = error ("Symbol Table error: "++ str ++ " is already defined.")
       | otherwise = (n,Symbol_table(sT, nL+1,nA,(str,VAR_attr(nL+1,t,dim)):sL) : rest)
       
insert n ((Symbol_table(sT, nL, nA, sL)):rest) (FUNCTION (str,ts,t))                                -- Inserting a function
       | inTable str sL = error ("Symbol Table error: " ++ str ++ " is already defined.")
       | otherwise = (n+1,(Symbol_table(sT, nL,nA,(str,FUN_attr(getLabel n "fn",ts,t)):sL)):rest)
       
-- Look up function
look_up :: ST -> String -> SYM_I_DESC 
look_up s x = find 0 s 
   where
      found level (VAR_attr(offset,type_,dim)) 
                    =  I_VARIABLE(level,offset,type_,dim)
      found level (FUN_attr(label,arg_Type,type_)) 
                    = I_FUNCTION(level,label,arg_Type,type_)
      find_level ((str,v):rest)|x== str = Just v
                               |otherwise =  find_level rest
      find_level [] = Nothing

      find n [] = error ("Could not find "++ x)
      find n (Symbol_table(_,_,_,vs):rest) = 
             (case find_level vs of         
               Just v -> found n v          
               Nothing -> find (n+1) rest)  
               
-- function that checks if symbol is already in symbol table
inTable :: String -> [(String, SYM_VALUE)] -> Bool
inTable str [] = False
inTable str ((x,_):xs)
      | str == x = True
      | otherwise = inTable str xs
      
-- Gets the name
getLabel :: Int -> String -> String
getLabel n str = str ++ (show n) 

remove_scope :: ST -> (Int, ST)
remove_scope [] = (0, [])
remove_scope (s:rest) = (nL, rest)
    where
        Symbol_table (_, nL, _, _) = s

checkReturn :: ST -> M_Type
checkReturn [] = error "Semantic error: Invalid return type"
checkReturn (s:rest) = case sT of
    L_FUN t -> t
    x -> error "Semantic error: Invalid return type"
    where 
        Symbol_table (sT,_,_,_) = s

-- variable checker
is_var :: M_Decl -> Bool
is_var m = case m of 
    M_Var _ -> True
    _ -> False

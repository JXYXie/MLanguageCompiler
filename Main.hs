{-  CPSC 411 Assignment 5 stack code generation
    Author: Xin Yan (Jack) Xie
    Apr 7, 2018
    Generates stack code from generated intermediate representation
-}
module Main where

import System.Environment
import LexM
import ParM
import SkelM
import PrintM
import AbsM
import ErrM
import AST
import ST
import IR
import GenIR
import GenAM
import Semantics
import Text.Show.Pretty

main = do
    args <- getArgs
    let fname = args !! 0           -- Gets the file name
    fconts <- readFile fname        -- And reads the file
    let tokens = myLexer fconts     -- Then lexical analyze the contents of file
    let ptree = pProg tokens
    case ptree of                   -- Parse correctly?
        Ok tree -> do               -- If so display message
            putStrLn "\nParse Successful!"
            let asTree = transProg tree         -- Takes the AST
            let semCheck = checkProg asTree     -- Initializes the symbol table and begin semantics checking
            let intRep = transProgIR asTree     -- Transform the AST into IR
            let amCode = genAM_Prog intRep      -- Now transform the IR into stack machine code
            
            case semCheck of                    -- Are there any semantics errors?
                False -> do                     -- If so
                    putStrLn "\n-----------AST-----------"
                    putStrLn $ (ppShow) asTree -- Just print out the AST
                    putStrLn "\nError generating the IR, semantics error!"
                True -> do                      -- No semantic errors
                    putStrLn "\n-----------AST-----------"
                    putStrLn $ (ppShow) asTree      -- Pretty show the AST
                    putStrLn "\n-----------IR-----------"
                    putStrLn $ (ppShow) intRep      -- and pretty show the IR
                    writeFile "output.am" amCode    -- write the stack code to file
                    putStrLn "\nThe AM code has been generated and stored in \"output.am\"."
                    
        Bad emsg -> putStrLn emsg           -- Otherwise syntax error
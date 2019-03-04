/******************************READ ME******************************/
Xin Yan Xie
10160975

To use
    
    1. First make sure haskell pretty show package is installed. 
    To do this use the command: "cabal install pretty-show""
    
    2. If you are on a
    
        Linux System:
            Run the command:    "make all"
    
        Windows system:
            Run the commands:   "happy -gca ParM.y"
                                "alex -g LexM.x"
                                "ghc --make Main.hs -o Main"
            In command prompt
            
    3. Usage:
        
        On Linux, run the command:  "./Main TESTFILE.m+"
        
        On Windows, just use        "Main TESTFILE.m+"
        
        The resulting AST and IR will be printed in the terminal, the AM stack code will be outputed to file "output.am"
        
        For example, to run Dr Cockett's first test the command would be:
            "Main \RobinCockettTests\test1.m+"
    
Note:
    There are 5 self made test files included with this assignment along with Dr Cockett's test files from his website
    They are located in \MyTests and \RobinCockettTests respectively
    This assignment prints both the AST and the corresponding IR
    The generated AM code is stored in "output.am"
    
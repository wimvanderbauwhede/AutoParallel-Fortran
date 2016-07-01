module F95StatementParser  ( parseF95Statement ) where

-- import whatever you need
import Language.Fortran.Parser
import Language.Fortran
-- Must be compiled with -i../language-fortran/src/

-- This function takes a single line of F95, no preprocessing is required from your side
-- It returns the string resulting from calling `show` on the parsed expression
parseF95Statement :: String -> String -> String
parseF95Statement f95_line context_str = (show fortranAst) -- ++ "\n" ++ (show contextAst)
		where
			fortranAst = statement_parse f95_line
			contextAst = context_parse context_str
-- Lanauge fortran doesn't include type information with assignments and my compiler only
-- considers types when evaluating iterator value ranges during loop analysis. 

-- parse_with_your_parser :: String -> Context -> (AST, Context)    
-- readContext :: String -> Context    
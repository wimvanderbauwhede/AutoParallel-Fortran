module Main where

import Language.Fortran.Parser
import Language.Fortran
import LanguageFortranTools
import F95StatementParser
import System.Environment
import qualified Data.Map as DMap

main = do
    args <- getArgs
    let
        arg = head args
        if_expr = parseF95Statement arg
    print if_expr    
    print $ show if_expr
    print "--------------------------------"
    let
        If _ _ readExpr _ _ _  = if_expr
        exprs = extractOperands readExpr
        exprs' = map (
                \e -> case e of 
                    Var _ _ v -> "Var "++(show v )
                    Con _ _ c -> "Con "++(show c)
                    e' -> show e'
                ) exprs
    mapM  print  exprs'

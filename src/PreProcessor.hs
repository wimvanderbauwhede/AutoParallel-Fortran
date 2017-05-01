module PreProcessor (preProcess)

where

--    Simple preprocessor used to circumvent some issues with Language-Fortran parser.

import Data.Char
import qualified Data.Map as DMap 

import Warning

preProcess :: Bool -> String -> (String, DMap.Map Int [String])
preProcess False inputStr = replaceIfDefByLabel $ removeBlankLines $ andOperatorFix $ orOperatorFix $ containsStatementFix $ caseStatementFix $ inputStr
preProcess True inputStr = replaceIfDefByLabel $ removeBlankLines $ andOperatorFix $ orOperatorFix $ containsStatementFix $ caseStatementFix $ fixedForm $ inputStr

caseInsensitive_strReplace :: [Char] -> [Char] -> [Char] -> [Char]
caseInsensitive_strReplace original replace str     
    | take (length original) (map (toLower) str) == original     
        = replace ++ caseInsensitive_strReplace original replace (drop (length original) str)
    | str == []        = []
    | otherwise     = (take 1 str) ++ caseInsensitive_strReplace original replace (drop 1 str)

-- removeBlankLines :: String -> String
-- removeBlankLines inputStr = foldl (removeBlankLines_foldl) "" allLines
--         where
--             allLines = lines inputStr

-- removeBlankLines_foldl :: String -> String -> String
-- removeBlankLines_foldl accum item = accum ++ (if all (isSpace) item then "" else item ++ "\n")

-- WV
removeBlankLines inputStr = unlines $ filter (not . (all isSpace)) $ lines inputStr

-- removeBlankLines :: [Char] -> [Char]
-- removeBlankLines [] = []
-- removeBlankLines ('\n':'\n':str) = removeBlankLines ('\n':str)
-- removeBlankLines (char:str) = char:(removeBlankLines str)

caseStatementFix :: String -> String
caseStatementFix input = caseInsensitive_strReplace "\ncase(" "\n case(" (caseInsensitive_strReplace "\ncase " "\n case " input)

containsStatementFix :: String -> String
containsStatementFix input = (caseInsensitive_strReplace "\ncontains" "\n contains " (caseInsensitive_strReplace "\ncontains " "\n contains " input))

orOperatorFix :: String -> String
orOperatorFix input = caseInsensitive_strReplace ".or." " .or. " input

andOperatorFix :: String -> String
andOperatorFix input = caseInsensitive_strReplace ".and." " .and. " input

semiColonFix :: String -> String
semiColonFix input = caseInsensitive_strReplace ";" "\n" input

fixedForm :: String -> String
fixedForm  inputStr = foldl (\accum item -> accum ++ (take 72 item) ++ "\n") "" allLines
        where
            allLines = lines inputStr

replaceIfDefByLabel :: String -> (String, DMap.Map Int [String])
replaceIfDefByLabel inputStr = 
    let
        src_lines = lines inputStr
        -- state            
        -- (add_to_stash, nest_counter, stash, lines_to_stash, lines_to_output, label_counter)
        init_state = (False,0,DMap.empty,[],[],0)
        (add_to_stash, nest_counter, stash, lines_to_stash, lines_to_output, label_counter) = foldl stashLine init_state src_lines
    in   
--        (inputStr, stash)    
        (unlines lines_to_output, stash)

-- logic
stashLine :: (Bool, Int, DMap.Map Int [String], [String], [String], Int) -> String -> (Bool, Int, DMap.Map Int [String], [String], [String], Int)
stashLine (add_to_stash, nest_counter, stash, lines_to_stash, lines_to_output, label_counter) line =
    let
        (nest_counter', add_to_stash') 
            | nest_counter == 0  = if ( 
                line `startsWith` "#ifndef NO_IO" ||
                line `startsWith` "#ifdef DBG" ||
                line `startsWith` "#ifdef TIMINGS" 
               ) then  (1,True) else (0, False)
            | line `startsWith` "#if"  = (nest_counter+1, add_to_stash)
            | line `startsWith` "#endif" = (nest_counter-1,add_to_stash)    
            | otherwise = (nest_counter,add_to_stash)

        (lines_to_stash',lines_to_output') 
            | add_to_stash' = (lines_to_stash++[line],lines_to_output)
            | otherwise = (lines_to_stash,lines_to_output++[line])
    
        (add_to_stash'', stash', lines_to_stash'' , lines_to_output'', label_counter') 
            | nest_counter' == 0 && add_to_stash' =
                let
                    label_counter_ =label_counter+1              
                    stash_ = DMap.insert (7188+label_counter_) lines_to_stash' stash
                    add_to_stash_ = False
                    lines_to_stash_ = []
                    lines_to_output_ = lines_to_output' ++["  "++(show (7188+label_counter_))++" continue"]
                in                  
                    (add_to_stash_, stash_, lines_to_stash_ ,  lines_to_output_, label_counter_)
            | otherwise =
                    (add_to_stash', stash, lines_to_stash' , lines_to_output', label_counter)
    in
        (add_to_stash'', nest_counter', stash', lines_to_stash'', lines_to_output'', label_counter')
    

startsWith line str =  (take (length str) line) == str

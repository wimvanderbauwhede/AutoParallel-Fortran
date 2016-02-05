module Main where

import Data.Generics (Data, Typeable, mkQ, mkT, gmapQ, gmapT, everything, everywhere)
import Language.Fortran.Parser
import Language.Fortran
import Data.Char
import Data.List
import System.Environment
import System.Process
import System.Directory

import PreProcessor
import VarAccessAnalysis
import VarDependencyAnalysis
import LanguageFortranTools
import CodeEmitter

--	Type used to standardise loop analysis functions
--						errors 		reduction variables read variables		written variables		
type AnalysisInfo = 	(String, 	[Expr [String]], 	[Expr [String]], 	[Expr [String]])

main :: IO ()
main = do
	--a <- parseFile "../testFiles/arrayLoop.f95"
	
	putStr "STUFF TO DO:\n"
	putStr "\t- Fix adjacent kernel fusion\n"
	putStr "\t- Add new reduction rule (Depends on self but only once)\n"
	putStr "\t- Code emission\n"
	putStr "\n"

	args <- getArgs
	let filename = args!!0

	--a <- parseFile "../testFiles/arrayLoop.f95"
	parsedProgram <- parseFile filename
	let parallelisedProg = paralleliseProgram (parsedProgram)
	let combinedProg = combineKernels (removeAllAnnotations parallelisedProg)

	--cppd <- cpp filename
	--putStr (cppd)

	putStr $ compileAnnotationListing parallelisedProg
	putStr "\n"
	putStr $ compileAnnotationListing combinedProg
	-- putStr "\n"
	emit (filename) "" parallelisedProg
	--emit (filename) "" combinedProg

	--putStr $ show $ parsedProgram
	--putStr "\n\n\n"

	-- putStr $ show $ parallelisedProg
	-- putStr "\n"

	--putStr "\n"

--	Taken from language-fortran example. Runs preprocessor on target source and then parses the result, returning an AST.
parseFile s = do inp <- readProcess "cpp" [s, "-D", "NO_IO", "-P"] "" 
                 return $ parse $ preProcess inp

cpp s = do 	inp <- readProcess "cpp" [s, "-D", "NO_IO", "-P"] "" 
        	return inp

combineKernels :: Program [String] -> Program [String]
combineKernels codeSeg = map (everywhere (mkT (combineKernelsBlock))) codeSeg

combineKernelsBlock :: Block [String] -> Block [String]
combineKernelsBlock block = combinedAdjacentNested
				where
					combinedNested = everywhere (mkT (combineNestedKernels)) block
					combinedAdjacentNested = everywhere (mkT (combineAdjacentKernels)) combinedNested

combineNestedKernels :: Fortran [String] -> Fortran [String]
combineNestedKernels codeSeg = case codeSeg of
					(OpenCLMap anno1 _ outerReads outerWrites outerLoopVs fortran) -> case fortran of
								FSeq _ src1 (OpenCLMap anno2 _ innerReads innerWrites innerLoopVs innerFortran) (NullStmt _ _) -> 
										OpenCLMap (anno1++anno2++[newAnnotation]) generatedSrcSpan reads writes loopVs innerFortran
											where 
												reads = listRemoveDuplications $ outerReads ++ innerReads
												writes = listRemoveDuplications $ outerWrites ++ innerWrites
												loopVs = listRemoveDuplications $ outerLoopVs ++ innerLoopVs
												newAnnotation = "Nested map at " ++ errorLocationFormatting src1 ++ " fused into surrounding map\n"
								otherwise -> codeSeg

					(OpenCLReduce anno1 _ outerReads outerWrites outerLoopVs outerRedVs fortran) -> case fortran of
								FSeq _ src1 (OpenCLReduce anno2 _ innerReads innerWrites innerLoopVs innerRedVs innerFortran) (NullStmt _ _) -> 
										OpenCLReduce (anno1++anno2++[newAnnotation]) generatedSrcSpan reads writes loopVs redVs innerFortran
											where 
												reads = listRemoveDuplications $ outerReads ++ innerReads
												writes = listRemoveDuplications $ outerWrites ++ innerWrites
												loopVs = listRemoveDuplications $ outerLoopVs ++ innerLoopVs
												redVs = listRemoveDuplications $ outerRedVs ++ innerRedVs
												newAnnotation = "Nested reduction at " ++ errorLocationFormatting src1 ++ " fused into surrounding reduction\n"
								otherwise -> codeSeg
					otherwise -> codeSeg


combineAdjacentKernels :: Fortran [String] -> Fortran [String]
combineAdjacentKernels codeSeg = case codeSeg of
					(FSeq anno1 src1 fortran1 (FSeq anno2 src2 fortran2 fortran3)) -> case fortran1 of
							OpenCLMap _ _ _ _ _ _ -> case fortran2 of
									OpenCLMap _ _ _ _ _ _ -> case attemptCombineAdjacentMaps fortran1 fortran2 of
																Just oclmap -> FSeq (anno1 ++ anno2 ++ [newAnnotation]) src1 oclmap fortran3  
																	where
																		newAnnotation = "Adjacent maps at " ++ errorLocationFormatting src1 ++ " and " ++ errorLocationFormatting src2 ++ " fused\n"
																Nothing -> codeSeg
									otherwise	-> codeSeg
							otherwise	-> codeSeg
					otherwise -> codeSeg

attemptCombineAdjacentMaps:: Fortran [String] -> Fortran [String] -> Maybe(Fortran [String])
attemptCombineAdjacentMaps 	(OpenCLMap anno1 _ reads1 writes1 loopVs1 fortran1) 
							(OpenCLMap anno2 _ reads2 writes2 loopVs2 fortran2) = result
									where
										combinedLoopVars = attemptCombineLoopVariables loopVs1 loopVs2
										result = case combinedLoopVars of
											Just combinedVars -> Just $ OpenCLMap anno generatedSrcSpan reads writes combinedVars fortran
											Nothing -> Nothing
										reads = listRemoveDuplications $ reads1 ++ reads2
										writes = listRemoveDuplications $ writes1 ++ writes2
										fortran = appendFortran_recursive fortran2 fortran1
										anno = anno1 ++ anno2

attemptCombineLoopVariables :: [(VarName [String], Expr [String], Expr [String], Expr [String])] 
									-> [(VarName [String], Expr [String], Expr [String], Expr [String])] 
									-> Maybe ([(VarName [String], Expr [String], Expr [String], Expr [String])])
attemptCombineLoopVariables loopVars1 loopVars2 |	standardLoop1 == standardLoop2 = Just loopVars1
												|	otherwise = Nothing
												where
													standardLoop1 = everywhere (mkT standardiseSrcSpan) loopVars1
													standardLoop2 = everywhere (mkT standardiseSrcSpan) loopVars2

--	Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new parallel (OpenCLMap etc)
--	nodes or the original sub-tree annotated with parallelisation errors.
paralleliseLoop :: [VarName [String]] -> VarAccessAnalysis ->Fortran [String] -> Fortran [String]
paralleliseLoop loopVars accessAnalysis loop 	= case mapAttempt_bool of
										True	-> prependAnnotation mapAttempt_ast ("Map at " ++ errorLocationFormatting (srcSpan loop) ++ "\n")
										False 	-> case reduceAttempt_bool of
													True 	-> prependAnnotation reduceAttempt_ast ("Reduction at " ++ errorLocationFormatting (srcSpan loop) ++ "\n")
													False	-> prependAnnotation reduceAttempt_ast ("\nCannot parallelise loop at " ++ errorLocationFormatting (srcSpan loop) ++ "\n")
													--False	-> appendAnnotation (reduceAttempt_ast) (show $ accessAnalysis)
								--case paralleliseLoop_map loop newLoopVars of 
								--	Just a -> a
								--	Nothing -> loop
								where
									newLoopVars = case getLoopVar loop of
										Just a -> loopVars ++ [a]
										Nothing -> loopVars

									nonTempVars = getNonTempVars (srcSpan loop) accessAnalysis
									dependencies = analyseDependencies accessAnalysis loop
									loopWrites = extractWrites_query loop

									mapAttempt = paralleliseLoop_map loop newLoopVars loopWrites nonTempVars accessAnalysis
									mapAttempt_bool = fst mapAttempt
									mapAttempt_ast = snd mapAttempt

									reduceAttempt = paralleliseLoop_reduce mapAttempt_ast newLoopVars loopWrites nonTempVars dependencies accessAnalysis
									reduceAttempt_bool = fst reduceAttempt
									reduceAttempt_ast = snd reduceAttempt

extractWrites_query :: (Typeable p, Data p) => Fortran p -> [VarName p]
extractWrites_query = everything (++) (mkQ [] extractWrites)

extractWrites :: (Typeable p, Data p) => Fortran p -> [VarName p]
extractWrites (Assg _ _ (Var _ _ list) _) = map (\(varname, exprs) -> varname) list
extractWrites _ = []

--	Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new OpenCLMap nodes or the
--	original sub-tree annotated with reasons why the loop cannot be mapped
paralleliseLoop_map :: Fortran [String] -> [VarName [String]] -> [VarName [String]] -> [VarName [String]] -> VarAccessAnalysis -> (Bool, Fortran [String])
paralleliseLoop_map loop loopVars loopWrites nonTempVars accessAnalysis	|	errors_map == "" 	=	(True,
																	OpenCLMap [] generatedSrcSpan 
													 				--(listRemoveDuplications (listSubtract (getVarNames_query loop) (Prelude.map (\(a, _, _, _) -> a) (loopCondtions_query loop))) ) 
													 				(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames reads_map)) (map (\(a, _, _, _) -> a) (loopCondtions_query loop)))
													 				(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames writes_map)) (map (\(a, _, _, _) -> a) (loopCondtions_query loop)))
																	--(flattenLoopConditions Nothing (VarName [] "g_id") 
																	(loopCondtions_query loop) --)   
																	--(removeLoopConstructs_trans loop))
																	(removeLoopConstructs_recursive loop))
									|	otherwise	=			(False, appendAnnotation loop (outputTab ++ "Cannot map due to:\n" ++ errors_map))
									where
										(errors_map, _, reads_map, writes_map) = analyseLoop_map loopVars loopWrites nonTempVars accessAnalysis loop

--	Function is applied to sub-trees that are loops. It returns either a version of the sub-tree that uses new OpenCLReduce nodes or the
--	original sub-tree annotated with reasons why the loop is not a reduction
paralleliseLoop_reduce ::Fortran [String] -> [VarName [String]] -> [VarName [String]] -> [VarName [String]] -> VarDependencyAnalysis -> VarAccessAnalysis -> (Bool, Fortran [String])
paralleliseLoop_reduce loop loopVars loopWrites nonTempVars dependencies accessAnalysis	|	errors_reduce == "" 	=	(True, 
																									OpenCLReduce [] generatedSrcSpan 
																					 				--(listRemoveDuplications (listSubtract (getVarNames_query loop) (Prelude.map (\(a, _, _, _) -> a) (loopCondtions_query loop))) ) 
																					 				(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames reads_reduce)) (map (\(a, _, _, _) -> a) (loopCondtions_query loop)))
																					 				(listRemoveDuplications $ listSubtract (foldl (++) [] (map extractVarNames writes_reduce)) (map (\(a, _, _, _) -> a) (loopCondtions_query loop)))
																									--(flattenLoopConditions Nothing (VarName [] "g_id") 
																									(loopCondtions_query loop) --)   
																									(listRemoveDuplications (foldl (++) [] (map extractVarNames reductionVariables)))
																									(removeLoopConstructs_recursive loop))
															|	otherwise				=	(False, appendAnnotation loop (outputTab ++ "Cannot reduce due to:\n" ++ errors_reduce))
									where
										(errors_reduce, reductionVariables, reads_reduce, writes_reduce) = analyseLoop_reduce [] loopVars loopWrites nonTempVars dependencies accessAnalysis loop 

--	Function takes a list of loop variables and a possible parallel loop's AST and returns a string that details the reasons why the loop
--	cannot be mapped. If the returned string is empty, the loop represents a possible parallel map
analyseLoop_map :: [VarName [String]] -> [VarName [String]] -> [VarName [String]] -> VarAccessAnalysis -> Fortran [String] -> AnalysisInfo
analyseLoop_map loopVars loopWrites nonTempVars accessAnalysis codeSeg = case codeSeg of
		Assg _ srcspan expr1 expr2 -> combineAnalysisInfo (combineAnalysisInfo expr1Analysis expr2Analysis) ("",[],[],[expr1])
						where
							expr1Analysis = (analyseAccess_map loopVars loopWrites nonTempVars accessAnalysis expr1)
							expr2Analysis = (analyseAccess_map loopVars loopWrites nonTempVars accessAnalysis expr2)
		For _ _ var _ _ _ _ -> foldl combineAnalysisInfo analysisInfoBaseCase childrenAnalysis
						where
							childrenAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_map (loopVars ++ [var]) loopWrites nonTempVars accessAnalysis)) codeSeg)
		_ -> foldl combineAnalysisInfo analysisInfoBaseCase (childrenAnalysis ++ nodeAccessAnalysis)
						where
							nodeAccessAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseAccess_map loopVars loopWrites nonTempVars accessAnalysis)) codeSeg)
							childrenAnalysis = (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_map loopVars loopWrites nonTempVars accessAnalysis)) codeSeg)

analyseLoop_reduce :: [Expr [String]] -> [VarName [String]] -> [VarName [String]] -> [VarName [String]] -> VarDependencyAnalysis -> VarAccessAnalysis -> Fortran [String] -> AnalysisInfo
analyseLoop_reduce condExprs loopVars loopWrites nonTempVars dependencies accessAnalysis codeSeg = case codeSeg of
		Assg _ srcspan expr1 expr2 -> 	combineAnalysisInfo (
										(if (not potentialReductionVar) && isNonTempAssignment then 
											outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ":\t Possible reduction variable (" 
												++ errorExprFormatting expr1 ++ ") is not assigned a value related to itself and does not appear in a preceeding conditional construct\n" 
											else "")
										++
										(if potentialReductionVar && (not dependsOnSelfOnce) then
											outputTab ++ outputTab ++ (errorLocationFormatting srcspan) ++ ":\t Possible reduction variable (" 
												++ errorExprFormatting expr1 ++ ") is related to itself more than once." 
											else "")
										,
										if potentialReductionVar then [expr1] else [],
										extractOperands expr2,
										[expr1])
										usesFullLoopVarError
			where
				writtenExprs = extractOperands expr1
				readOperands = extractOperands expr2
				readExprs = foldl (\accum item -> if isFunctionCall accessAnalysis item then accum ++ (extractContainedVars item) else accum ++ [item]) [] readOperands

				dependsOnSelfOnce = foldl (/=) False (map (\y -> foldl (||) False $ (map (\x -> isIndirectlyDependentOn dependencies y x) writtenVarnames)) readVarnames)


				writtenVarnames = foldl (\accum item -> accum ++ extractVarNames item) [] writtenExprs
				readVarnames 	= foldl (\accum item -> accum ++ extractVarNames item) [] readExprs

				isNonTempAssignment = hasVarName nonTempVars expr1
				referencedCondition = (foldl (||) False $ map (\x -> hasOperand x expr1) condExprs)
				referencedSelf = (hasOperand expr2 expr1)
				dependsOnSelf = dependsOnSelfOnce || (foldl (||) False $ map (\x -> isIndirectlyDependentOn dependencies x x) writtenVarnames) --isIndirectlyDependentOn dependencies 
				usesFullLoopVarError = analyseAccess_reduce loopVars loopWrites nonTempVars accessAnalysis expr1

				--potentialReductionVar = isNonTempAssignment && (referencedSelf || referencedCondition || dependsOnSelfOnce) && ((\(str, _, _, _) -> str == "") usesFullLoopVarError)
				potentialReductionVar = isNonTempAssignment && (referencedSelf || referencedCondition || dependsOnSelf) && ((\(str, _, _, _) -> str == "") usesFullLoopVarError)

		If _ _ expr _ _ _ -> foldl combineAnalysisInfo analysisInfoBaseCase (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce (condExprs ++ [expr]) loopVars loopWrites nonTempVars dependencies accessAnalysis)) codeSeg)
		For _ _ var _ _ _ _ -> foldl combineAnalysisInfo analysisInfoBaseCase (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce condExprs (loopVars ++ [var]) loopWrites nonTempVars dependencies accessAnalysis)) codeSeg)
		_ -> foldl combineAnalysisInfo analysisInfoBaseCase (gmapQ (mkQ analysisInfoBaseCase (analyseLoop_reduce condExprs loopVars loopWrites nonTempVars dependencies accessAnalysis)) codeSeg)

analysisInfoBaseCase :: AnalysisInfo
analysisInfoBaseCase = ("",[],[],[])

combineAnalysisInfo :: AnalysisInfo -> AnalysisInfo -> AnalysisInfo
combineAnalysisInfo accum item = (accumErrors ++ itemErrors, accumReductionVars ++ itemReductionVars, accumReads ++ itemReads, accumWrites ++ itemWrites)
								where
									(accumErrors, accumReductionVars, accumReads, accumWrites) = accum
									(itemErrors, itemReductionVars, itemReads, itemWrites) = item		

analyseAccess_map :: [VarName [String]] -> [VarName [String]] -> [VarName [String]] -> VarAccessAnalysis -> Expr [String] -> AnalysisInfo
analyseAccess_map loopVars loopWrites nonTempVars accessAnalysis expr = (errors, [],[expr],[])
								where
									operands = case fnCall of
											True ->	extractContainedVars expr
											False -> extractOperands expr
									writtenOperands = filter (hasVarName loopWrites) operands
									fnCall = isFunctionCall accessAnalysis expr
									--nonTempWrittenOperands = filter (\x -> not $ hasVarName nonTempVars x) writtenOperands
									nonTempWrittenOperands = filter(hasVarName nonTempVars) writtenOperands
									errors = foldl (++) "" $ map (\item -> 

																if listSubtract loopVars (foldl (\accum item -> accum ++ extractVarNames item) [] (extractContainedVars item)) 
																	/= [] then outputTab ++ outputTab ++ errorLocationFormatting (srcSpan item)  
																	++ ":\t Non temporary, write variable (" ++ errorExprFormatting item ++ ") accessed without use of full loop variable\n"  else "") nonTempWrittenOperands

analyseAccess_reduce :: [VarName [String]] -> [VarName [String]] -> [VarName [String]] -> VarAccessAnalysis -> Expr [String] -> AnalysisInfo
analyseAccess_reduce loopVars loopWrites nonTempVars accessAnalysis expr = (errors, [],[],[])
								where
									operands = case fnCall of
											True ->	extractContainedVars expr
											False -> extractOperands expr
									writtenOperands = filter (hasVarName loopWrites) operands
									fnCall = isFunctionCall accessAnalysis expr
									--nonTempWrittenOperands = filter (\x -> not $ hasVarName nonTempVars x) writtenOperands
									nonTempWrittenOperands = filter(hasVarName nonTempVars) writtenOperands
									errors = foldl (++) "" $ map (\item -> 

																if listSubtract loopVars (foldl (\accum item -> accum ++ extractVarNames item) [] (extractContainedVars item)) 
																	== [] then outputTab ++ outputTab ++ errorLocationFormatting (srcSpan item)  
																	++ ":\t Non temporary, write variable (" ++ errorExprFormatting item ++ ") written to with use of full loop variable\n"  else "") nonTempWrittenOperands

--paralleliseProgram :: (Typeable p, Data p) => ProgUnit p -> ProgUnit p 
paralleliseProgram :: Program [String] -> Program [String] 
paralleliseProgram codeSeg = map (everywhere (mkT (paralleliseBlock (accessAnalysis)))) codeSeg -- map (everywhere (mkT (paralleliseBlock (accessAnalysis)))) codeSeg
	where
		accessAnalysis = analyseAllVarAccess codeSeg

paralleliseBlock :: VarAccessAnalysis -> Block [String] -> Block [String]
paralleliseBlock accessAnalysis block = gmapT (mkT (paralleliseForLoop accessAnalysis)) block
		--where 
		--	accessAnalysis = analyseAllVarAccess block

paralleliseForLoop :: VarAccessAnalysis -> Fortran [String] -> Fortran [String]
paralleliseForLoop  accessAnalysis inp = case inp of
		For _ _ _ _ _ _ _ -> paralleliseLoop [] accessAnalysis $ gmapT (mkT (paralleliseForLoop accessAnalysis )) inp
		_ -> gmapT (mkT (paralleliseForLoop accessAnalysis)) inp

--	Function uses a SYB query to get all of the loop condtions contained within a particular AST. loopCondtions_query traverses the AST
--	and calls getLoopConditions when a Fortran node is encountered.
loopCondtions_query :: (Typeable p, Data p) =>  Fortran p -> [(VarName p, Expr p, Expr p, Expr p)]
loopCondtions_query = everything (++) (mkQ [] getLoopConditions)

getLoopConditions :: (Typeable p, Data p) => Fortran p -> [(VarName p, Expr p, Expr p, Expr p)]
getLoopConditions codeSeg = case codeSeg of
		For _ _ var start end step _ -> [(var, start, end, step)]
		OpenCLMap _ _ _ _ loopVars _ -> loopVars
		OpenCLReduce _ _ _ _ loopVars _ _ -> loopVars
		_ -> []

--	Traverses the AST and prooduces a single string that contains all of the parallelising errors for this particular run of the compiler.
--	compileAnnotationListing traverses the AST and applies getAnnotations to Fortran nodes (as currently they are the only nodes that have
--	ever have annotations applied. The resulting string is then output to the user.
compileAnnotationListing :: Program [String] -> String
compileAnnotationListing codeSeg = everything (++) (mkQ [] getAnnotations) codeSeg

getAnnotations :: Fortran [String] -> String
getAnnotations codeSeg = case tag codeSeg of
	[] -> ""
	_ -> (foldl (++) "" (tag codeSeg))-- ++ "\n"
	--_ -> "Loop at " ++ (errorLocationFormatting (srcSpan codeSeg)) ++ " cannot be parallelised.\n" ++ (foldl (++) "" (tag codeSeg)) ++ "\n"

--	Returns a list of all of the names of variables that are used in a particular AST. getVarNames_query performs the traversal and applies
--	getVarNames at appropriate moments.
getVarNames_query :: (Typeable p, Data p) =>  Fortran p -> [VarName p]
getVarNames_query fortran = everything (++) (mkQ [] getVarNames) fortran

getVarNames :: (Typeable p, Data p) =>  VarName p -> [VarName p]
getVarNames expr = [expr]

--	Generic function that takes two lists a and b and returns a +list c that is all of the elements of a that do not appear in b.
listSubtract :: Eq a => [a] -> [a] -> [a]
listSubtract a b = filter (\x -> notElem x b) a

--	Function checks whether every Expr in a list is a VarName from another list.
exprListContainsVarNames :: (Typeable p, Data p, Eq p) =>  [Expr p] -> [VarName p] -> Bool
exprListContainsVarNames contains container = all (== True) (everything (++) (mkQ [] (varNameCheck container)) contains)

varNameCheck :: (Typeable p, Data p, Eq p) => [VarName p] -> VarName p -> [Bool]
varNameCheck container contains = [elem contains container]

-- 	Function returns the loop variable for an AST representing a for loop
getLoopVar :: Fortran p -> Maybe(VarName p)
getLoopVar (For _ _ var _ _ _ _) = Just var
getLoopVar _ = Nothing

--	Value used as a global spacing measure. Used for output formatting.
outputTab :: String
outputTab = "  "

isAssociativeOp :: BinOp p -> Bool
isAssociativeOp (Plus p) = True
isAssociativeOp (Mul p) = True
isAssociativeOp (Or p) = True
isAssociativeOp _ = False

isAssociativFunction :: String -> Bool
isAssociativFunction "min" = True
isAssociativFunction "max" = True
isAssociativFunction _ = False
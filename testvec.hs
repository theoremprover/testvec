module Main where

import Language.C
import Language.C.System.GCC   -- Preprocessor
import Language.C.Analysis.TravMonad
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.SemRep
import qualified Data.Map as Map
import Control.Monad
import System.Environment

main = do
	[sourcefile] <- getArgs
	parseMyFile sourcefile >>= analyze

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file = do
	parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
	case parse_result of
		Left parse_err -> error (show parse_err)
		Right ast      -> return ast

analyze :: CTranslUnit -> IO ()
analyze ctu = do
	let (Right (globaldecls,[])) = runTrav_ (analyseAST ctu)
	forM_ (Map.toList $ gObjs globaldecls) $ \ (ident,decl) -> do
		case decl of
			FunctionDef (FunDef (VarDecl (VarName ident _) declattrs ty) stmt nodeinfo) -> do
				putStrLn $ show ident
			_ -> return ()
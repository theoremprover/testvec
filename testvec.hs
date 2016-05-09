{-# LANGUAGE LambdaCase #-}

module Main where

import Language.C
import Language.C.System.GCC   -- Preprocessor
import Language.C.Analysis.TravMonad
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.SemRep
import Language.C.Analysis.DefTable
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
analyze (CTranslUnit extdecls nodeinfo) = do
	forM_ extdecls $ \case
		CFDefExt fundef -> analyzeFunDef fundef
		CDeclExt decl -> analyzeDecl decl
		CAsmExt asm _ -> error "Found CAsmExt"

analyzeDecl (CDecl declspecs diss nodeinfo) = do
	forM_ diss $ \case
		(Just declr,_,Nothing)
		
	[(Maybe (CDeclarator a), Maybe (CInitializer a), Maybe (CExpression a))]
	print "

analyzeFunDef (CFunDef declspecs cdeclr cdecls stmt nodeinfo) =

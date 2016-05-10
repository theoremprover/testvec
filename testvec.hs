{-# LANGUAGE LambdaCase #-}

module Main where

import Language.C
import Language.C.System.GCC   -- Preprocessor
import Language.C.Data.Ident
import qualified Data.Map as Map
import Control.Monad
import System.Environment
import Text.Printf
import Text.PrettyPrint

main = do
	let sourcefile = "vfprintf.i" --[sourcefile] <- getArgs
	maini sourcefile

maini sourcefile = parseMyFile sourcefile >>= analyze

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file = do
	parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
	case parse_result of
		Left parse_err -> error (show parse_err)
		Right ast      -> return ast

analyze :: CTranslUnit -> IO ()
analyze (CTranslUnit extdecls nodeinfo) = do
	forM_ extdecls $ \case
		CDeclExt decl   -> analyzeDecl decl
		CFDefExt fundef -> analyzeFunDef fundef
		CAsmExt asm _   -> error "Found CAsmExt"

analyzeDecl (CDecl declspecs diss nodeinfo) = do
	forM_ diss $ \case
		_ -> return ()

analyzeFunDef (CFunDef declspecs (CDeclr (Just (Ident name _ _)) _ _ _ _) cdecls stmt nodeinfo) = do
	putStrLn "--------------------------------------"
	print name
	paths <- followPaths stmt
	print paths

followPaths stmt = case stmt of
	CExpr (Just cexpr) nodeinfo -> 
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

notImplYet x = error $ show x ++ " not implemented yet"

analyze :: CTranslUnit -> IO ()
analyze (CTranslUnit extdecls nodeinfo) = do
	forM_ extdecls $ \ extdecl -> case
		CDeclExt decl   -> analyzeDecl decl
		CFDefExt fundef -> analyzeFunDef fundef
		CAsmExt asm _   -> notImplYet extdecl

analyzeDecl (CDecl declspecs diss nodeinfo) = do
	forM_ diss $ \case
		_ -> return ()

-- Possible InputValues is a list of value ranges
type InputValues a = Any | Ranges [(a,a)]
	deriving (Show,Eq)

class SymbValueRepr a where
	

analyzeFunDef (CFunDef declspecs (CDeclr (Just (Ident name _ _)) _ _ _ _) cdecls stmt nodeinfo) = do
	putStrLn "--------------------------------------"
	print name
	paths <- followStmt Map.empty stmt
	print paths
	
followStmt inputvals stmt = case stmt of
	CExpr (Just cexpr) nodeinfo -> followExpr inputvals cexpr
	_ -> notImplYet stmt

followExpr inputvals cexpr = case cexpr of
	CAssign assignop (CVar (Ident name _ _) _) assignedexpr _ -> 
	_ -> notImplYet cexpr


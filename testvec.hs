{-# LANGUAGE LambdaCase,ExistentialQuantification #-}

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
	let sourcefile = "test.c" --[sourcefile] <- getArgs
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
	forM_ extdecls $ \ extdecl -> case extdecl of
		CDeclExt decl   -> analyzeDecl decl
		CFDefExt fundef -> analyzeFunDef fundef
		CAsmExt asm _   -> notImplYet extdecl

analyzeDecl (CDecl declspecs diss nodeinfo) = do
	forM_ diss $ \case
		_ -> return ()

-- Possible InputValues is a list of value ranges
data InputValues a = Any | Ranges [(a,a)]
	deriving (Show,Eq)

type InputTypes a = Map.Map Ident (CTypeSpecifier a)

class SymbValueRepr a where
	
printIdent (Ident name _ _) = putStrLn name

class (Show a) => ShowAST a where
	showAST :: Int -> a -> [String]

showIndent :: Int -> String -> [String]
showIndent i s = [ concat (take i (repeat "| ")) ++ s ]
showASTList :: (ShowAST a) => Int -> [a] -> [String]
showASTList i l = showIndent (i+1) "[" ++ concatMap (showAST (i+2)) l ++ showIndent (i+1) "]"

instance (Show a) => ShowAST (CFunctionDef a) where
	showAST i (CFunDef declspecs cdeclr cdecls stmt _) =
		showIndent i "CFunDef" ++
		showASTList (i+1) declspecs ++
		showAST (i+1) cdeclr ++
		showASTList (i+1) cdecls ++
		showAST (i+1) stmt

instance (Show a) => ShowAST (CDeclarationSpecifier a) where
	showAST i (CTypeSpec ctypespec) =
		showIndent i "CTypeSpec" ++
		showAST (i+1) ctypespec
	showAST _ x = notImplYet x

instance (Show a) => ShowAST (CTypeSpecifier a) where
	showAST i (CIntType _) = showIndent i "int"

instance (Show a) => ShowAST (CDeclarator a) where
	showAST i (CDeclr mb_ident cderivdeclrs mb_strlit cattribs _ =
		showIndent i "CDeclr" ++
		showAST (i+1) mb_ident ++
		showASTList (i+1) cderivdeclrs ++
		showAST (i+1) mb_strlit ++
		showASTList (i+1) cattribs

instance (ShowAST a) => ShowAST (Maybe a) where
	showAST i (Just a) = "Just " ++ showAST i a
	showAST i (Nothing)

printAST ast = putStrLn (unlines $ showAST ast)

analyzeFunDef c@(CFunDef declspecs (CDeclr (Just ident) derivdeclrs mb_strlit attrs _) cdecls stmt _) = do
	putStrLn "--------------------------------------"
	printAST c
{-
	paths <- followStmt inputs stmt
	print paths
	where
	inputs = concatMap extracttype declspecs
	extracttype declspec = case declspec of
		CTypeSpec ctypespec -> 
followStmt inputs stmt = case stmt of
	CExpr (Just cexpr) nodeinfo -> followExpr inputs cexpr
	_ -> notImplYet stmt

followExpr inputs cexpr = case cexpr of
	CAssign assignop (CVar ident _) assignedexpr _ -> 
	_ -> notImplYet cexpr

-}


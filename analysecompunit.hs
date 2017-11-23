{-# LANGUAGE FlexibleInstances,FlexibleContexts #-}

module Main where

import GHC.Generics
import Language.C
import Language.C.Pretty
import Language.C.System.GCC   -- Preprocessor
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad
import Language.C.Analysis.AstAnalysis
import Language.C.Analysis.TravMonad
import Language.C.Analysis.SemRep
import Language.C.Data.Ident
import System.Environment
import Control.Monad.State.Strict

--import Text.Printf
--import Text.PrettyPrint
--import Text.Show.Pretty

fileUnderTest = "test2.c" -- "D:\\testvec\\newlib_src\\newlib-1.18.0\\newlib\\libc\\stdio\\vfprintf.c"
includeFiles = [] --["-ID:\\testvec\\newlib_src\\newlib-1.18.0\\newlib\\libc\\include"]
funname = "f"
t = maini fileUnderTest includeFiles funname

main = do
	[sourcefile,includefiles_str,funname] <- getArgs
	maini sourcefile (break_at_comma includefiles_str) funname
	where
	break_at_comma s = case span (/=',') s of
		(s1,"") -> [s1]
		(s1,s2) -> s1 : break_at_comma (drop 1 s2)

maini sourcefile includefiles funname = do
	globalscope <- parseMyFile sourcefile includefiles
	a <- evalStateT (analyseFunM [("res",Any)] funname) globalscope
	print a

type GlobalScope = [(String,FunDef)]

parseMyFile :: FilePath -> [FilePath] -> IO GlobalScope
parseMyFile input_file includefiles = do
	parse_result <- parseCFile (newGCC "gcc") Nothing includefiles input_file
	case parse_result of
		Left parse_err -> error (show parse_err)
		Right ctranslunit -> case runTrav [] $ analyseAST ctranslunit of
			Left errs -> error $ show errs
			Right (globdecls,travstate) -> do
				resultss <- forM (Map.assocs (gObjs globdecls)) $ \ (Ident name _ _,identdecl) -> return $
					case identdecl of
						FunctionDef fundef -> Just (name,fundef)
						_                  -> Nothing
				return $ catMaybes resultss

notImpl x = error $ "Not yet implemented: " ++ show x

data Values = Any | Not Values | Values [Values] | Interval Values Values
	deriving (Show,Eq)

type Env = [(String,Values)]

type AnalyseM a = StateT GlobalScope IO a

analyseFunM :: Env -> String -> AnalyseM Env
analyseFunM env funname = do
	mb_fundef <- gets (lookup funname)
	case mb_fundef of
		Nothing -> error $ "Could not find function " ++ show funname
		Just (FunDef funname_decl stmt a) -> case stmt of
			CIf cond if_stmt else_stmt a -> analyseBoolExpr
			CReturn mb_expr a -> [show mb_expr]
			CCompound labels compound_items a -> analyseCompoundItems vals (reverse compound_items) a
			_ -> notImpl stmt
{-

analyseCompoundItems vals (compound_item:cis) a = case compound_item of
	CBlockStmt stmt -> analyseStmt vals stmt
	CBlockDecl (CDecl _ decls a) -> analyseDecls vals (reverse decls) a
	_ -> notImpl compound_item

analyseDecls vals (decl:decls) a =

analyseStmt vals stmt = case stmt of
	CCompound labels compound_items a -> analyseCompoundItems vals (reverse compound_items) a
	CIf cond then_stmt mb_else_stmt a -> 
	CReturn (Just expr) a -> reverseExpr vals expr a
	_ -> notImpl stmt
	
reverseExpr vals expr a = case expr of
	CVar ident a -> 
-}
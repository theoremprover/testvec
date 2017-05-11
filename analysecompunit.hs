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
funname = "h"
t = maini fileUnderTest includeFiles funname

main = do
	[sourcefile,includefiles_str,funname] <- getArgs
	maini sourcefile (break_at_comma includefiles_str) funname
	where
	break_at_comma s = case span (/=',') s of
		(s1,"") -> [s1]
		(s1,s2) -> s1 : break_at_comma (drop 1 s2)

maini sourcefile includefiles funname = do
	globalscope <- parseMyFile sourcefile includefiles[("return",Values [Atomic 5])]
	let resultenv = 
	a <- evalStateT (analyseFunM funname resultenv) $ AnalyseS resultenv globalscope
	print a

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

{-
-}

notImpl x = error $ "Not yet implemented: " ++ show x

data Values = Any | Not Values | Values [Values] | Interval Values Values | Atomic Int
	deriving (Show,Eq)

type Env = [(String,Values)]

type AnalyseM a = StateT GlobalScope IO a

data AnalyseS = AnalyseS {
	globalScopeS    :: [(String,FunDef)],
	requiredResultS :: Env }

--analyseFunM :: Env -> String -> AnalyseM Env
analyseFunM funname resultenv = do
	mb_fundef <- gets (lookup funname)
	case mb_fundef of
		Nothing -> error $ "Could not find function " ++ show funname
		Just (FunDef funname_decl stmt a) -> analyseStmt resultenv stmt

analyseStmt resultenv stmt = case stmt of
--	CIf cond if_stmt else_stmt a -> do
--		return $ show cond
	CReturn (Just expr) a -> do
		req_res <- gets requiredResultS
		reverseExpr req_res expr
	CCompound labels compound_items a -> analyseCompoundItems compound_items resultenv
	_ -> return resultenv --notImpl stmt

analyseCompoundItems cis resultenv = foldrM analyseCompoundItem resultenv cis

analyseCompoundItem compound_item resultenv = case compound_item of
	CBlockStmt stmt -> analyseStmt env stmt
	--CBlockDecl (CDecl _ decls a) -> analyseDecls vals (reverse decls) a
	_ -> return resultenv --notImpl compound_item

reverseExpr resultenv expr = case expr of
	CBinary CSubOp expr1 expr2 a -> 
	CVar (CIdent name _ _) a -> 
	CConst (CIntConst (CInteger const _ _) a) -> 
{-

analyseDecls vals (decl:decls) a =

reverseExpr vals expr a = case expr of
	CVar ident a -> 
-}
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
--import Text.Printf
--import Text.PrettyPrint
--import Text.Show.Pretty

fileUnderTest = "test2.c" -- "D:\\testvec\\newlib_src\\newlib-1.18.0\\newlib\\libc\\stdio\\vfprintf.c"
includeFiles = [] --["-ID:\\testvec\\newlib_src\\newlib-1.18.0\\newlib\\libc\\include"]
t = maini fileUnderTest includeFiles

main = do
	[sourcefile,includefiles_str] <- getArgs
	maini sourcefile (break_at_comma includefiles_str)
	where
	break_at_comma s = case span (/=',') s of
		(s1,"") -> [s1]
		(s1,s2) -> s1 : break_at_comma (drop 1 s2)

maini sourcefile includefiles = parseMyFile sourcefile includefiles >>= analyze (Not []) >>= print

parseMyFile :: FilePath -> [FilePath] -> IO CTranslUnit
parseMyFile input_file includefiles = do
	parse_result <- parseCFile (newGCC "gcc") Nothing includefiles input_file
	case parse_result of
		Left parse_err -> error (show parse_err)
		Right ast      -> return ast

data Values v = Not (Values v) | Values (Values v) | Interval (Values v) (Values v)
	deriving (Show,Eq)

--analyze :: CTranslUnit -> IO GlobalDecls
analyze vals ctranslunit = do
	case runTrav [] $ analyseAST ctranslunit of
		Left errs -> error $ show errs
		Right (globdecls,travstate) -> do
			resultss <- forM (Map.assocs (gObjs globdecls)) $ \ (Ident name _ _,identdecl) -> return $ case identdecl of
				FunctionDef fundef -> Just $ analyseFunBody vals fundef
				_ -> Nothing
			return $ catMaybes resultss

analyseFunBody vals (FunDef vardecl stmt a) = case stmt of
	CIf cond if_stmt else_stmt a -> [show cond]
	CReturn mb_expr a -> [show mb_expr]
	CCompound labels compound_items a -> analyseCompoundItems vals compound_items a
	_ -> error $ "Not yet implemented: " ++ show stmt

analyseCompoundItems vals compound_items a = 
{-# LANGUAGE LambdaCase,StandaloneDeriving,FlexibleInstances,DeriveGeneric,FlexibleContexts,DeriveFoldable,GADTs #-}

module Main where

import GHC.Generics
import Language.C
import Language.C.Pretty
import Language.C.System.GCC   -- Preprocessor
--import Language.C.Data.Ident
import qualified Data.Map as Map
import Control.Monad
--import Data.Foldable hiding (forM_)
--import Language.C.Analysis.TravMonad
import System.Environment
--import Text.Printf
import Text.PrettyPrint

import Text.Show.Pretty

main = do
	let sourcefile = "D:\\testvec\\newlib_src\\newlib-1.18.0\\newlib\\libc\\stdio\\vfprintf.c" --[sourcefile] <- getArgs
	maini sourcefile

maini sourcefile = parseMyFile sourcefile >>= analyze

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file = do
	parse_result <- parseCFile (newGCC "gcc") Nothing ["-ID:\\testvec\\newlib_src\\newlib-1.18.0\\newlib\\libc\\include"] input_file
	case parse_result of
		Left parse_err -> error (show parse_err)
		Right ast      -> return ast

analyze :: CTranslUnit -> IO ()
analyze ctranslunit = do
	print $ pretty ctranslunit
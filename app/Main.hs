module Main where

import Language.C
import Language.C.System.GCC   -- Preprocessor
import System.Environment
import Language.C.Syntax.AST

import Testvec

main = do
	let sourcefile = "test\\test.c" --[sourcefile] <- getArgs
	maini sourcefile

maini sourcefile = parseMyFile sourcefile >>= analyze

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file = do
	parse_result <- parseCFile (newGCC "gcc") Nothing ["-ID:\\testvec\\newlib_src\\newlib-1.18.0\\newlib\\libc\\include"] input_file
	case parse_result of
		Left parse_err -> error (show parse_err)
		Right ast      -> return ast

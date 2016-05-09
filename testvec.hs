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
		CFDefExt fundef -> handleTravError $ analyseFunDef fundef
		CDeclExt decl -> return Nothing
		CAsmExt asm _ -> error "Found CAsmExt"
	getErrors >>= sequence_ . (map print)

	globdefs <- liftM globalDefs getDefTable
	forM_ (Map.toList $ gObjs globdefs) $ \case
		(ident,Declaration decl) -> print ident
		_ -> return ()

{-
    mapRecoverM_ analyseExt decls
    -- check we are in global scope afterwards
    getDefTable >>= \dt -> when (not (inFileScope dt)) $
        error "Internal Error: Not in filescope after analysis"
    -- get the global definition table (XXX: remove ?)
    
    where
    mapRecoverM_ f = mapM_ (handleTravError . f)
-}
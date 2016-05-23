{-# LANGUAGE LambdaCase,StandaloneDeriving,FlexibleInstances,DeriveGeneric #-}

module Main where

import GHC.Generics
import Language.C
import Language.C.System.GCC   -- Preprocessor
import Language.C.Data.Ident
import qualified Data.Map as Map
import Control.Monad
import System.Environment
import Text.Printf
import Text.PrettyPrint

import Text.Show.Pretty

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

deriving instance Generic NodeInfo
instance PrettyVal NodeInfo where
	prettyVal nodeinfo = String ""
deriving instance Generic (CFunctionDef a)
instance PrettyVal (CFunctionDef NodeInfo)
deriving instance Generic Language.C.Name
instance PrettyVal Language.C.Name
deriving instance Generic (CDeclaration a)
instance PrettyVal (CDeclaration NodeInfo)
instance PrettyVal Position where
	prettyVal pos = String (show pos)
instance PrettyVal Ident where
	prettyVal (Ident s _ _) = String s
deriving instance Generic (CDeclarationSpecifier a)
instance PrettyVal (CDeclarationSpecifier NodeInfo)
deriving instance Generic (CDeclarator a)
instance PrettyVal (CDeclarator NodeInfo)
deriving instance Generic (CStatement a)
instance PrettyVal (CStatement NodeInfo)
deriving instance Generic (CInitializer a)
instance PrettyVal (CInitializer NodeInfo)
deriving instance Generic (CStorageSpecifier a)
instance PrettyVal (CStorageSpecifier NodeInfo)
deriving instance Generic (CDerivedDeclarator a)
instance PrettyVal (CDerivedDeclarator NodeInfo)
deriving instance Generic (CTypeSpecifier a)
instance PrettyVal (CTypeSpecifier NodeInfo)
deriving instance Generic (CStringLiteral a)
instance PrettyVal (CStringLiteral NodeInfo)
deriving instance Generic (CAssemblyStatement a)
instance PrettyVal (CAssemblyStatement NodeInfo)
deriving instance Generic (CExpression a)
instance PrettyVal (CExpression NodeInfo)
deriving instance Generic (CArraySize a)
instance PrettyVal (CArraySize NodeInfo)
deriving instance Generic (CCompoundBlockItem a)
instance PrettyVal (CCompoundBlockItem NodeInfo)
deriving instance Generic (CAttribute a)
instance PrettyVal (CAttribute NodeInfo)
deriving instance Generic (CEnumeration a)
instance PrettyVal (CEnumeration NodeInfo)
deriving instance Generic CString
instance PrettyVal CString
deriving instance Generic (CAssemblyOperand a)
instance PrettyVal (CAssemblyOperand NodeInfo)
deriving instance Generic (CBuiltinThing a)
instance PrettyVal (CBuiltinThing NodeInfo)
deriving instance Generic (CStructureUnion a)
instance PrettyVal (CStructureUnion NodeInfo)
deriving instance Generic (CTypeQualifier a)
instance PrettyVal (CTypeQualifier NodeInfo)
deriving instance Generic (CConstant a)
instance PrettyVal (CConstant NodeInfo)
deriving instance Generic CAssignOp
instance PrettyVal CAssignOp
deriving instance Generic (CPartDesignator a)
instance PrettyVal (CPartDesignator NodeInfo)
deriving instance Generic CStructTag
instance PrettyVal CStructTag
deriving instance Generic CChar
instance PrettyVal CChar
deriving instance Generic CBinaryOp
instance PrettyVal CBinaryOp
deriving instance Generic CFloat
instance PrettyVal CFloat
deriving instance Generic CUnaryOp
instance PrettyVal CUnaryOp
deriving instance Generic CInteger
instance PrettyVal CInteger
deriving instance Generic CIntRepr
instance PrettyVal CIntRepr
deriving instance Generic CIntFlag
instance PrettyVal CIntFlag
deriving instance Generic (Flags a)
instance PrettyVal (Flags CIntFlag)

data InputValues v = Ranges [(v,v)] | Not [v]
	deriving (Show,Eq)

type InputTypes a = Map.Map Ident (CTypeSpecifier a)

showType ty = case ty of
	CIntType _ -> "int"
	_ -> notImplYet ty

printParams params = do
	forM_ params $ \ (Ident name _ _,(ty,vals)) -> do
		putStrLn $ printf "  %12s :: %s  in  %s" name (showType ty) (show vals)

analyzeFunDef c@(CFunDef declspecs (CDeclr (Just (Ident name _ _)) derivdeclrs mb_strlit attrs _) cdecls stmt _) = do
	putStrLn $ "## CFunDef " ++ name
--	putStrLn $ dumpStr c
	writeFile (name++".html") $ htmlPage defaultHtmlOpts (valToHtml defaultHtmlOpts $ prettyVal c)

	let [ CFunDeclr (Right (paramdecls,_)) _ _ ] = derivdeclrs
	params <- foldlM followDecl [] paramdecls
	mapM_ print $ map (\(Ident name _ _,tv) -> (name,tv)) parameters
	inputvals <- followStmt params stmt
	printParams inputvals

followDecl params (CDecl [CTypeSpec ty] [(Just (CDeclr (Just ident) _ _ _ _),_,_)] _) =
	return $ (ident,(ty,Not [])) : params

followStmt params stmt = case stmt of
	CCompound [] blockitems _ -> followBlockItems params blockitems
	CBlockDecl (
	CExpr (Just cexpr) nodeinfo -> followExpr params cexpr
	_ -> notImplYet stmt

followBlockItems params [] = return params
followBlockItems params (blockitem:bss) = case blockitem of
	CBlockStmt stmt -> do
		params' <- followStmt params stmt
		followBlockItems params' bss
	_ -> notImplYet blockitem

followExpr params cexpr = case cexpr of
	CAssign assignop (CVar ident _) assignedexpr _ -> 
	_ -> notImplYet cexpr

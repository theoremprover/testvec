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

{-
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
showASTList i l = showIndent i "[" ++ concatMap (showAST (i+1)) l ++ showIndent (i+1) "]"

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
	showAST i (CDeclr mb_ident cderivdeclrs mb_strlit cattribs _) =
		showIndent i "CDeclr" ++
		showAST (i+1) mb_ident ++
		showASTList (i+1) cderivdeclrs ++
		showAST (i+1) mb_strlit ++
		showASTList (i+1) cattribs

instance (Show a) => ShowAST (CDeclaration a) where
	showAST i (CDecl declspecs declinitexprs _) =
		showIndent i "CDecl" ++
		showASTList (i+1) declspecs ++
		showASTList (i+1) declinitexprs

instance (ShowAST a,ShowAST b,ShowAST c) => ShowAST (a,b,c) where
	showAST i (a,b,c) =
		showIndent i "(" ++
		showAST (i+1) a ++
		showIndent (i+1) "," ++
		showAST (i+1) b ++
		showIndent (i+1) "," ++
		showAST (i+1) c ++
		showIndent i ")"

instance (ShowAST a) => ShowAST (Maybe a) where
	showAST i (Just a) = showIndent i "Just" ++ showAST (i+1) a
	showAST i Nothing  = showIndent i "Nothing"

printAST ast = putStrLn (unlines $ showAST ast)

-}

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

analyzeFunDef c@(CFunDef declspecs (CDeclr (Just (Ident name _ _)) derivdeclrs mb_strlit attrs _) cdecls stmt _) = do
	putStrLn "--------------------------------------"
	putStrLn $ dumpStr c
	writeFile (name++".html") $ htmlPage defaultHtmlOpts (valToHtml defaultHtmlOpts $ prettyVal c)
--instance CFunctionDef
{-
	print c

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

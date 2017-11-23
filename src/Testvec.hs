module Testvec where

import Language.C.Pretty
import Language.C.Syntax.AST
import Text.PrettyPrint
import Text.Show.Pretty


data ValueSet a = Values [a] | ValueInterval a a | Union (ValueSet a) (ValueSet a)
	deriving (Eq,Show)

analyze :: CTranslUnit -> IO ()
analyze ctranslunit = do
	print $ pretty ctranslunit

type Var = String

data Operator = Plus | Minus | Mult | Div
	deriving (Eq,Show)

data Expr = IntLitE Int | VarE Var | BinOpE Operator Expr Expr
	deriving (Eq,Show)

data AST = Assignment Var Expr
	deriving (Eq,Show)

type Env = [(Var,ValueSet Int)]

test = derive gamma ast
	where
	ast = Assignment "a" $ BinOpE Plus (VarE "x") (IntLitE 1)
	gamma = [("x",Values [2])]

derive gamma (Assignment var expr) = solve var expr

solve var expr = case expr of
	BinOpE op expr1 expr2 -> 

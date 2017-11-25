module Testvec where

import Language.C.Pretty
import Language.C.Syntax.AST
import Text.PrettyPrint
import Text.Show.Pretty


data ValueSet a = Interval a a | Union (ValueSet a) (ValueSet a) | Empty
	deriving (Eq,Show)

analyze :: CTranslUnit -> IO ()
analyze ctranslunit = do
	print $ pretty ctranslunit

instance (Num a) => Num (ValueSet a) where
	negate Empty = Empty
	negate (Interval from to) = Interval (negate to) (negate from)
	negate (Union vs1 vs2) = Union (negate vs1) (negate vs2)
	Empty + _ = Empty
	_ + Empty = Empty
	(Interval from1 to1) + (Interval from2 to2) = Interval (from1+from2) (to1+to2)
	(Interval from1 to1) + (Union vs1 vs2) = Union (from1+from2) (to1+to2)
	abs Empty = Empty
	abs (Union vs1 vs2) = Union (abs vs1) (abs vs2)
	abs (Interval from to) = Interval (min (abs from) (abs to)) (max (abs from) (abs to))
	signum Empty = Empty
	signum (Interval from to) = case signum from 

type Var = String

data Operator = Plus | Minus | Mult | Div
	deriving (Eq,Show)

data Expr = IntLitE Int | VarE Var | BinOpE Operator Expr Expr
	deriving (Eq,Show)

data AST = Assignment Var Expr
	deriving (Eq,Show)

type Env = [(Var,ValueSet Int)]



{-
test = derive gamma ast
	where
	ast = Assignment "a" $ BinOpE Plus (VarE "x") (IntLitE 1)
	gamma = [("a",ValueInterval 7 7)]

-- unify environments, if possible
unifyEnvs gamma1 [] = gamma1
unifyEnvs gamma1 (binding:gamma2) = unifyEnvs (mergeInto binding gamma1) gamma2

mergeInto binding [] = [binding]
mergeInto (var,valset) ((var1,valset1):gamma) | var==var1 = (var,intersectValSet valset valset1) : gamma
mergeInto binding (binding1:gamma) = binding1 : mergeInto binding gamma

intersectValSet (Interval from1 to1) (Interval from2 to2) | to1 < from2 = Nothing
intersectValSet (Interval from1 to1) (Interval from2 to2) | to2 < from1 = Nothing
intersectValSet (Values v1s) (Interval from to) | from <= v1s && v1s <= to = Just $ ValueInterval from to
intersectValSet

-- Calculate the pre-env from the post-env and a command
derive gamma_post (Assignment var expr) = unifyEnvs gamma_post expr_gamma
	where
	Just l_valset = lookup var gamma_post
	expr_gamma = solve gamma_post l_valset expr

-- solve an expression so that it evalutes to valueset in the given env
solve env valset expr = case expr of
	BinOpE op expr1 expr2 -> 
-}
{-
  Name: Nishita Narvekar
  Class: CS 252
  Assigment: HW2
  Date: 09/28/2017
  Description: while language interpreter
-}


module WhileInterp (
  Expression(..),
  Binop(..),
  Value(..),
  testProgram,
  run
) where

import Data.Map (Map)
import qualified Data.Map as Map


-- We represent variables as strings.
type Variable = String

-- The store is an associative map from variables to values.
-- (The store roughly corresponds with the heap in a language like Java).
type Store = Map Variable Value

data Expression =
    Var Variable                            -- x
  | Val Value                               -- v
  | Assign Variable Expression              -- x := e
  | Sequence Expression Expression          -- e1; e2
  | Op Binop Expression Expression
  | If Expression Expression Expression     -- if e1 then e2 else e3
  | While Expression Expression             -- while (e1) e2
  | And Expression Expression
  | Or Expression Expression
  | Not Expression
  deriving (Show)

data Binop =
    Plus     -- +  :: Int  -> Int  -> Int
  | Minus    -- -  :: Int  -> Int  -> Int
  | Times    -- *  :: Int  -> Int  -> Int
  | Divide   -- /  :: Int  -> Int  -> Int
  | Gt       -- >  :: Int -> Int -> Bool
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  deriving (Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show)


-- This function will be useful for defining binary operations.
-- The first case is done for you.
-- Be sure to explicitly check for a divide by 0 and throw an error.

applyOp :: Binop -> Value -> Value -> Value
applyOp Plus (IntVal i) (IntVal j) = IntVal $ i + j
applyOp Minus (IntVal i) (IntVal j) = IntVal $ i - j
applyOp Times (IntVal i) (IntVal j) = IntVal $ i * j
applyOp Divide (IntVal i) (IntVal j) = IntVal $ i `div` j

applyOp Gt (IntVal i) (IntVal j) =
  if i > j
    then (BoolVal True)
      else
        (BoolVal False)

applyOp Ge (IntVal i) (IntVal j) =
  if i >= j
    then (BoolVal True)
      else
        (BoolVal False)

applyOp Lt (IntVal i) (IntVal j) =
  if i < j
    then (BoolVal True)
      else
        (BoolVal False)

applyOp Le (IntVal i) (IntVal j) =
  if i <= j
    then (BoolVal True)
      else
        (BoolVal False)

applyOp _ _ _ = error "TBD"


-- Implement this function according to the specified semantics
evaluate :: Expression -> Store -> (Value, Store)
evaluate e s =
    case e of
    Op o e1 e2 ->
      let (v1,s1) = evaluate e1 s
          (v2,s') = evaluate e2 s1
      in (applyOp o v1 v2, s')

    Assign var e ->
      let (v1,s1) = evaluate e s
      in (assignment v1 var s1)
--(Assign "X" (Val (BoolVal True)))
    If e1 e2 e3 ->
      let (v1,s1) = evaluate e1 s
          (v2,s2) = evaluate e2 s1
          (v3,s3) = evaluate e3 s2
      in (applyif v1 v2 v3, s3)
--(While (Val (BoolVal False)) (Val (IntVal 42)))
    While e1 e2 ->
      let (v1,s1) = evaluate e1 s
      in
        case (v1) of
          BoolVal True ->
            let (v2,s2) = evaluate e2 s
            in (evaluate e s2)
          BoolVal False -> evaluate e1 s

    Sequence e1 e2 ->
      let (v1,s1) = evaluate e1 s
          (v2,s2) = evaluate e2 s1
      in (v2,s2)

--(Var "X")
    Var e1 ->
      applyvar e1 s

    And e1 e2 ->
      let (v1,s1) = evaluate e1 s
          (v2,s2) = evaluate e2 s1
      in (applyand v1 v2, s2)

    Or e1 e2 ->
      let (v1,s1) = evaluate e1 s
          (v2,s2) = evaluate e2 s1
      in (applyor v1 v2, s2)

    Not e1 ->
      let (v1,s1) = evaluate e1 s
      in (applynot v1, s1)

    Val v -> (v,s)

--putStrLn $ show $ WhileInterp.run (If (Val (BoolVal True)) (Val (IntVal 1)) (Val (IntVal 2)))
--putStrLn $ show $ WhileInterp.run (If (Val (BoolVal False)) (Val (IntVal 1)) (Val (IntVal 2)))
applyand:: Value -> Value -> Value
applyand (BoolVal v1) (BoolVal v2) =
  BoolVal ((&&) v1 v2)

applyor:: Value -> Value -> Value
applyor (BoolVal v1) (BoolVal v2) =
  BoolVal ((||) v1 v2)

applynot:: Value -> Value
applynot v1 =
  if (expEq v1 (BoolVal True))
    then (BoolVal False)
      else
        (BoolVal True)

expEq :: Value -> Value -> Bool
expEq (BoolVal True) (BoolVal True) = True
expEq (BoolVal False) (BoolVal False) = False
expEq _ _ = False

applyif:: Value -> Value -> Value ->Value
applyif v1 v2 v3 =
  if (expEq v1 (BoolVal True))
    then v2
      else
        v3


assignment:: Value -> Variable -> Store -> (Value,Store)
assignment v1 var s= (v1,Map.insert var v1 s)
  --case v1 of
    --Var a -> ((BoolVal True),Map.insert a (BoolVal True) s)

applyvar:: Variable -> Store -> (Value,Store)
applyvar var s =
  case (Map.lookup var s) of
        Just i -> (i,s)
        Nothing -> (BoolVal False,s)
--  let a = Map.lookup var s
--  in if(a == Map.empty)
  --  then (IntVal 0,s)
  --    else
  --      (a,s)

--evaluate e2 s = (IntVal 0,s)
-- Evaluates a program with an initially empty state
run :: Expression -> (Value, Store)
run prog = evaluate prog Map.empty

-- The same as run, but only returns the Store
testProgram :: Expression -> Store
testProgram prog = snd $ run prog

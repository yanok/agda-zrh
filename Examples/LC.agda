module Examples.LC where

open import Lib.Basics
open import Lib.Nat

Name : Set
Name = Nat -- using numbers to encode variable names

-- dumb Untyped Lambda Calculus terms encoding
data LC : Set where
  var : Name -> LC -- there are ways to ensure we are well-scoped but I'll keep it simple for now
  _$_ : LC -> LC -> LC -- application
  lam : Name -> LC -> LC -- abstraction

omega : LC
omega = lam 0 (var 0 $ var 0)

eval : (fuel : Nat) -> (Name -> One + LC) -> LC -> One + LC
eval zero env x = inl <> -- out of fuel
eval (suc fuel) env (var x) with env x
eval (suc fuel) env (var x) | inl <> = inl <>
eval (suc fuel) env (var x) | inr r = eval fuel env r
eval (suc fuel) env (f $ x) with eval fuel env f | eval fuel env x
eval (suc fuel) env (f $ x) | inr (var v) | inr x' = inr (var v $ x')
eval (suc fuel) env (f $ x) | inr (f'' $ x'') | inr x' = inr ((f'' $ x'') $ x')
eval (suc fuel) env (f $ x) | inr (lam y b) | inr x' = {!eval!}
... | _ | _ = inl <>
eval (suc fuel) env (lam x b) = {!!}

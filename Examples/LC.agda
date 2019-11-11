module Examples.LC where

open import Lib.Basics
open import Lib.Nat

Name : Set
Name = Nat -- using numbers to encode variable names

-- we need to be able to compare numbers to implement environment
-- it would be enough to just return Bool in this case but
-- let's make it more precise
compare : (x y : Nat) -> x == y + Not (x == y)
compare zero zero = inl refl
compare zero (suc y) = inr (λ ())
compare (suc x) zero = inr (λ ())
compare (suc x) (suc y) with compare x y
compare (suc x) (suc y) | inl eq = inl (suc $= eq)
compare (suc x) (suc y) | inr neq = inr (λ { refl → neq refl})

-- dumb Untyped Lambda Calculus terms encoding
data LC : Set where
  var : Name -> LC -- there are ways to ensure we are well-scoped but I'll keep it simple for now
  _$_ : LC -> LC -> LC -- application
  lam : Name -> LC -> LC -- abstraction

-- some terms
omega : LC
omega = lam 0 (var 0 $ var 0)

-- Maybe X is One + X
Maybe : Set -> Set
Maybe X = One + X

-- pattern synonyms for nothing and just
pattern nothing = inl <>
pattern just x = inr x

-- environment
Env : Set
Env = Name -> Maybe LC

empty : Env
empty _ = nothing

update : Name -> LC -> Env -> Env
update name v env x with compare name x
update name v env x | inl _ = just v
update name v env x | inr _ = env x

delete : Name -> Env -> Env
delete name env x with compare name x
delete name env x | inl _ = nothing
delete name env x | inr _ = env x

eval : (fuel : Nat) -> Env -> LC -> Maybe LC
eval fuel env (var x) with env x
eval fuel env (var x) | nothing = just (var x)
eval zero env (var x) | just r = nothing
eval (suc fuel) env (var x) | just r = eval fuel env r
eval fuel env (f $ x) with eval fuel env f
eval zero env (f $ x) | just (lam y b) = nothing
eval (suc fuel) env (f $ x) | just (lam y b) = eval fuel (update y x env) b
eval fuel env (f $ x) | just f' with eval fuel env x
eval fuel env (f $ x) | just f' | nothing = nothing
eval fuel env (f $ x) | just f' | just x' = just (f' $ x')
eval fuel env (f $ x) | nothing = nothing
eval fuel env (lam x b) with eval fuel (delete x env) b
eval fuel env (lam x b) | nothing = nothing
eval fuel env (lam x b) | just b' = just (lam x b')

-- predicate saying if given lambda term terminates
Terminates : LC -> Set
Terminates t = Sg Nat \n -> Sg LC \t' -> eval n empty t == just t'

-- assume we have excluded middle
module AssumeEM (em : (P : Set) -> P + Not P) where
  haltingProblem : LC -> Two -- given arbitrary lambda term, say if it terminates
  haltingProblem t with em (Terminates t)
  haltingProblem t | inl _ = tt
  haltingProblem t | inr _ = ff

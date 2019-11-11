module Exercise.Zero where

open import Lib.Basics
open import Lib.Nat

{-----------------------------------------------------------------------------
TOP TIP: if you have annoyingly many open goals, comment out big chunks of the
file with a multi-line comment a bit like this one.
-----------------------------------------------------------------------------}


{----------------------------------------------------------------------------}
{- 1.1: Tree Sort -}
{----------------------------------------------------------------------------}

-- 1.1.1 implement concatenation for lists

_++_ : {X : Set} -> List X -> List X -> List X
xs ++ ys = {!!}

infixr 3 _++_

-- a datatype of node-labelled binary trees is given as follows

data Tree (X : Set) : Set where
  leaf : Tree X
  _<[_]>_ : Tree X -> X -> Tree X -> Tree X

-- 1.1.2 implement the insertion of a number into a tree, ensuring that
-- the numbers in the tree are in increasing order from left to right;
-- make sure to retain duplicates

insertTree : Nat -> Tree Nat -> Tree Nat
insertTree x t = {!!}

-- 1.1.3 implement the function which takes the elements of a list and
-- builds an ordered tree from them, using insertTree

makeTree : List Nat -> Tree Nat
makeTree xs = {!!}

-- 1.1.4 implement the function which flattens a tree to a list,
-- using concatenation

flatten : {X : Set} -> Tree X -> List X
flatten t = {!!}

-- 1.1.5 using the above components, implement a sorting algorithm which
-- works by building a tree and then flattening it

treeSort : List Nat -> List Nat
treeSort = {!!}

-- 1.1.6 give a collection of unit tests which cover every program line
-- from 1.1.1 to 1.1.5

-- 1.1.7 implement a fast version of flatten, taking an accumulating parameter,
-- never using ++. and ensuring that the law
--
-- fastFlatten t xs == flatten t ++ xs
--
-- is true; for an extra style point, ensure that the accumulating parameter
-- is never given a name in your program

fastFlatten : {X : Set} -> Tree X -> List X -> List X
fastFlatten t = {!!}

-- 1.1.8 use fastFlatten to build a fast version of tree sort

fastTreeSort : List Nat -> List Nat
fastTreeSort xs = {!!}

-- 1.1.9 again, give unit tests which cover every line of code


{----------------------------------------------------------------------------}
{- 1.2: Shooting Propositional Logic Fish In A Barrel -}
{----------------------------------------------------------------------------}

-- 1.2.1 implement the following operations; try to use only
-- [C-c C-c] and [C-c C-a]

orCommute : {A B : Set} -> A + B -> B + A
orCommute x = {!!}

orAbsorbL : {A : Set} -> Zero + A -> A
orAbsorbL x = {!!}

orAbsorbR : {A : Set} -> A + Zero -> A
orAbsorbR x = {!!}

orAssocR : {A B C : Set} -> (A + B) + C -> A + (B + C)
orAssocR x = {!!}

orAssocL : {A B C : Set} -> A + (B + C) -> (A + B) + C
orAssocL x = {!!}

-- 1.2.2 implement the following operations; try to use only
-- [C-c C-c] and [C-c C-a]

andCommute : {A B : Set} -> A * B -> B * A
andCommute x = {!!}

andAbsorbL : {A : Set} -> A -> One * A
andAbsorbL x = {!!}

andAbsorbR : {A : Set} -> A -> A * One
andAbsorbR x = {!!}

andAssocR : {A B C : Set} -> (A * B) * C -> A * (B * C)
andAssocR x = {!!}

andAssocL : {A B C : Set} -> A * (B * C) -> (A * B) * C
andAssocL x = {!!}

-- how many times is [C-c C-c] really needed?

-- 1.2.3 implement the following operations; try to use only
-- [C-c C-c] and [C-c C-a]

distribute : {A B C : Set} -> A * (B + C) -> (A * B) + (A * C)
distribute x = {!!}

factor : {A B C : Set} -> (A * B) + (A * C) -> A * (B + C)
factor x = {!!}


-- 1.2.4 try to implement the following operations; try to use only
-- [C-c C-c] and [C-c C-a]; at least one of them will prove to be
-- impossible, in which case you should comment it out and explain
-- why it's impossible

deMorgan1 : {A B : Set} -> (Not A + Not B) -> Not (A * B)
deMorgan1 x y = {!!}

deMorgan2 : {A B : Set} -> Not (A * B) -> (Not A + Not B)
deMorgan2 x = {!!}

deMorgan3 : {A B : Set} -> (Not A * Not B) -> Not (A + B)
deMorgan3 x y = {!!}

deMorgan4 : {A B : Set} -> Not (A + B) -> (Not A * Not B)
deMorgan4 x = {!!}


-- 1.2.5 try to implement the following operations; try to use only
-- [C-c C-c] and [C-c C-a]; at least one of them will prove to be
-- impossible, in which case you should comment it out and explain
-- why it's impossible

dnegI : {X : Set} -> X -> Not (Not X)
dnegI = {!!}

dnegE : {X : Set} -> Not (Not X) -> X
dnegE = {!!}

neg321 : {X : Set} -> Not (Not (Not X)) -> Not X
neg321 = {!!}

hamlet : {B : Set} -> B + Not B
hamlet = {!!}

nnHamlet : {B : Set} -> Not (Not (B + Not B))
nnHamlet = {!!}

-- yanok: Additional recursive vs inductive definitions exercise
-- Lib.Nat defines _<=_ relation as a recursive function with result in Set
-- but it's more usual to define it inductively:

data _<=I_ : Nat -> Nat -> Set where -- it's a binary relation on Nat
  refl-<=I : {n : Nat} -> n <=I n -- every natural is less or equal to itself
  <=I-suc : {n m : Nat} -> n <=I m -> n <=I suc m -- if n is less or equal than m, n is less or equal than suc m

-- prove that these definitions are equivalent
-- you will need some additional lemmas for things which are immediately obvious
-- in one definition but need some effort in another

-- Prove that inductive <=I definition allows suc'ing both sides
-- What should we do induction on?
suc-<=I : {n m : Nat} -> n <=I m -> suc n <=I suc m
suc-<=I n<=m = {!!}

-- Now show that <= implies <=I
<=-imp-<=I : (n m : Nat) -> n <= m -> n <=I m
<=-imp-<=I n m n<=m = {!!}

-- You might also want to prove some simple stuff about <=
-- Show that it is reflexive
refl-<= : (n : Nat) -> n <= n
refl-<= n = {!!}

-- And that suc'ing the bigger number is ok
<=-suc : (n m : Nat) -> n <= m -> n <= suc m
<=-suc n m n<=m = {!!}

-- Now show that <=I implies <=
<=I-imp-<= : {n m : Nat} -> n <=I m -> n <= m
<=I-imp-<= n<=Im = {!!}

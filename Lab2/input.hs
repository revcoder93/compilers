---------------------------------------------------------
--Courtesy of CSEN 403 Concepts of Programming languages
--Prof. Dr. Slim Abdennadher & Eng. Maha Badreldin
---------------------------------------------------------

---------------------------------------------------------
--Types
---------------------------------------------------------
data Term = Var String| Cons String deriving (Show,Eq)

type Substitution = (Term,Term) 

data Solution = No | Yes [Substitution] deriving (Show,Eq)

data Predicate = P String [Term] deriving Show

type Goals = [Predicate]

type Rule = (Predicate,Goals)

type KB = [Rule]

---------------------------------------------------------
--Terms Unifications between query and KB predicates
---------------------------------------------------------
unifyTerms:: Term -> Term -> Solution -> Solution
unifyTerms (Cons c1) (Cons c2) sol = if c1 == c2 then sol else No
unifyTerms (Var v1) (Var v2) (Yes subList) = Yes (subList++[(Var v2, Var v1)])
unifyTerms (Cons c) (Var v) sol = unifyTerms (Var v) (Cons c) sol
unifyTerms (Var v) (Cons c) (Yes subList)= if isFree (Var v) (Yes subList) then Yes (subList++[(Var v,Cons c)])
                                           else if getCurVal (Var v) (Yes subList) == (Cons c) then (Yes subList) else No

isFree :: Term -> Solution -> Bool
isFree (Var v) (Yes subList) = notElem (Var v) boundVars where boundVars = map fst subList

getCurVal :: Term -> Solution -> Term
getCurVal (Var v) (Yes ((x,y):tail)) = if (Var v) == x then y else getCurVal (Var v) (Yes tail)
--thought: f(A,maha,B,A,C) vs f(slim,X,X,Y,Z) {A->slim, X->maha, X->B, Y->A, Z-> C}
--f(A,B) vs f(X,X) {X->A, X->B}
--f(A,A) vs f(X,Y) {X->A, Y->A}
--f(A,maha) vs f(X,X)

--thought: f(A,maha,B,C,D) vs f(slim,X,mina,Y,Z) 
---------------------------------------------------------
--Unification between query and a head in the KB
---------------------------------------------------------
--unifyWithHead (P "f" [Var "A", Cons "maha", Var "B", Var "C", Var "D"]) (P "f" [Cons "slim", Var "X", Cons "mina", Var "Y", Var "Z"]) (Yes [])
--ans:
--Yes [(Var "A",Cons "slim"),(Var "X",Cons "maha"),(Var "B",Cons "mina"),(Var "Y",Var "C"),(Var "Z",Var "D")]
unifyWithHead:: Predicate -> Predicate -> Solution -> Solution
unifyWithHead (P name1 args1) (P name2 args2) sol = 
 if name1 == name2 then unifyArgs args1 args2 sol else No
 
unifyArgs:: [Term] -> [Term] -> Solution -> Solution
--both arg lists must be of the same length
unifyArgs [] [] sol = sol
unifyArgs [] _ _ = No
unifyArgs _ [] _ = No
unifyArgs (x:xs) (y:ys) sol = if tmpSol == No then No
							  else unifyArgs xs ys tmpSol
							  where tmpSol = unifyTerms x y sol 

---------------------------------------------------------
--Apply substitution to a KB rule
---------------------------------------------------------
applySub :: Substitution ->  Predicate -> Predicate
applySub sub  (P name args) = P name (replace args sub)

replace :: [Term] -> Substitution -> [Term]
replace [] _ = []
replace (x:xs) (old,new) = if old==x then new:replace xs (old,new) else x:replace xs (old,new)

applySubBody ::  Substitution ->  Goals -> Goals
applySubBody s g = map (applySub s) g 

applySolSubBody ::  Solution -> Goals -> Goals
applySolSubBody (Yes subList) body = foldr applySubBody body subList

--    f(slim ,X    ,mina,Y ,Z):- f2(X,Z), f3(slim,Z,X,Y).
-- ?- f(A   , maha,B   ,C ,D)
--applySolSubBody (Yes [(Var "A",Cons "slim"),(Var "X",Cons "maha"),(Var "B",Cons "mina"),(Var "Y",Var "C"),(Var "Z",Var "D")]) [P "f2" [Var "X", Var "Z"],P "f3" [Cons "slim", Var "Z", Var "X", Var "Y"]]
--ans:
--[P "f2" [Cons "maha",Var "D"],P "f3" [Cons "slim",Var "D",Cons "maha",Var "C"]]

filterTmpVars :: [Substitution] -> [Substitution]
filterTmpVars [] = []
filterTmpVars ((Var v,new):xs) = if v>="M" then filterTmpVars xs else (Var v,new):filterTmpVars xs
--filterTmpVars [(Var "A",Cons "slim"),(Var "X",Cons "maha"),(Var "B",Cons "mina"),(Var "Y",Var "C"),(Var "Z",Var "D")]
--ans:
--[(Var "A",Cons "slim"),(Var "B",Cons "mina")]

---------------------------------------------------------
--Find all solutions to a query using a KB
---------------------------------------------------------
allSolutions:: Predicate -> KB -> [Solution]
allSolutions p kb = allSol p kb kb (Yes [])

--try the rules in the KB one by one in the same order
allSol:: Predicate -> KB -> KB -> Solution -> [Solution] 
allSol p [] original s = []
allSol p (r:rs) original s = (solveUsingRule p r original s)++(allSol p rs original s)

solveUsingRule :: Predicate -> Rule -> KB -> Solution -> [Solution]
solveUsingRule p (head,body) kb s =  if tmpSol == No then [] else proveBody (applySolSubBody  tmpSol body) kb (Yes (filterTmpVars subList))
                                     where {tmpSol = unifyWithHead p head s; (Yes subList) = tmpSol}

proveBody :: Goals -> KB  -> Solution -> [Solution]
proveBody [] _ s = [s]
proveBody (goal:goals) kb s = concat (map (proveBody goals kb) (allSol goal kb kb s))

---------------------------------------------------------
--Sample KB
---------------------------------------------------------
{-

male(timmy).
male(alex).
male(slim).
male(azmy).
male(remon).

female(amira).
female(reem).
female(wanda).

parent(slim,amira).
parent(wanda,timmy).
parent(azmy,reem).
parent(azmy,remon).

father(X,Y):- male(X),
              parent(X,Y).

daughterFather(X,Y):- father(Y,X), female(X).

translation:
[(P "male" [Cons "timmy"],[]),(P "male" [Cons "alex"],[]),(P "male" [Cons "slim"],[]), (P "male" [Cons "azmy"],[]), (P "male" [Cons "remon"],[]), (P "female" [Cons "amira"],[]), (P "female" [Cons "reem"],[]), (P "female" [Cons "wanda"],[]), (P "parent" [Cons "slim", Cons "amira"],[]), (P "parent" [Cons "wanda", Cons "timmy"],[]), (P "parent" [Cons "azmy", Cons "reem"],[]), (P "parent" [Cons "azmy", Cons "remon"],[]), (P "father" [Var "X", Var "Y"], [P "male" [Var "X"], P "parent" [Var "X", Var "Y"]]), (P "daughterFather" [Var "X", Var "Y"], [P "father" [Var "Y", Var "X"], P "female" [Var "X"]])]

Main> allSolutions (P "male" [Var "A"]) [(P "male" [Cons "timmy"],[]),(P "male" [Cons "alex"],[]),(P "male" [Cons "slim"],[]), (P "male" [Cons "azmy"],[]), (P "male" [Cons "remon"],[]), (P "female" [Cons "amira"],[]), (P "female" [Cons "reem"],[]), (P "female" [Cons "wanda"],[]), (P "parent" [Cons "slim", Cons "amira"],[]), (P "parent" [Cons "wanda", Cons "timmy"],[]), (P "parent" [Cons "azmy", Cons "reem"],[]), (P "parent" [Cons "azmy", Cons "remon"],[]), (P "father" [Var "X", Var "Y"], [P "male" [Var "X"], P "parent" [Var "X", Var "Y"]]), (P "daughterFather" [Var "X", Var "Y"], [P "father" [Var "Y", Var "X"], P "female" [Var "X"]])]
--ans:
--[
Yes [(Var "A",Cons "timmy")],
Yes [(Var "A",Cons "alex")],
Yes [(Var "A",Cons "slim")],
Yes [(Var "A",Cons "azmy")],
Yes [(Var "A",Cons "remon")]
]

Main> allSolutions (P "female" [Var "A"]) [(P "male" [Cons "timmy"],[]),(P "male" [Cons "alex"],[]),(P "male" [Cons "slim"],[]), (P "male" [Cons "azmy"],[]), (P "male" [Cons "remon"],[]), (P "female" [Cons "amira"],[]), (P "female" [Cons "reem"],[]), (P "female" [Cons "wanda"],[]), (P "parent" [Cons "slim", Cons "amira"],[]), (P "parent" [Cons "wanda", Cons "timmy"],[]), (P "parent" [Cons "azmy", Cons "reem"],[]), (P "parent" [Cons "azmy", Cons "remon"],[]), (P "father" [Var "X", Var "Y"], [P "male" [Var "X"], P "parent" [Var "X", Var "Y"]]), (P "daughterFather" [Var "X", Var "Y"], [P "father" [Var "Y", Var "X"], P "female" [Var "X"]])]
--ans:
--[
Yes [(Var "A",Cons "amira")],
Yes [(Var "A",Cons "reem")],
Yes [(Var "A",Cons "wanda")]
]

Main> allSolutions (P "parent" [Var "A",Var "B"]) [(P "male" [Cons "timmy"],[]),(P "male" [Cons "alex"],[]),(P "male" [Cons "slim"],[]), (P "male" [Cons "azmy"],[]), (P "male" [Cons "remon"],[]), (P "female" [Cons "amira"],[]), (P "female" [Cons "reem"],[]), (P "female" [Cons "wanda"],[]), (P "parent" [Cons "slim", Cons "amira"],[]), (P "parent" [Cons "wanda", Cons "timmy"],[]), (P "parent" [Cons "azmy", Cons "reem"],[]), (P "parent" [Cons "azmy", Cons "remon"],[]), (P "father" [Var "X", Var "Y"], [P "male" [Var "X"], P "parent" [Var "X", Var "Y"]]), (P "daughterFather" [Var "X", Var "Y"], [P "father" [Var "Y", Var "X"], P "female" [Var "X"]])]
--ans:
--[
Yes [(Var "A",Cons "slim"),(Var "B",Cons "amira")],
Yes [(Var "A",Cons "wanda"),(Var "B",Cons "timmy")],
Yes [(Var "A",Cons "azmy"),(Var "B",Cons "reem")],
Yes [(Var "A",Cons "azmy"),(Var "B",Cons "remon")]
]

Main> allSolutions (P "parent" [Var "A",Cons "reem"]) [(P "male" [Cons "timmy"],[]),(P "male" [Cons "alex"],[]),(P "male" [Cons "slim"],[]), (P "male" [Cons "azmy"],[]), (P "male" [Cons "remon"],[]), (P "female" [Cons "amira"],[]), (P "female" [Cons "reem"],[]), (P "female" [Cons "wanda"],[]), (P "parent" [Cons "slim", Cons "amira"],[]), (P "parent" [Cons "wanda", Cons "timmy"],[]), (P "parent" [Cons "azmy", Cons "reem"],[]), (P "parent" [Cons "azmy", Cons "remon"],[]), (P "father" [Var "X", Var "Y"], [P "male" [Var "X"], P "parent" [Var "X", Var "Y"]]), (P "daughterFather" [Var "X", Var "Y"], [P "father" [Var "Y", Var "X"], P "female" [Var "X"]])]
--ans:
--[
Yes [(Var "A",Cons "azmy")]
]

Main> allSolutions (P "parent" [Var "A",Cons "alex"])  [(P "male" [Cons "timmy"],[]),(P "male" [Cons "alex"],[]),(P "male" [Cons "slim"],[]), (P "male" [Cons "azmy"],[]), (P "male" [Cons "remon"],[]), (P "female" [Cons "amira"],[]), (P "female" [Cons "reem"],[]), (P "female" [Cons "wanda"],[]), (P "parent" [Cons "slim", Cons "amira"],[]), (P "parent" [Cons "wanda", Cons "timmy"],[]), (P "parent" [Cons "azmy", Cons "reem"],[]), (P "parent" [Cons "azmy", Cons "remon"],[]), (P "father" [Var "X", Var "Y"], [P "male" [Var "X"], P "parent" [Var "X", Var "Y"]]), (P "daughterFather" [Var "X", Var "Y"], [P "father" [Var "Y", Var "X"], P "female" [Var "X"]])]
--ans:
--[]

Main> allSolutions (P "father" [Var "A",Var "B"]) [(P "male" [Cons "timmy"],[]),(P "male" [Cons "alex"],[]),(P "male" [Cons "slim"],[]), (P "male" [Cons "azmy"],[]), (P "male" [Cons "remon"],[]), (P "female" [Cons "amira"],[]), (P "female" [Cons "reem"],[]), (P "female" [Cons "wanda"],[]), (P "parent" [Cons "slim", Cons "amira"],[]), (P "parent" [Cons "wanda", Cons "timmy"],[]), (P "parent" [Cons "azmy", Cons "reem"],[]), (P "parent" [Cons "azmy", Cons "remon"],[]), (P "father" [Var "X", Var "Y"], [P "male" [Var "X"], P "parent" [Var "X", Var "Y"]]), (P "daughterFather" [Var "X", Var "Y"], [P "father" [Var "Y", Var "X"], P "female" [Var "X"]])]
--ans:
--[
Yes [(Var "A",Cons "slim"),(Var "B",Cons "amira")],
Yes [(Var "A",Cons "azmy"),(Var "B",Cons "reem")],
Yes [(Var "A",Cons "azmy"),(Var "B",Cons "remon")]
]

Main> allSolutions (P "daughterFather" [Var "A",Var "B"]) [(P "male" [Cons "timmy"],[]),(P "male" [Cons "alex"],[]),(P "male" [Cons "slim"],[]), (P "male" [Cons "azmy"],[]), (P "male" [Cons "remon"],[]), (P "female" [Cons "amira"],[]), (P "female" [Cons "reem"],[]), (P "female" [Cons "wanda"],[]), (P "parent" [Cons "slim", Cons "amira"],[]), (P "parent" [Cons "wanda", Cons "timmy"],[]), (P "parent" [Cons "azmy", Cons "reem"],[]), (P "parent" [Cons "azmy", Cons "remon"],[]), (P "father" [Var "X", Var "Y"], [P "male" [Var "X"], P "parent" [Var "X", Var "Y"]]), (P "daughterFather" [Var "X", Var "Y"], [P "father" [Var "Y", Var "X"], P "female" [Var "X"]])]
--ans:
--[
Yes [(Var "B",Cons "slim"),(Var "A",Cons "amira")],
Yes [(Var "B",Cons "azmy"),(Var "A",Cons "reem")]
]

Main> allSolutions (P "daughterFather" [Cons "amira",Cons "alex"]) [(P "male" [Cons "timmy"],[]),(P "male" [Cons "alex"],[]),(P "male" [Cons "slim"],[]), (P "male" [Cons "azmy"],[]), (P "male" [Cons "remon"],[]), (P "female" [Cons "amira"],[]), (P "female" [Cons "reem"],[]), (P "female" [Cons "wanda"],[]), (P "parent" [Cons "slim", Cons "amira"],[]), (P "parent" [Cons "wanda", Cons "timmy"],[]), (P "parent" [Cons "azmy", Cons "reem"],[]), (P "parent" [Cons "azmy", Cons "remon"],[]), (P "father" [Var "X", Var "Y"], [P "male" [Var "X"], P "parent" [Var "X", Var "Y"]]), (P "daughterFather" [Var "X", Var "Y"], [P "father" [Var "Y", Var "X"], P "female" [Var "X"]])]
--ans:
--[]

Main> allSolutions (P "daughterFather" [Cons "amira",Var "A"]) [(P "male" [Cons "timmy"],[]),(P "male" [Cons "alex"],[]),(P "male" [Cons "slim"],[]), (P "male" [Cons "azmy"],[]), (P "male" [Cons "remon"],[]), (P "female" [Cons "amira"],[]), (P "female" [Cons "reem"],[]), (P "female" [Cons "wanda"],[]), (P "parent" [Cons "slim", Cons "amira"],[]), (P "parent" [Cons "wanda", Cons "timmy"],[]), (P "parent" [Cons "azmy", Cons "reem"],[]), (P "parent" [Cons "azmy", Cons "remon"],[]), (P "father" [Var "X", Var "Y"], [P "male" [Var "X"], P "parent" [Var "X", Var "Y"]]), (P "daughterFather" [Var "X", Var "Y"], [P "father" [Var "Y", Var "X"], P "female" [Var "X"]])]
--ans:
--[Yes [(Var "A",Cons "slim")]]

Main> allSolutions (P "daughterFather" [Var "D",Cons "slim"]) [(P "male" [Cons "timmy"],[]),(P "male" [Cons "alex"],[]),(P "male" [Cons "slim"],[]), (P "male" [Cons "azmy"],[]), (P "male" [Cons "remon"],[]), (P "female" [Cons "amira"],[]), (P "female" [Cons "reem"],[]), (P "female" [Cons "wanda"],[]), (P "parent" [Cons "slim", Cons "amira"],[]), (P "parent" [Cons "wanda", Cons "timmy"],[]), (P "parent" [Cons "azmy", Cons "reem"],[]), (P "parent" [Cons "azmy", Cons "remon"],[]), (P "father" [Var "X", Var "Y"], [P "male" [Var "X"], P "parent" [Var "X", Var "Y"]]), (P "daughterFather" [Var "X", Var "Y"], [P "father" [Var "Y", Var "X"], P "female" [Var "X"]])]
--ans:
--[Yes [(Var "D",Cons "amira")]]

Main> allSolutions (P "daughterFather" [Cons "remon", Cons "azmy"]) [(P "male" [Cons "timmy"],[]),(P "male" [Cons "alex"],[]),(P "male" [Cons "slim"],[]), (P "male" [Cons "azmy"],[]), (P "male" [Cons "remon"],[]), (P "female" [Cons "amira"],[]), (P "female" [Cons "reem"],[]), (P "female" [Cons "wanda"],[]), (P "parent" [Cons "slim", Cons "amira"],[]), (P "parent" [Cons "wanda", Cons "timmy"],[]), (P "parent" [Cons "azmy", Cons "reem"],[]), (P "parent" [Cons "azmy", Cons "remon"],[]), (P "father" [Var "X", Var "Y"], [P "male" [Var "X"], P "parent" [Var "X", Var "Y"]]), (P "daughterFather" [Var "X", Var "Y"], [P "father" [Var "Y", Var "X"], P "female" [Var "X"]])]
--ans:
--[]

Main> allSolutions (P "daughterFather" [Cons "reem", Cons "azmy"]) [(P "male" [Cons "timmy"],[]),(P "male" [Cons "alex"],[]),(P "male" [Cons "slim"],[]), (P "male" [Cons "azmy"],[]), (P "male" [Cons "remon"],[]), (P "female" [Cons "amira"],[]), (P "female" [Cons "reem"],[]), (P "female" [Cons "wanda"],[]), (P "parent" [Cons "slim", Cons "amira"],[]), (P "parent" [Cons "wanda", Cons "timmy"],[]), (P "parent" [Cons "azmy", Cons "reem"],[]), (P "parent" [Cons "azmy", Cons "remon"],[]), (P "father" [Var "X", Var "Y"], [P "male" [Var "X"], P "parent" [Var "X", Var "Y"]]), (P "daughterFather" [Var "X", Var "Y"], [P "father" [Var "Y", Var "X"], P "female" [Var "X"]])]
--ans:
--[Yes []]

-}


{-
f(A,maha,B,C,D) vs f(slim,X,mina,Y,Z) {A->slim, X->maha, X->B, Y->A, Z-> C}
[(P "f" [Cons "slim", Var "X", Cons "mina", Var "Y", Var "Z"],[P "f2" [Var "Z", Cons "maha"]])]

Main> allSolutions (P "f" [Var "A", Cons "maha", Var "B", Var "C", Var "D"]) [(P "f2" [Var "W", Cons "maha"],[]),(P "f" [Cons "slim", Var "X", Cons "mina", Var "Y", Var "Z"],[P "f2" [Var "Z", Cons "maha"]])]
[Yes [(Var "A",Cons "slim"),(Var "B",Cons "mina")]]

Main> allSolutions (P "f" [Var "A", Cons "maha", Var "B", Var "C", Var "D"]) [(P "f2" [Var "W", Cons "maha"],[]),(P "f" [Var "U", Var "X", Cons "mina", Var "Y", Var "Z"],[P "f2" [Var "Z", Cons "maha"]])]
[Yes [(Var "B",Cons "mina")]]

Main> allSolutions (P "f" [Var "A", Cons "maha", Var "B", Var "C", Var "D"]) [(P "f2" [Cons "maha", Cons "maha"],[]),(P "f" [Var "U", Var "X", Cons "mina", Var "Y", Var "Z"],[P "f2" [Var "Z", Cons "maha"]])]
[Yes [(Var "B",Cons "mina"),(Var "D",Cons "maha")]]


Main> allSolutions (P "f" [Var "A", Cons "maha", Var "B", Var "C", Var "D"]) [(P "f2" [Var "X", Cons "maha"],[P "f3" [Var "X"]]), (P "f3" [Var "X"],[P "f4" [Var "X"]]), (P "f4" [Cons "maha"],[]),(P "f" [Var "U", Var "X", Cons "mina", Var "Y", Var "Z"],[P "f2" [Var "Z", Cons "maha"]])]
[Yes [(Var "B",Cons "mina"),(Var "D",Cons "maha")]]

allSolutions (P "f" [Var "A", Cons "maha", Var "B", Var "C", Var "D"]) [(P "f2" [Var "X", Cons "maha"],[P "f3" [Var "X"]]), (P "f3" [Var "X"],[P "f4" [Var "X"]]), (P "f4" [Var "X"],[]),(P "f" [Var "U", Var "X", Cons "mina", Var "Y", Var "Z"],[P "f2" [Var "Z", Cons "maha"]])]
[Yes [(Var "B",Cons "mina")]]

-}
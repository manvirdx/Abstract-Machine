type Var = String

data Term = -- x | lx.M | MN
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term
--  deriving Show

instance Show Term where
  show = pretty

example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Variable "a")) (Variable "x")) (Variable "b")))

pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m 
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m
 
----------------------------
-- Pre-requisite functions--
----------------------------
variables :: [Var]
variables = [[a] | a <- ['a'..'z']] ++ (alphabetList 1)
    where
        alphabetList :: Int -> [Var]
        alphabetList i = [[a] ++ (show i) | a <- ['a'..'z']] ++ (alphabetList (i+1))

filterVariables :: [Var] -> [Var] -> [Var]
filterVariables listA listB = [x | x <- listA, not (elem x listB)]

fresh :: [Var] -> Var
fresh list = head (filterVariables variables list)

used :: Term -> [Var]
used (Variable v) = [v]
used (Lambda x term) = merge [x] (used term)
used (Apply term1 term2) = merge (used term1) (used term2) 

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

rename :: Var -> Var -> Term -> Term
rename x y (Variable z) = if x == z then (Variable y) else (Variable z)
rename x y (Lambda z n) = if x == z then (Lambda z n) else (Lambda z (rename x y n))
rename x y (Apply  n m) = Apply (rename x y n) (rename x y m)

substitute :: Var -> Term -> Term -> Term
substitute x f (Variable v) = if x == v then f else (Variable v) 
substitute x f (Apply  n m) = Apply (substitute x f n) (substitute x f m)
substitute x f (Lambda z n) = if x == z then (Lambda z n) 
                              else (Lambda ld (substitute x f (rename z (ld) n))) 
                              where ld = fresh(merge [x] (merge (used f) (used n)))
-------------------------------------
--Part 1:  Partial Abstract Machine--
-------------------------------------
state1 = (Lambda "x" (Lambda "y" (Variable "x")) , [Variable "Yes", Variable "No"])
term1 = Apply (Apply (Lambda "x" (Lambda "y" (Variable "x"))) (Variable "Yes")) (Variable "No")
term2 = Apply (Apply (Lambda "b" (Apply example (Variable "Yes"))) (Lambda "z" (Variable "z"))) (Variable "No")

type PState = (Term, [Term])

p_start :: Term -> PState
p_start t = (t, [])

p_step :: PState -> PState
p_step ((Lambda z n), (m:s)) = (substitute z m n, s)
p_step ((Apply n m, ms)) = (n, m:ms)
p_step ((n, m)) = (n, m)

p_final :: PState -> Bool
p_final ((Lambda z n), []) = True
p_final (Variable v, _) = True
p_final _ = False

p_run :: Term -> IO ()
p_run p = do
        let t = p_start p in run_machine t
            where    
                run_machine :: PState -> IO ()
                run_machine t = do
                   if p_final t
                   then do
                       print t
                       print (p_readback t)
                   else do
                       print t
                       run_machine (p_step t)
        
p_readback :: PState -> Term
p_readback ((t, p:ps)) = p_readback ((Apply t p), ps)
p_readback((p, [])) = p

-------------------------------------
-- Part 2: Krivine Abstract Machine--
-------------------------------------
data Env = Env [(Var,Term,Env)]

type State = (Term,Env,[(Term,Env)])

instance Show Env where
  show (Env e) = show e

state2 = (Apply (Lambda "x" (Variable "x")) (Variable "y") , Env [("y",Lambda "z" (Variable "z"), Env [])], [])

state3 = (Apply (Variable "x") (Variable "x"), Env [("x", Lambda "x" (Apply (Variable "x") (Variable "x")) , Env [])], [])

state4 = (Lambda "y" (Variable "x") , Env [] , [(Variable "z" , Env [("z", Lambda "a" (Variable "b") , Env [("b", Variable "c" , Env [])])])]) 


-------------------------

start :: Term -> State
start n = (n, Env [], [])

step :: State -> State
step (Variable x , Env ((y,m,f):e) , cs)
  | x == y                          = (m,f,cs)
  | otherwise                       = (Variable x, Env e, cs)
step (Lambda x n , Env e, (m,f):cs) = (n, Env ((x,m,f):e), cs)
step (Apply  n m ,     e,       cs) = (n, e, (m,e):cs)

final :: State -> Bool
final (Variable _ , Env [] ,  _) = True
final (Lambda _ _ ,      _ , []) = True
final _ = False

run :: Term -> IO ()
run = loop . start
  where
    loop st = do
      print st
      if final st then
        print (readback st)
      else
        loop (step st)

readback :: State -> Term
readback (n,Env e,cs) = foldl Apply (close n e) [close m f | (m,Env f) <- cs]
 where
  close (Variable x) [] = Variable x
  close (Variable x) ((y,m,Env f):e)
    | x == y            = close m f
    | otherwise         = close (Variable x) e
  close (Lambda x n)  e = Lambda x (close n ((x,Variable x, Env []):e))
  close (Apply  n m)  e = Apply (close n e) (close m e)

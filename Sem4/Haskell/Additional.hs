module Additional where
import Data.Functor.Identity

-- Zadanie 99  

-- definicja drzewa
data Term a = LL a
            | VAR String
            | SQRT (Term a)
            | PLUS (Term a) (Term a)
            | MULT (Term a) (Term a)
            | DIV (Term a) (Term a)
            | SIN (Term a)
            | COS (Term a)
            | LOG (Term a)
            | NEG (Term a)
            deriving (Eq, Show, Functor)

-- bezpieczne funkcje
mbSqrt :: Maybe Double -> Maybe Double
mbSqrt (Just x)
    | x < 0 = Nothing
    | otherwise = pure (sqrt x)
mbSqrt Nothing  = Nothing

mbDiv :: Maybe Double -> Maybe Double -> Maybe Double
mbDiv x y
    | y == pure 0 = Nothing
    | otherwise = liftA2 (/) x y

mbLog :: Maybe Double -> Maybe Double
mbLog (Just x)
    | x < 0 = Nothing
    | otherwise = pure (log x)
mbLog Nothing  = Nothing

-- ewaluacja drzewa (mając tłumacza zmiennych)
evalTerm :: (Applicative f) =>
            Term (f (Maybe Double)) ->
            (String -> f (Maybe Double)) ->
            f (Maybe Double)
evalTerm (LL x) env        = x
evalTerm (VAR v) env       = env v
evalTerm (SQRT t) env      = fmap mbSqrt (evalTerm t env)
evalTerm (PLUS t1 t2) env  = liftA2 (liftA2 (+)) (evalTerm t1 env) (evalTerm t2 env)
evalTerm (MULT t1 t2) env  = liftA2 (liftA2 (*)) (evalTerm t1 env) (evalTerm t2 env)
evalTerm (DIV t1 t2) env   = liftA2 mbDiv (evalTerm t1 env) (evalTerm t2 env)
evalTerm (SIN t) env       = fmap (fmap sin) (evalTerm t env)
evalTerm (COS t) env       = fmap (fmap cos) (evalTerm t env)
evalTerm (LOG t) env       = fmap mbLog (evalTerm t env)
evalTerm (NEG t) env       = fmap (fmap (0-)) (evalTerm t env)

-- Obsługa list zmiennych
newtype ListaVar f  = LV { getListVar :: [(String, f (Maybe Double))] }

getVar :: (Applicative f) => ListaVar f -> String -> f (Maybe Double)
getVar (LV []) _ = pure Nothing
getVar (LV ((x,v):ls)) var
    | x == var = v
    | otherwise = getVar (LV ls) var

-- ewaluacja drzewa (mając listę zmiennych)
runTerm :: (Applicative f) => Term (f (Maybe Double)) -> ListaVar f -> f (Maybe Double)
runTerm term subst = evalTerm term (getVar subst)

-- przyklad
vars = LV $ map (\(x, y) -> (x, Identity $ Just y))
    [ ("x", 3.1)
    , ("y", 2.7)
    , ("z", 4.7)]

term = 
    MULT 
        (PLUS 
            (SQRT (VAR "x")) 
            (DIV 
                (LOG (VAR "y")) 
                (SIN (VAR "z")))) 
        (NEG 
            (COS 
                (PLUS 
                    (VAR "x") 
                    (MULT 
                        (VAR "y") 
                        (VAR "z")))))

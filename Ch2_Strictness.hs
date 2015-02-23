fromJust (Just v) = v

--

data E
 = Constant Int
 | Var String
 | StrictCall String [E]
 | IfThenElse E E E
 | FunctionCall String [E]

data Fb = [String] := E
type F = (String, Fb)

--

f_f = ("f", ["x"] := IfThenElse (Var "x")
                         (StrictCall "*" [Var "x", FunctionCall "f" [StrictCall "-" [Var "x", Constant 1]]])
                         (Constant 1))
{-
*Main> interpE [f_f] [] (FunctionCall "f" [Constant 4])
24
-}

g_f = ("g", ["x", "y"] := IfThenElse (Var "x") (Constant 0) (Var "y"))

{-
*Main> interpE [g_f] [] (FunctionCall "g" [Constant 0, Constant 1])
1
*Main> interpE [g_f] [] (FunctionCall "g" [Constant 1, Constant 1])
0
-}

--

interpE :: [F] -> [(String,Int)] -> E -> Int
interpE fs s (Constant i) = i
interpE fs s (Var v) = fromJust (lookup v s)
interpE fs s (StrictCall f es) =
 strictApplyE fs f (map (interpE fs s) es)
interpE fs s (IfThenElse b t e) =
 case interpE fs s b of
  0 -> interpE fs s e
  _ -> interpE fs s t
interpE fs s (FunctionCall f es) =
 applyE fs f (map (interpE fs s) es)

strictApplyE fs "*" [x,y] = x*y
strictApplyE fs "-" [x,y] = x-y
strictApplyE _ f args = error (f ++ " called badly with " ++ show (length args) ++ " args")

applyE :: [F] -> String -> [Int] -> Int
applyE fs f args = case lookup f fs of
 Just (vars := body) -> interpE fs (zip vars args) body
 _ -> error ("No such function: " ++ f)
 
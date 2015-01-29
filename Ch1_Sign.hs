module Sign where

data Exp
 = Number Int
 | Exp :+: Exp
 | Exp :*: Exp
 deriving (Eq, Show)

t1 = Number (-413) :*: (Number 2571 :+: Number 879)

e_std :: Exp -> Int
e_std (Number i) = i
e_std (x :+: y) = e_std x + e_std y
e_std (x :*: y) = e_std x * e_std y

data Sign = Zero | Pos | Neg | Num
 deriving (Eq, Show)

sgn n
 | n < 0 = Neg
 | n == 0 = Zero
 | n > 0 = Pos

p Zero Zero = Zero
p Zero Pos = Pos
p Zero Neg = Neg
p Zero Num = Num

p Pos Zero = Pos
p Pos Pos = Pos
p Pos Neg = Num
p Pos Num = Num

p Neg Zero = Neg
p Neg Pos = Num
p Neg Neg = Neg
p Neg Num = Num

p Num Zero = Num
p Num Pos = Num
p Num Neg = Num
p Num Num = Num

m Zero Zero = Zero
m Zero Pos = Zero
m Zero Neg = Zero
m Zero Num = Zero

m Pos Zero = Zero
m Pos Pos = Pos
m Pos Neg = Neg
m Pos Num = Num

m Neg Zero = Zero
m Neg Pos = Neg
m Neg Neg = Pos
m Neg Num = Num

m Num Zero = Zero
m Num Pos = Num
m Num Neg = Num
m Num Num = Num

e_sgn :: Exp -> Sign
e_sgn (Number i) = sgn i
e_sgn (x :+: y) = e_sgn x `p` e_sgn y
e_sgn (x :*: y) = e_sgn x `m` e_sgn y


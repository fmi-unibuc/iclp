data Exp = I Integer | P Exp Exp | M Exp Exp
  deriving (Show)

data Record = Om
   { varsta :: Integer
   , barba  :: Bool
   , ochi   :: Bool
   }
  deriving Show


varsta10 :: Record -> Integer
varsta10 Om {varsta = x} = x + 10

treceAnul :: Record -> Record
treceAnul om@Om {varsta = v} = om {varsta = v + 1}

f :: Integer -> Exp
f x = let y = I x
          z = P y y
      in M z z

eval :: Exp -> Integer
eval (I x)   = x
eval (P x y) = eval x + eval y
eval (M x y) = eval x * eval y

data Semn = Negativ | Zero | Pozitiv
  deriving Show

semn :: Integer -> Semn
semn x
  | x < 0  = Negativ
  | x == 0 = Zero
  | otherwise  = Pozitiv


egal :: Eq a => a -> a -> Bool
egal x y
  | x == y = True
egal x y = False


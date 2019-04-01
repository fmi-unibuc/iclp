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

{-
data Either a b = Left a | Right b

instance Functor (Either a) where
  fmap f (Right x)  = Right (f x)
  fmap f (Left err) = Left err
-}


helloWorld :: IO ()
helloWorld = putStrLn "Hello, World!"

myIOTest :: IO ()
myIOTest = do
    x <- fmap length getLine
    print x
    myIOTest

actiune :: IO Integer
actiune = do
    line <- getLine
    let r = read line :: Integer
    return r

main :: IO ()
main = do
    val <- actiune
    let val' = val * val
    print val'
    main


class Functor f where
    fmap :: (a -> b) -> f a -> f b
    fmap f ma = do
        a <- ma
        return (f a)

class Functor f => Applicative f where
    pure :: a -> f a  -- return
    pure = return
    (<*>) :: f (a -> b) -> f a -> f b
    mf <*> ma = do
        f <- mf
        a <- ma
        return (f a)
    mf <*> ma = do
        a <- ma
        f <- mf
        return (f a)


class Applicative m => Monad m where
    return :: a -> m a -- pure
    (>>=) :: f a -> (a -> f b) -> f b



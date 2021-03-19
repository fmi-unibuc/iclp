inc x = x + 1
double x = x * 2
half x = x `div` 2

result = inc (double (half 10))


inc x k = k (x + 1)
double x k = k (x * 2)
half x k = k (x `div` 2)

result k = half 10 (\v -> double v (\v' -> inc v k))

result id


gcd a b k
  | b == 0 = k a
  | a < b = gcd b a k
  | otherwise = gcd b (a `mod` b) k

gcdstar [] = 0
gcdstar (1:_) = 1
gcdstar (x:xs) = gcd x (gcdstar xs)

gcdstar [] k = k 0
gcdstar (1:_) k = k 1
gcdstar (x:xs) k = gcdstar xs (\v -> gcd x v k)

gcdstar [] k ke = k 0
gcdstar (1:_) k ke = ke 1
gcdstar (x:xs) k ke = gcdstar xs (\v -> gcd x v k) ke

gcdstar' l k = gcdstar l k k

gcdstar' [100, 100, 100, 1, 100] id
gcdstar [100, 100, 100, 1, 100] id id
gcdstar [100, 100, 1, 100] (\v -> gcd 100 v id) id
gcdstar [100, 1, 100] (\v -> gcd 100 v (\v -> gcd 100 v id)) id
gcdstar [1, 100] (\v -> gcd 100 v (\v -> gcd 100 v (\v -> gcd 100 v id))) id
id 1
1

gcdstar' [100, 100, 100, 100] id
gcdstar [100, 100, 100, 100] id id
gcdstar [100, 100, 100] (\v -> gcd 100 v id) id
gcdstar [100, 100] (\v -> gcd 100 v (\v -> gcd 100 v id)) id
gcdstar [100] (\v -> gcd 100 v (\v -> gcd 100 v (\v -> gcd 100 v id))) id
gcdstar [] (\v -> gcd 100 v (\v -> gcd 100 v (\v -> gcd 100 v (\v -> gcd 100 v id)))) id
(\v -> gcd 100 v (\v -> gcd 100 v (\v -> gcd 100 v (\v -> gcd 100 v id)))) 0
gcd 100 0 (\v -> gcd 100 v (\v -> gcd 100 v (\v -> gcd 100 v id)))
(\v -> gcd 100 v (\v -> gcd 100 v (\v -> gcd 100 v id))) 100
gcd 100 100 (\v -> gcd 100 v (\v -> gcd 100 v id))
...

f x = x
f x k = k x

pi1 a b k = k a

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n == 2    = [(a, c), (a, b), (c, b)]
    | otherwise = (hanoi (n - 1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)

hanoiFour :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiFour n a b c d
    | n == 2    = hanoi 2 a b c
    | n == 3    = [(a, c), (a, d), (a, b), (d, b), (c,b)]
    | otherwise = (hanoiFour (n - 2) a c b d) ++ [(a, d), (a, b), (d, b)] ++ (hanoiFour (n - 2) c b a d)
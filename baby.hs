iappend [] = [([],[])]
iappend (x:xs) = ([], x:xs):map(\(a,b) -> (x:a,b)) (iappend xs)

--harmonic 0 = 0;
--harmonic n = sum[ 1/x | x <- [1..n]]

myreplace :: Eq a => a -> a -> [a] -> [a]
myreplace a b [] = []
myreplace a b (x:xs)
    |x == a = b:(myreplace a b xs)
    |otherwise = x:(myreplace a b xs)

myapply :: Eq a => [a] -> [(a,a)] -> [a]
myapply x [] = x
myapply [] x = []
myapply [x] ((a,b):t)
    |x == a = [b]
    |x /= a = myapply [x] t

myapply (x:xs) ((a,b):t) = myapply [x] ((a,b):t)++(myapply xs ((a,b):t))

--myordered :: Ord a => [a] -> Bool
myordered [] = True
myordered [a] = True
-- myordered (x:y:xs)
    -- |x > y = False
    -- |otherwise = myordered (y:xs)
myordered lst = snd(foldl(\acc x -> if x >= fst(acc) && snd(acc) == True then (x,True) else (x,False)) (0,True) lst)

myrmd :: Eq a => [a] -> [a]
myrmd [] = []
myrmd [a] = [a]
myrmd (x:xs) = foldl(\acc x -> if (x `elem` acc) == False then acc++[x] else acc) [] (x:xs)
--myrmd (x:xs)
    -- |x `elem` xs = myrmd xs
    -- |otherwise = x:myrmd xs

deln :: Eq a => Int -> a -> [a] -> [a]
deln n c [] = []
deln n c (x:xs)
    |c == x && n > 0 = (deln (n-1) c xs)
    |otherwise = x:(deln n c xs)

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = foldr(\x (a:as) -> (x:a):(a:as)) [[]] (x:xs)

harmonic n = foldl(\acc x -> (1/x)+acc) 0 [1..n]




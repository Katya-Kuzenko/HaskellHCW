-- EX. 1

-- Part A
diff :: (Double -> Double) -> Double -> Double -> Double
diff f dx x = (f (x + dx) - f x) / dx

ex1partA = do
    let func1 = \x -> x^2
    print "Ex. 1 Part A"
    print "Function diff for f = x^2"
    print $ diff func1 0.0001 2.0 -- 4.0001000000078335
    print "Analytical calculation for f = x^2"
    print $ 2 * 2.0

-- Part B
newton_iter :: (Double -> Double) -> Double -> Double -> Int -> Double
newton_iter f f' x k
    | k <= 0 = x
    | otherwise = newton_iter f f' (x - (f x) / f') (k - 1)

ex1partB = do -- щось не дуже правдоподібно вийшло 
    let func2 = \x -> sin x
    let func2' x = cos x
    let func3 = \x -> x^3 - 328*x^2 - 1999*x - 1670
    let func3' x = 3*x^2 - 656*x - 1999
    print "Ex. 1 Part B"
    print "Function newton_iter for f = sin(x)"
    print $ newton_iter func2 (diff func2 0.01 0.5) 0.5 10
    print "Analytical calculation for f = sin(x)"
    print $ func2' 0.5
    print "Function newton_iter for f = x^3 - 328x^2 - 1999x - 1670"
    print $ newton_iter func3 (diff func3 0.01 100) 100 10
    print "Analytical calculation for f = x^3 - 328x^2 - 1999x - 1670"
    print $ func3' 100

ex1 = do
    ex1partA
    ex1partB

-- EX. 2
type IntSet = (Int -> Bool)

isMember :: IntSet -> Int -> Bool
isMember f x = f x

-- Part A
emptySet :: IntSet
emptySet x = False
allInts :: IntSet
allInts x = True 

-- Part B
interval :: Int -> Int -> IntSet
interval lBound uBound = \x -> x >= lBound && x <= uBound

ex2 = do
    print $ isMember emptySet 5
    print $ isMember allInts 42
    print $ isMember (interval 0 5) 10
    print $ isMember (interval 0 5) 3
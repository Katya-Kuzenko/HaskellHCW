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

-- Part C
euclidFunc :: Int -> IntSet
euclidFunc k = \x -> euc x k == 1
    where 
        euc :: Int -> Int -> Int
        euc a 0 = a 
        euc a b = euc b (a `mod` b)

-- Part D
setIntersection :: IntSet -> IntSet -> IntSet -- перетин
setIntersection setA setB = \x -> setA x && setB x 

setUnion        :: IntSet -> IntSet -> IntSet -- об'єднання
setUnion setA setB = \x -> setA x || setB x 

setComplement   :: IntSet -> IntSet -> IntSet -- доповнення
setComplement setA setB = \x -> setA x && not (setB x)

addToSet      :: Int -> IntSet -> IntSet
addToSet a set = setUnion set (\x -> x == a)

deleteFromSet :: Int -> IntSet -> IntSet
deleteFromSet a set = setIntersection set (\x -> x /= a)

ex2 = do
    print "Ex. 2"
    print "Check the function emptySet"
    print $ isMember emptySet 5 -- False
    print "Check the function allInts"
    print $ isMember allInts 42 -- True
    print "Check the function interval"
    print $ isMember (interval 0 5) 10 -- False
    print $ isMember (interval 0 5) 3 -- True
    print "Check the function euclidFunc"
    print $ isMember (euclidFunc 5) 3 -- True
    print $ isMember (euclidFunc 8) 2 -- False
    print "Check the function setIntersection"
    print $ isMember (setIntersection (interval 1 5) (interval 3 10)) 4 -- True
    print $ isMember (setIntersection (interval 1 5) (interval 3 10)) 6 -- False
    print "Check the function setUnion"
    print $ isMember (setUnion (interval 1 5) (interval 3 10)) 4 -- True
    print $ isMember (setUnion (interval 1 5) (interval 3 10)) 6 -- True
    print $ isMember (setUnion (interval 1 5) (interval 3 10)) 12 -- False
    print "Check the function setComplement"
    print $ isMember (setComplement allInts (interval 1 10)) 5 -- False
    print $ isMember (setComplement allInts (interval 1 10)) 15 -- True
    print "Check the function addToSet"
    print $ isMember (addToSet 4 (interval 1 3)) 4 -- True
    print $ isMember (addToSet 4 (interval 1 3)) 2 -- True
    print "Check the function deleteFromSet"
    print $ isMember (deleteFromSet 4 (interval 1 5)) 4 -- False
    print $ isMember (deleteFromSet 4 (interval 1 5)) 1 -- True


main = do
    ex1
    ex2
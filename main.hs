-- EX. 1

-- Part A
diff :: (Double -> Double) -> Double -> Double -> Double
diff f dx x = (f (x + dx) - f x) / dx

func :: Double -> Double -- функція для порівняння результату виводу та аналітичного результату
func = \x -> x^2

partA = do
    print "Ex. 1 Part A"
    print $ diff func 0.0001 2.0 -- 4.0001000000078335 (аналітичний результат - 4.0)
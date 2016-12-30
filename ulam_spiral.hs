ulamCorners :: (Num a, Enum a) => a -> [a]
ulamCorners n = [base + c*oddN | c <- [0..3], let oddN = (2*n) - 1, let base = oddN^2 + oddN + 1 + c]

isPrime :: Integral a => a -> Bool
isPrime 2 = True
isPrime n =
    let upperBound = floor . sqrt . fromIntegral $ n
        in case [x | x <- 2:[3,5..upperBound], mod n x == 0] of
            [] -> True
            (x:xs) -> False

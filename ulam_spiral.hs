corners n = [base + c*oddN | c <- [0..3], let oddN = (2*n) - 1, let base = oddN^2 + oddN + 1]

factors 1 = []
factors n = k : (factors $ div n k)
    where k = head [x | x <- [2..], mod n x == 0]

isPrime n = n == (head $ factors n)

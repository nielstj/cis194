type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 beg _ end = [(beg, end)]
hanoi n beg aux end = hanoi (n-1) beg end aux 
                      ++ hanoi 1 beg aux end 
                      ++ hanoi (n-1) aux beg end

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = [] 
hanoi4 1 beg _ _ end = [(beg, end)]
hanoi4 n beg aux1 aux2 end = hanoi4 (n - 2) beg aux2 end aux1
                             ++ hanoi4 1 beg aux1 end aux2
                             ++ hanoi4 1 beg aux1 aux2 end
                             ++ hanoi4 1 aux2 beg aux1 end
                             ++ hanoi4 (n - 2) aux1 beg aux2 end

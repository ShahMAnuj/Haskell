firstLetter :: [Char] -> [Char]
firstLetter xs = xs !! 0 : " is for " ++ xs

speller :: [[Char]] -> [Char]
speller [] = []
speller xs = speller_init xs ++ speller_last xs

speller_init :: [[Char]] -> [Char]
speller_init xs = foldl (\x y -> x ++ y ++ ", ") [] (map firstLetter (init xs))

speller_last :: [[Char]] -> [Char]
speller_last xs = firstLetter (last xs)

type Card = ([Integer], [Integer])

main = do
    input <- getContents
    let cards   = map parse (lines input)
        matches = map (length . countMatches) cards
        counts  = update [1 | card <- cards] matches
     in
        print (sum counts)

update :: [Int] -> [Int] -> [Int]
update _ []          = []
update (c:cs) (m:ms) = c : update (map (+c) (take m cs) ++ drop m cs) ms

countMatches :: Card -> [Integer]
countMatches (winners,numbers) = filter (`elem` winners) numbers

parse :: String -> Card
parse s =
    let content         = drop 2 (words s)
        (list1,_:list2) = break (== "|") content
    in
        (map read list1, map read list2)

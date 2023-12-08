type Card = ([Integer], [Integer])

main = do
    input <- getContents
    let cards  = map parse (lines input)
        values = map value cards
     in
        print (sum values)

value :: Num a => Card -> a
value (winners,numbers) =
    let count = length $ filter (`elem` winners) numbers
    in
        if count == 0
            then fromIntegral count
            else 2^(count-1)

parse :: String -> Card
parse s =
    let content         = drop 2 (words s)
        (list1,_:list2) = break (== "|") content
    in
        (map read list1, map read list2)

main = do
    input <- getContents
    let lines'      = map parse (lines input)
        predictions = map previous lines'
     in
        print (sum predictions)
    where
        parse = map read . words

previous :: [Integer] -> Integer
previous nums = head nums - slope nums where
    slope :: [Integer] -> Integer
    slope values =
        let differences = [b - a | (a, b) <- zip values (tail values)]
        in
            if all (== 0) differences
                then 0
                else head differences - slope differences

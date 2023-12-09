main = do
    input <- getContents
    let lines'      = map parse (lines input)
        predictions = map next lines'
     in
        print (sum predictions)
    where
        parse = map read . words

next :: [Integer] -> Integer
next nums = last nums + slope nums where
    slope values =
        let differences = [b - a | (a, b) <- zip values (tail values)]
        in
            if all (== 0) differences
                then 0
                else last differences + slope differences

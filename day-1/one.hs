main = do
    input <- getContents
    let total = sum $ map calibrationValue (lines input) in
        print total

calibrationValue :: String -> Integer
calibrationValue s =
    let d1 = firstDigit s
        d2 = lastDigit s
    in
        d1*10 + d2
    where
        firstDigit :: String -> Integer
        firstDigit "" = error "no digit found"
        firstDigit (x:xs) =
            case parseDigit x of
                Just n  -> n
                Nothing -> firstDigit xs

        lastDigit :: String -> Integer
        lastDigit = firstDigit . reverse

        parseDigit :: Char -> Maybe Integer
        parseDigit d =
            case d of
                '0' -> Just 0
                '1' -> Just 1
                '2' -> Just 2
                '3' -> Just 3
                '4' -> Just 4
                '5' -> Just 5
                '6' -> Just 6
                '7' -> Just 7
                '8' -> Just 8
                '9' -> Just 9
                _   -> Nothing

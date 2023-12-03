import Data.List (isPrefixOf)

main = do
    input <- getContents
    let total = sum $ map calibrationValue (lines input) in
        print total

calibrationValue :: String -> Integer
calibrationValue s =
    let d1     = head digits
        d2     = last digits
        digits = findDigits s
    in
        d1*10 + d2
    where
        findDigits :: String -> [Integer]
        findDigits "" = []
        findDigits s@(x:xs) =
            case x of
                '0' -> 0 : findDigits xs
                '1' -> 1 : findDigits xs
                '2' -> 2 : findDigits xs
                '3' -> 3 : findDigits xs
                '4' -> 4 : findDigits xs
                '5' -> 5 : findDigits xs
                '6' -> 6 : findDigits xs
                '7' -> 7 : findDigits xs
                '8' -> 8 : findDigits xs
                '9' -> 9 : findDigits xs
                _   -> found ++ findDigits xs
                    where
                        found = take 1 $ map convert $ filter (`isPrefixOf` s) numbers
                        numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

                        convert :: String -> Integer
                        convert s =
                            case s of
                                "one"   -> 1
                                "two"   -> 2
                                "three" -> 3
                                "four"  -> 4
                                "five"  -> 5
                                "six"   -> 6
                                "seven" -> 7
                                "eight" -> 8
                                "nine"  -> 9
                                _       -> error "unrecognized number"

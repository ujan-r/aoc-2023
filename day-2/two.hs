import Data.Maybe (fromJust)
import Data.Text (splitOn, unpack)
import Data.String (fromString)

type Counts = (Integer, Integer, Integer)

main = do
    input <- getContents
    print $ sum $ map (power . minimumCubes . snd . parse) (lines input)

parse :: String -> (Integer, [Counts])
parse s =
    let id   = parseNumber $ head parts
        game = parseGame   $ last parts
    in
        (fromJust id, game)
    where
        parts  = map unpack parts'
        parts' = splitOn sep s'
        s'     = fromString s
        sep    = fromString ": "

        parseNumber :: String -> Maybe Integer
        parseNumber = parseNumber' Nothing where

            parseNumber' :: Maybe Integer -> String -> Maybe Integer
            parseNumber' previous "" = previous
            parseNumber' previous (x:xs) =
                let digit = parseDigit x
                in
                    case previous of
                        Nothing -> parseNumber' digit xs
                        Just n  -> case digit of
                            Nothing -> Just n
                            Just digit -> parseNumber' (Just $ 10*n + digit) xs
                where
                    parseDigit :: Char -> Maybe Integer
                    parseDigit d = case d of
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

        parseGame :: String -> [Counts]
        parseGame s =
            let rounds  = map unpack rounds'
                rounds' = splitOn roundSep s'
            in
                map parseRound rounds
            where
                roundSep = fromString "; "
                s'       = fromString s

                parseRound :: String -> Counts
                parseRound s =
                    let counts = map toCountAndColor parts
                        parts  = map unpack parts'
                        parts' = splitOn colorSep s'
                    in
                        parseRound' (0, 0, 0) counts
                    where
                        colorSep = fromString ", "
                        s'       = fromString s

                        toCountAndColor :: String -> (Integer, String)
                        toCountAndColor s =
                            let count = fromJust $ parseNumber (head parts)
                                color = last parts
                            in
                                (count, color)
                            where
                                parts  = map unpack parts'
                                parts' = splitOn sep s'
                                s'     = fromString s
                                sep    = fromString " "


                        parseRound' :: Counts -> [(Integer, String)] -> Counts
                        parseRound' counts [] = counts
                        parseRound' (r,g,b) ((n,color):xs) =
                            case color of
                                "red"   -> parseRound' (r+n, g, b) xs
                                "green" -> parseRound' (r, g+n, b) xs
                                "blue"  -> parseRound' (r, g, b+n) xs

minimumCubes :: [Counts] -> Counts
minimumCubes = minimumCubes' (0, 0, 0)
    where
        minimumCubes' :: Counts -> [Counts] -> Counts
        minimumCubes' counts [] = counts
        minimumCubes' (r',g',b') ((r,g,b):xs) =
            let max_r = max r r'
                max_g = max g g'
                max_b = max b b'
            in
                minimumCubes' (max_r, max_g, max_b) xs

power :: Counts -> Integer
power (r,g,b) = r * g * b

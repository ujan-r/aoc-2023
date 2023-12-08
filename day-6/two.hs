type Race = (Integer, Integer)

main = do
    input <- getContents
    let race  = parse input
        time  = validHoldTimes race
        count = length time
     in
        print count

parse :: String -> Race
parse s =
    let s1:s2:_ = lines s
        time'   = concat $ tail (words s1)
        record' = concat $ tail (words s2)
    in
        (read time', read record')

validHoldTimes :: Race -> [Integer]
validHoldTimes race@(time,record) =
    takeWhile valid $ dropWhile (not . valid) [0..time]
    where
        valid holdTime = distance race holdTime > record

distance :: Race -> Integer -> Integer
distance (time,distance) hold =
    let speed = hold
        time' = time - hold
    in
        speed * time'

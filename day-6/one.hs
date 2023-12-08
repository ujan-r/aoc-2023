type Race = (Integer, Integer)

main = do
    input <- getContents
    let races  = parse input
        times  = map validHoldTimes races
        counts = map length times
     in
        print (product counts)

parse :: String -> [Race]
parse s =
    let [s1,s2] = lines s
        times   = map read $ tail (words s1)
        records = map read $ tail (words s2)
    in
        zip times records

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

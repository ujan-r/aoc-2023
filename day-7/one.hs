import Data.List (sortBy, sort, group)

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine
          | T | J | Q | K | A
    deriving (Eq, Ord, Show)

instance Read Card where
    readsPrec _ input =
        let x:xs = input
        in
            case parseCard x of
                Just c  -> [(c, xs)]
                Nothing -> []
        where
            parseCard :: Char -> Maybe Card
            parseCard x = case x of
                'A' -> Just A
                'K' -> Just K
                'Q' -> Just Q
                'J' -> Just J
                'T' -> Just T
                '9' -> Just Nine
                '8' -> Just Eight
                '7' -> Just Seven
                '6' -> Just Six
                '5' -> Just Five
                '4' -> Just Four
                '3' -> Just Three
                '2' -> Just Two
                _   -> Nothing

type Hand = [Card]

main = do
    input <- getContents
    let list     = parse input
        sorted   = sortBy (\(h1,_) (h2,_) -> compare' h1 h2) list
        bids     = map snd sorted
        winnings = [rank * bid | (rank, bid) <- zip [1..] bids]
     in
        print (sum winnings)

parse :: String -> [(Hand, Integer)]
parse = map parseLine . lines
    where
        parseLine :: String -> (Hand, Integer)
        parseLine s =
            let [cards',bid'] = words s
                hand          = [read [c] | c <- cards']
                bid           = read bid'
            in
                (hand, bid)

compare' :: Hand -> Hand -> Ordering
compare' h1 h2 =
    case compare (strength h1) (strength h2) of
        LT -> LT
        GT -> GT
        EQ -> compare h1 h2
    where
        strength :: Hand -> Integer
        strength cards =
            let groups = group (sort cards)
                counts = map length groups
            in
                case sort counts of
                    [5]             -> 6    -- five of a kind
                    [1, 4]          -> 5    -- four of a kind
                    [2, 3]          -> 4    -- full house
                    [1, 1, 3]       -> 3    -- three of a kind
                    [1, 2, 2]       -> 2    -- two pair
                    [1, 1, 1, 2]    -> 1    -- one pair
                    [1, 1, 1, 1, 1] -> 0    -- high card

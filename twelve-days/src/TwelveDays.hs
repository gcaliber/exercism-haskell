module TwelveDays (recite) where

gifts = ["twelve Drummers Drumming, ", 
         "eleven Pipers Piping, ", 
         "ten Lords-a-Leaping, ",
         "nine Ladies Dancing, ",
         "eight Maids-a-Milking, ", 
         "seven Swans-a-Swimming, ",
         "six Geese-a-Laying, ",
         "five Gold Rings, ",
         "four Calling Birds, ", 
         "three French Hens, ",
         "two Turtle Doves, ",
         "and a Partridge in a Pear Tree."]

ordinal = ["second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"]

recite :: Int -> Int -> [String]
recite start stop | start > stop = []
                  | start == 1   = "On the first day of Christmas my true love gave to me, a Partridge in a Pear Tree."
                                   : recite (start + 1) stop
                  | otherwise    = ("On the " ++ ordinal !! (start - 2) ++ " day of Christmas my true love gave to me, " 
                                   ++ concat (drop (12 - start) gifts)) : recite (start + 1) stop


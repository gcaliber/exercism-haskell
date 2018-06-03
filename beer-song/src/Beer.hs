module Beer (song) where

song :: String
song = sing 99

sing :: Int -> String
sing 0 = "No more bottles of beer on the wall, no more bottles of beer.\n\
         \Go to the store and buy some more, 99 bottles of beer on the wall.\n"
sing n | n == 2 = "2 bottles of beer on the wall, 2 bottles of beer.\n\
                  \Take one down and pass it around, 1 bottle of beer on the wall.\n\n"  ++ sing (n - 1)
       | n == 1 = "1 bottle of beer on the wall, 1 bottle of beer.\n\
                  \Take it down and pass it around, no more bottles of beer on the wall.\n\n"  ++ sing (n - 1)
       | otherwise = show n ++ " bottles of beer on the wall, " ++ show n ++ " bottles of beer.\n\
                     \Take one down and pass it around, " ++ show (n - 1) ++ " bottles of beer on the wall.\n\n" ++ sing (n - 1)
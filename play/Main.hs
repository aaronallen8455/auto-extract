main :: IO ()
main = pure ()

foo = let ttt = 12 in EXTRACT@hello (ttt + 19 * bar)

bar = 13

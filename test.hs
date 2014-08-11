import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "watch.hs", "optionparser.hs"]


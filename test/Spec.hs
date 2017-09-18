import Test.Hspec

import qualified SpecLexer as L

main :: IO ()
main = do
  _ <- L.test
  putStrLn "\n3 test groups executed, check the console above to see the results"
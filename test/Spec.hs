import Test.Hspec

import qualified SpecGenerator as G
import qualified SpecParser as P
import qualified SpecLexer as L

main :: IO ()
main = do
  L.test
  P.test
  G.test
  putStrLn "\n3 test groups executed, check the console above to see the results"
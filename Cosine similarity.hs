import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

cosineSimilarity :: [Int] -> [Double] -> [Int] -> [Double] -> Double
cosineSimilarity a_keys a_values b_keys b_values =
  let
    -- Combine keys and values into key-value pairs
    aPairs = zip a_keys a_values
    bPairs = zip b_keys b_values

    -- Create lookup function for key-value pairs
    lookupValue :: Int -> [(Int, Double)] -> Double
    lookupValue k = maybe 0 id . lookup k

    -- Get common keys
    commonKeys = intersect a_keys b_keys

    -- Compute dot product
    dotProduct = sum [ (lookupValue k aPairs) * (lookupValue k bPairs) | k <- commonKeys ]

    -- Compute norm of a vector
    norm vec = sqrt $ sum [ v * v | (_, v) <- vec ]

    normA = norm aPairs
    normB = norm bPairs
  in
    if normA == 0 || normB == 0
      then 0.0
      else dotProduct / (normA * normB)
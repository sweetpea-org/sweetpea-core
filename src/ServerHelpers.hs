module ServerHelpers where

import Data.List
import Data.Text (Text, pack, unpack, split, strip)

import DataStructures


strToInt :: Text -> Int
strToInt str = read $ unpack str


addClauseToHeader :: String -> String
addClauseToHeader = updateHeader 1


updateHeader :: Int -> String -> String
updateHeader additionalClauseCount header =
  let segments = split (==' ') (strip (pack header))
  in let newClauseCount = (strToInt (segments !! 3)) + additionalClauseCount
     in unwords $ map unpack ((take 3 segments) ++ [pack $ show newClauseCount])


cnfToLine :: [Int] -> String
cnfToLine cnf =
  intercalate " " ((map show cnf) ++ ["0"])


cnfsToLines :: CNF -> [String]
cnfsToLines cnfs =
  map cnfToLine cnfs


-- Appends a set of clauses to an existing CNF string, updates the header, and returns the new CNF string.
addClausesToCnf :: String -> CNF -> String
addClausesToCnf cnfStr cnfs =
  let lines = split (=='\n') (strip $ pack cnfStr) in
    let updatedHeader = updateHeader (length cnfs) (unpack $ lines !! 0) in
      let updatedLines = [(pack updatedHeader)] ++ (drop 1 lines) ++ (map pack (cnfsToLines cnfs)) in
        intercalate "\n" (map unpack updatedLines)


parseCMSatSolution :: String -> [Int]
parseCMSatSolution output = do
  -- If it's UNSAT, then return an empty list
  if isInfixOf "s UNSATISFIABLE" output
    then []
    -- Get solution lines from output.
    else let lines = filter (isPrefixOf "v") (map (unpack . strip) (split (=='\n') (strip (pack output))))
         in let intStrs = split (==' ') $ strip $ pack $ (filter (/='v') (foldr (++) "" lines))
            in map strToInt intStrs


isFormulaSat :: String -> Bool
isFormulaSat cmSatOutput = isInfixOf "s SATISFIABLE" cmSatOutput

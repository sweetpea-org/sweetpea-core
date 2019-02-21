
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Parser

where

import Data.Aeson
import Data.Maybe
import qualified Data.Map as Map
import GHC.Generics
import Control.Monad.Trans.State
import DataStructures
import SweetPeaCore
import CodeGen


data Request = Request { equalityType :: Ordering
                       , k :: Int
                       , booleanValues :: [Int]
                       } deriving (Generic, Eq, Show, ToJSON, FromJSON)

data ActionType = BuildCNF
                | SampleNonUniform
                | IsSAT
                deriving (Generic, Eq, Enum, Show, ToJSON, FromJSON)

data JSONSpec = JSONSpec { action :: Maybe ActionType
                         , fresh :: Maybe Int
                         , cnfs :: Maybe CNF
                         , cnfId :: Maybe String
                         , support :: Maybe Int
                         , requests :: Maybe [Request]
                         , unigenOptions :: Maybe [String]
                         , sampleCount :: Maybe Int
                         } deriving (Generic, Eq, Show, ToJSON, FromJSON)


processRequests :: JSONSpec -> String
processRequests spec = showDIMACS finalCnf finalNVars (fromJust (support spec))
  where (finalNVars, finalCnf) = (finalNVars, cnf ++ (fromJust (cnfs spec)))
          where (finalNVars, cnf) = execState (mapM processARequest (fromJust (requests spec))) $ initState (fromJust (fresh spec))

processARequest :: Request -> State (Count, CNF) ()
processARequest (Request EQ k boolVals) = assertKofN    k boolVals
processARequest (Request LT k boolVals) = kLessThanN    k boolVals
processARequest (Request GT k boolVals) = kGreaterThanN k boolVals

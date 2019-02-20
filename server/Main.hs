{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main where

import Web.Spock
import Web.Spock.Config

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import GHC.Generics
import Data.Aeson hiding (json)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.UUID
import Data.UUID.V4 as UUID

import System.Exit
import System.IO
import System.Directory
import System.Process

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified System.IO.Strict as S
import qualified Database.Redis as R
import qualified Network.HTTP.Types.Status as Http

import Parser
import ServerHelpers

data SolutionSpec = SolutionSpec { assignment :: [Int]
                                 , frequency :: Int
                                 } deriving (Generic, Eq, Show, ToJSON, FromJSON)

data ResponseSpec = ResponseSpec { ok :: Bool
                                 , solutions :: [SolutionSpec]
                                 , count :: Int
                                 , stdout :: String
                                 , stderr :: String
                                 } deriving (Generic, Eq, Show, ToJSON, FromJSON)

data JobStatus = InProgress
               | Ready
               deriving (Generic, Eq, Enum, Show, ToJSON, FromJSON)

data JobResponseSpec = JobResponseSpec { id :: String
                                       , status :: JobStatus
                                       , result :: Maybe String
                                       } deriving (Generic, Eq, Show, ToJSON, FromJSON)

data ServerState = RedisAppState (R.Connection)

type Api = SpockM () () ServerState ()
type ApiAction a = SpockAction () () ServerState a

serverCfg :: IO (SpockCfg () () ServerState)
serverCfg = do
    redisConn <- R.checkedConnect R.defaultConnectInfo
    spockConfig <- defaultSpockCfg () PCNoDatabase (RedisAppState redisConn)
    return spockConfig { spc_maxRequestSize = Nothing } -- Disable the request size limit

main :: IO ()
main = do
  spockCfg <- serverCfg
  runSpock 8080 (spock spockCfg app)

app :: Api
app =
  do get root $
       text "Healthy\n"

     -- This is a helper endpoint to allow the frontend to just generate the CNF file,
     -- without doing any solving/sampling, for inspection/troubleshooting.
     post "experiments/build-cnf" $ do
       spec <- jsonBody' :: ApiAction JSONSpec
       let dimacsStr = B8.pack $ processRequests spec
       bytes dimacsStr

     -- Synchronous endpoint for generating a CNF and invoking unigen.
     post "experiments/generate" $ do
       spec <- jsonBody' :: ApiAction JSONSpec
       guid <- liftIO $ toString <$> UUID.nextRandom
       let filename = guid ++ ".cnf"
       let outputFilename = guid ++ ".out"
       liftIO $ saveCnf filename spec

       let args = case (arguments (fromJust (unigen spec))) of
             Nothing -> []
             Just argList -> argList
       (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode "unigen" (args ++ [filename, outputFilename]) ""

       liftIO $ removeFile filename
       -- Perusing the code, it appears that unigen uses an exit code of 0 to indicate error, while positive
       -- exit codes seem to indicate some form of success.
       -- https://bitbucket.org/kuldeepmeel/unigen/src/4677b2ec4553b2a44a31910db0037820abdc1394/ugen2/cmsat/Main.cpp?at=master&fileviewer=file-view-default#Main.cpp-831
       if exitCode == ExitSuccess
         then json $ ResponseSpec False [] (-1) stdout stderr
         else do
           solutionFileStr <- liftIO $ readSolutionFile outputFilename
           liftIO $ removeFile outputFilename
           json $ ResponseSpec True (extractSolutions solutionFileStr) (-1) "" ""

     -- Synchronous endpoint for generating a CNF and repeatedly invoking a SAT solver to
     -- compute some number of non-uniformly sampled solutions.
     post ("experiments/generate/non-uniform" <//> var) $ \count -> do
       spec <- jsonBody' :: ApiAction JSONSpec
       guild <- liftIO $ toString <$> UUID.nextRandom
       let filename = guild ++ ".cnf"
       liftIO $ saveCnf filename spec

       solutions <- liftIO $ computeSolutions filename (support (fromJust (unigen spec))) count []
       liftIO $ removeFile filename

       json $ ResponseSpec True (map ((flip SolutionSpec) 1) solutions) (-1) "" ""

     -- Submit a job for asynchronous processing
     -- The type of job as well as any job-specific parameters are embedded in the 'job' field of the request body.
     post "experiments/jobs" $ do
       spec <- jsonBody' :: ApiAction JSONSpec
       (RedisAppState redisConn) <- getState

       -- Generate a UUID for this job
       guid <- liftIO $ toString <$> UUID.nextRandom

       -- Record it as started
       liftIO $ R.runRedis redisConn $ do
         R.hset (B8.pack guid) "submitted" "yes"
         return ()

       -- Spin up a thread that will:
       -- * Execute the desired action
       -- * Populate the redis cache when done, using the UUID as the key
       liftIO $ forkIO (processJob spec redisConn guid)

       -- Return the job id
       json $ JobResponseSpec guid InProgress Nothing

     -- Get the status of a previously submitted job.
     get ("experiments/jobs" <//> var) $ \guidString -> do
       (RedisAppState redisConn) <- getState
       let guid = B8.pack guidString

       submitted <- liftIO $ R.runRedis redisConn $ do
         R.hget guid "submitted"

       result <- liftIO $ R.runRedis redisConn $ do
         R.hget guid "result"

       case submitted of
         Right (Just "yes") -> json $ buildJobResponse guid result
         Right Nothing      -> setStatus Http.status404
         Left reply         -> error (show reply)


processJob :: JSONSpec -> R.Connection -> String -> IO ()
processJob request redisConn guid = do
  -- Extract the action type
  let actionTypeValue = (actionType $ fromJust (action request))

  -- Start computing the action
  result <- liftIO $ case actionTypeValue of
                       BuildCNF -> buildCnf request
                       SampleNonUniform -> sampleNonUniform request guid
                       IsSAT -> determineSAT redisConn request

  -- Save the result
  R.runRedis redisConn $ do
    R.hset (B8.pack guid) "result" (B8.pack result)
    R.expire (B8.pack guid) 86400
    return ()


buildJobResponse :: B8.ByteString -> (Either R.Reply (Maybe B8.ByteString)) -> JobResponseSpec
buildJobResponse guid (Right (Just value)) = JobResponseSpec (B8.unpack guid) Ready (Just (B8.unpack value))
buildJobResponse guid (Right Nothing)      = JobResponseSpec (B8.unpack guid) InProgress Nothing
buildJobResponse guid jobValue             = error (show jobValue)


getRedisValue :: (Either R.Reply (Maybe B8.ByteString)) -> Maybe String
getRedisValue (Right (Just value)) = Just (B8.unpack value)
getRedisValue (Right Nothing)      = Nothing
getRedisValue value                = error (show value)


buildCnf :: JSONSpec -> IO String
buildCnf request =
  return (processRequests request)


sampleNonUniform :: JSONSpec -> String -> IO String
sampleNonUniform request guid = do
  let filename = guid ++ ".cnf"
  let actionParams = (fromJust (parameters $ fromJust (action request))) :: Map.Map String String
  let paramLookup = Map.lookup "count" actionParams
  let count = read (fromJust paramLookup) :: Int

  liftIO $ saveCnf filename request

  solutions <- liftIO $ computeSolutions filename (support (fromJust (unigen request))) count []
  liftIO $ removeFile filename

  return (BL8.unpack $ encode $ ResponseSpec True (map ((flip SolutionSpec) 1) solutions) (-1) "" "")


-- Given a request containing cnfs and a cnfId, this function will append the cnfs to the file referenced
-- by cnfId in the cache, and then run cryptominisat to see if it is still SAT.
determineSAT :: R.Connection -> JSONSpec -> IO String
determineSAT redisConn request = do
  let baseGuid = fromJust (cnfId request)
  let cnfList  = fromJust (cnfs request)

  -- Get the existing CNF from the cache
  baseCnfResponse <- liftIO $ R.runRedis redisConn $ do
    R.hget (B8.pack baseGuid) "result"
  let baseCnf = fromJust (getRedisValue baseCnfResponse)

  -- Append the new CNFs and update the clause count
  let updatedCnf = addClausesToCnf baseCnf cnfList

  -- Write the modified CNF to a new guid file
  guid <- liftIO $ toString <$> UUID.nextRandom
  let filename = guid ++ ".cnf"
  liftIO $ writeFile filename updatedCnf

  -- Run cryptominisat
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode "cryptominisat5" ["--verb=0", filename] ""

  -- Cleanup the file
  liftIO $ removeFile filename

  return (show $ isFormulaSat stdout)


saveCnf:: String -> JSONSpec -> IO ()
saveCnf filename spec =
  let dimacsStr = processRequests spec
    in writeFile filename dimacsStr


readSolutionFile :: String -> IO String
readSolutionFile filename = do
  readFile filename


extractSolutions :: String -> [SolutionSpec]
extractSolutions solutionStr =
  let lines = T.split (=='\n') (T.strip (T.pack solutionStr))
  in map buildSolution lines


buildSolution :: T.Text -> SolutionSpec
buildSolution sol = do
  let intStrs = T.split (==' ') (T.pack [c | c <- T.unpack $ T.strip sol, not (c == 'v')])
    in let assignment = map strToInt (take ((length intStrs) - 1) intStrs)
           frequency = strToInt $ last $ T.split (==':') (last intStrs)
       in SolutionSpec assignment frequency


extractCount :: String -> Int
extractCount output =
  let lines = T.split (=='\n') (T.strip (T.pack output))
  in strToInt $ lines !! 0


computeSolutions :: String -> Int -> Int -> [[Int]] -> IO [[Int]]
computeSolutions filename support count solutions = do
  if count == 0
    then return solutions
    else do
    -- Invoke cryptominisat to get a solution
    (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode "cryptominisat5" ["--verb=0", filename] ""

    -- Extract assignment from stdout
    let rawSolution = parseCMSatSolution stdout

    -- Quit if the formula was unsat
    if rawSolution == []
      then return solutions
      else do let solution = take support rawSolution

              -- Update the file to not include this solution.
              liftIO $ updateFile filename solution

              -- Go for another round
              computeSolutions filename support (count - 1) (solutions ++ [solution])


updateFile :: String -> [Int] -> IO ()
updateFile filename solution = do
  -- Load the CNF file, split into lines
  -- Read strictly to ensure we can rewrite the file afterwards.
  contents <- liftIO $ S.readFile filename
  let lines = T.split (=='\n') (T.strip (T.pack contents))

  -- Update the clause count.
  let updatedHeader = addClauseToHeader (T.unpack (lines !! 0))

  -- Negate the given solution
  let negatedSolution = map (*(-1)) solution
  let negatedSolutionStr = unwords $ map show (negatedSolution ++ [0])

  -- Append it to the end of the CNF file, with a 0 at the end.
  let updatedLines = [(T.pack updatedHeader)] ++ (drop 1 lines) ++ [(T.pack negatedSolutionStr)]

  -- Rewrite file to disk.
  let updatedContents = (intercalate "\n" (map T.unpack updatedLines))
  writeFile filename updatedContents

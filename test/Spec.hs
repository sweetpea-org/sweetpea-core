{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Trans.State
import Control.Monad (replicateM)

import DataStructures
import ServerHelpers

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [dataStructureTests, serverTests]

dataStructureTests :: TestTree
dataStructureTests = testGroup "DataStructure Tests" [helperTests]

serverTests :: TestTree
serverTests = testGroup "Server Tests"
  [ strToIntTests
  , addClauseToHeaderTests
  , updateHeaderTests
  , cnfsToLinesTests
  , addClausesToCnfTests
  , parseCMSatSolutionTests
  , isFormulaSatTests
  ]

---------------------------------------------------------------------------------------------------------------
-- DataStructures tests

helperTests = testGroup "var/cnf utility tests"
  [ testCase "getFresh" $
      runState getFresh emptyState
        @?= (1,(1,[]))
  , testCase "getNFresh" $
      runState (getNFresh 3) emptyState
        @?= ([1,2,3],(3,[]))
  , testCase "nested getNFresh" $
      runState (getNFresh 3) (execState (getNFresh 3) emptyState)
        @?= ([4,5,6],(6,[]))
  , testCase "appendCNF" $
      execState (appendCNF [[1, 2, -3]]) emptyState
        @?= (0,[[1,2,-3]])
  , testCase "nested appendCNF" $
      execState (appendCNF [[-4, 5], [4]]) (execState (appendCNF [[1, 2, -3]]) emptyState)
        @?= (0,[[-4,5],[4],[1,2,-3]])
  , testCase "zeroOut" $
        execState (zeroOut [1, 2, 3]) emptyState
          @?= (0,[[-1],[-2],[-3]])
  , testCase "setToOne" $
        execState (setToOne 1) emptyState
          @?= (0,[[1]])
  , testCase "setToOne + zeroOut nested" $
        execState (zeroOut [1, 2, 3]) (execState (setToOne 4) emptyState)
          @?= (0,[[-1],[-2],[-3],[4]])
  , testCase "setToZero" $
        execState (setToZero 1) emptyState
          @?= (0,[[-1]])
  ]

---------------------------------------------------------------------------------------------------------------
-- ServerHelpers Tests

strToIntTests = testGroup "strToInt tests"
  [ testCase "should convert to int correctly" $
    strToInt "42" @?= 42
  ]

addClauseToHeaderTests = testGroup "addClauseToHeader tests"
  [ testCase "should add 1 to the clause count" $
    addClauseToHeader "p cnf 3 5" @?= "p cnf 3 6"
  ]

updateHeaderTests = testGroup "updateHeader tests"
  [ testCase "should update the clause count" $
    updateHeader 5 "p cnf 3 5" @?= "p cnf 3 10"
  ]

cnfsToLinesTests = testGroup "cnfsToLines tests"
  [ testCase "should convert a single CNF to a string" $
    cnfToLine [3, 4, 7] @?= "3 4 7 0"
  , testCase "should convert multiple CNFs to a list of strings" $
    cnfsToLines [[3, 4, 7], [1, 2, 3], [4, 5, 6]] @?= ["3 4 7 0", "1 2 3 0", "4 5 6 0"]
  ]

addClausesToCnfTests = testGroup "addClausesToCnf tests"
  [ testCase "should append the clauses and update the header" $
    let originalCnf = "p cnf 3 1\n" ++
                      "1 2 3 0\n" in
      let expected  = "p cnf 3 3\n" ++
                      "1 2 3 0\n" ++
                      "1 2 0\n" ++
                      "3 2 0" in
        addClausesToCnf originalCnf [[1, 2], [3, 2]] @?= expected
  ]

parseCMSatSolutionTests = testGroup "parseCMSatSolution tests"
  [ testCase "single line solution" $
    parseCMSatSolution "s SATISFIABLE\nv 1 2 -3 4 0" @?= [1, 2, -3, 4, 0]
  , testCase "two line solution" $
    parseCMSatSolution "s SATISFIABLE\nv 1 2 -3 4\nv -5 6 7 0\n" @?= [1, 2, -3, 4, -5, 6, 7, 0]

  , testCase "solution with extra whitespace" $
    let output = "stdout: s SATISFIABLE\n" ++
                 "v -1 -2 3 4 -5 \n" ++
                 "v -6 7 -8 9 \n" ++
                 "v 10 11 0  " in
      parseCMSatSolution output @?= [-1, -2, 3, 4, -5, -6, 7, -8, 9, 10, 11, 0]

  , testCase "unsatisfiable formula should return empty list" $
    let output = "c Independent vars set: 1,2,3,4,5\n" ++
                 "s UNSATISFIABLE\n" in
      parseCMSatSolution output @?= []
  ]

isFormulaSatTests = testGroup "isFormulaSat tests"
  [ testCase "should return true if SAT" $
    isFormulaSat "s SATISFIABLE\nv 1 2 3 0" @?= True
  , testCase "should return false if UNSAT" $
    isFormulaSat "s UNSATISFIABLE\n" @?= False
  ]

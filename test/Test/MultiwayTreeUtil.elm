module Test.MultiwayTreeUtil (testSuite) where

import ElmTest            exposing (..)
import Test.SampleData    exposing (..)
import MultiwayTreeZipper exposing (..)
import MultiwayTreeUtil   exposing (..)

(&>) : Maybe a -> (a -> Maybe b) -> Maybe b
(&>) = Maybe.andThen

isJust : Maybe a -> Bool
isJust mb = case mb of
  Just _  -> True
  Nothing -> False

testSuite : Test
testSuite = suite "matcher"
  [   testFlatten
  ,   testTraverse
  ,   testTraverseFrom
  ,   testForestLookup
  ,   testTreeLookup
  ,   testLca
  ]

testFlatten : Test
testFlatten = suite "flatten" [
      test "proper length"  <| flip assertEqual (List.length <| flatten sampleTree) 11
    , test "proper content" <| flip assertEqual (flatten sampleTree) ["a","b","e","k","c","f","g","d","h","i","j"]
    ]

testTraverse : Test
testTraverse =
  let
    zipper1 = goToChild 0 (sampleTree, []) &> goToChild 0 &> goToChild 0
    zipper2 = goToChild 2 (sampleTree, []) &> goToChild 2
    zipper3 = treeLookup "j" sampleTree
  in suite "traverse" [
    test "single node"     <| flip assertEqual (traverse (sampleTree, []))               ["a"]
  , test "zipper length"   <| flip assertEqual (Maybe.map (traverse >> List.length) zipper1)  (Just 4)
  , test "zipper2 length"  <| flip assertEqual (Maybe.map (traverse >> List.length) zipper2)  (Just 3)
  , test "zipper"          <| flip assertEqual (Maybe.map traverse zipper1)                   (Just ["a", "b", "e", "k"])
  , test "zipper2"         <| flip assertEqual (Maybe.map traverse zipper2)                   (Just ["a", "d", "j"])
  , test "zipper3"         <| flip assertEqual (Maybe.map traverse zipper2)                   (Maybe.map traverse zipper3)
  ]

testTraverseFrom : Test
testTraverseFrom =
  let
    zipper1 = goToChild 0 (sampleTree, []) &> goToChild 0 &> goToChild 0
    zipper2 = goToChild 2 (sampleTree, []) &> goToChild 2
    zipper3 = treeLookup "j" sampleTree
  in suite "traverseFrom" [
    test "single node"     <| flip assertEqual (traverseFrom Nothing (sampleTree, []))        <| traverse (sampleTree, [])
  , test "zipper"          <| flip assertEqual (Maybe.map (traverseFrom Nothing) zipper1)     <| Maybe.map traverse zipper1
  , test "zipper2"         <| flip assertEqual (Maybe.map (traverseFrom Nothing) zipper2)     <| Maybe.map traverse zipper2
  , test "zipper3"         <| flip assertEqual (Maybe.map (traverseFrom Nothing) zipper2)     <| Maybe.map traverse zipper3
  , test "zipper from a"   <| flip assertEqual (Maybe.map (traverseFrom <| Just "a") zipper1) <| Just ["b", "e", "k"]
  , test "zipper from b"   <| flip assertEqual (Maybe.map (traverseFrom <| Just "b") zipper1) <| Just ["e", "k"]
  , test "zipper from e"   <| flip assertEqual (Maybe.map (traverseFrom <| Just "e") zipper1) <| Just ["k"]
  , test "zipper from k"   <| flip assertEqual (Maybe.map (traverseFrom <| Just "k") zipper1) <| Just []
  , test "zipper2 from a"  <| flip assertEqual (Maybe.map (traverseFrom <| Just "a") zipper2) <| Just ["d", "j"]
  , test "zipper2 from d"  <| flip assertEqual (Maybe.map (traverseFrom <| Just "d") zipper2) <| Maybe.map (traverseFrom <| Just "d") zipper3
  ]

testTreeLookup : Test
testTreeLookup = suite "treeLookup" [
    test "finds valid node" <| flip assertEqual (isJust <| treeLookup "j" sampleTree)            <| True
  , test "finds Nothing"    <| flip assertEqual (isJust <| treeLookup "z" sampleTree)            <| False
  , test "finds j"          <| flip assertEqual (Maybe.map datum <| treeLookup "j" sampleTree)   <| Just "j"
  , test "finds a"          <| flip assertEqual (Maybe.map datum <| treeLookup "a" sampleTree)   <| Just "a"
  , test "finds zipper"     <| flip assertEqual ((treeLookup "d" sampleTree) &> goToRoot)        <| Just (sampleTree, [])
  ]

testForestLookup : Test
testForestLookup = suite "forestLookup" [
    test "finds j"    <| flip assertEqual (isJust <| forestLookup "j" sampleForest)            <| True
  , test "finds z"    <| flip assertEqual (isJust <| forestLookup "z" sampleForest)            <| True
  , test "!finds t"   <| flip assertEqual (isJust <| forestLookup "t" sampleForest)            <| False
  , test "same tree"  <| flip assertEqual ((forestLookup "j" sampleForest) &> goToRoot)        <| (forestLookup "k" sampleForest) &> goToRoot
  , test "finds a"    <| flip assertNotEqual ((forestLookup "z" sampleForest) &> goToRoot)     <| (forestLookup "j" sampleForest) &> goToRoot
  ]

testLca : Test
testLca = suite "lca" [
    test "finds j"      <| flip assertEqual (lca "j" "j" sampleForest)                          <| Just "j"
  , test "finds k"      <| flip assertEqual (lca "d" "j" sampleForest)                          <| Just "d"
  , test "lca1"         <| flip assertEqual (lca "j" "k" sampleForest)                          <| Just "a"
  , test "lca2"         <| flip assertEqual (lca "x" "z" sampleForest)                          <| Just "w"
  , test "lca3"         <| flip assertEqual (lca "r" "q" sampleForest)                          <| Just "l"
  , test "associative"  <| flip assertEqual (lca "a" "k" sampleForest)                          <| lca "k" "a" sampleForest
  , test "nothing"      <| flip assertEqual (lca "k" "z" sampleForest)                          <| Nothing
  ]

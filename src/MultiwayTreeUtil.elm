module MultiwayTreeUtil (flatten, traverse, traverseFrom, forestLookup, treeLookup, lca) where

{-| A set of utility function to work with multiway tree

# Traverse
@docs flatten, traverse, traverseFrom

# Lookup
@docs forestLookup, treeLookup, lca
-}

import Maybe
import MultiwayTree as Tree exposing (Tree (..), Forest)
import MultiwayTreeZipper exposing (Zipper, Context (..), goToChild, goUp, datum)

{-| Private -}
createZipper : Tree a -> Zipper a
createZipper tree = (tree, [])

{-| Private -}
unsafeFromJust : Maybe a -> a
unsafeFromJust mb = case mb of
  Just v -> v
  Nothing -> Debug.crash "unsafeFromJust"

{-| Flattens the tree resulting with list of all node values
-}
flatten : Tree a -> List a
flatten tree =
  let tail = List.concatMap flatten (Tree.children tree)
  in (Tree.datum tree) :: tail

{-| Performs a traverse of zipper resulting with a list of node values
-}
traverse : Zipper a -> List a
traverse zipper = traverseFrom Nothing zipper

{-| Performs a traverse of zipper starting from given node.
-}
traverseFrom : Maybe a -> Zipper a -> List a
traverseFrom value zipper =
  let value' = datum zipper
  in case value == Just value' of
      True  -> []
      False -> case goUp zipper of
        Nothing      -> [value']
        Just zipper' -> traverseFrom value zipper' ++ [value']

{-| Lookup for specified node in a disjoint tree set
-}
forestLookup : a -> Forest a -> Maybe (Zipper a)
forestLookup value forest = List.head
  <| flip List.filterMap forest
  <| \tree -> treeLookup value tree

{-| Lookup for specified node in a tree
-}
treeLookup : a -> Tree a -> Maybe (Zipper a)
treeLookup value tree =
  let zipperLookup node z = case datum z == node of
    True  -> Just z
    False -> case Tree.children (fst z) of
      []     -> Nothing
      childs -> List.head
        <| flip List.filterMap [0 .. List.length childs - 1]
        <| \idx -> zipperLookup node (unsafeFromJust <| flip goToChild z idx)
  in zipperLookup value (createZipper tree)

{-| Finds the lowest common ancestor in a disjoint tree set
-}
lca : a -> a -> Forest a -> Maybe a
lca a b forest =
  let
    zipperA = List.head <| List.filterMap (treeLookup a) forest
    zipperB = List.head <| List.filterMap (treeLookup b) forest

    traverseA = Maybe.withDefault [] <| Maybe.map traverse zipperA
    traverseB = Maybe.withDefault [] <| Maybe.map traverse zipperB

    result = case traverseA of
      [] -> Nothing
      _  -> case List.head traverseA == List.head traverseB of
        False -> Nothing
        True  -> List.foldl (\(a,b) acc -> if a == b then Just a else acc)
          Nothing
          <| List.map2 (,) traverseA traverseB

  in result

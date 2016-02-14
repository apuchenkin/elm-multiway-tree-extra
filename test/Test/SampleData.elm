module Test.SampleData where

import MultiwayTree exposing (Tree (..), Forest)

sampleTree : Tree String
sampleTree =
    Tree "a"
        [ Tree "b"
            [ Tree "e"
                [ Tree "k" [] ]
            ]
        , Tree "c"
            [ Tree "f" []
            , Tree "g" []
            ]
        , Tree "d"
            [ Tree "h" []
            , Tree "i" []
            , Tree "j" []
            ]
        ]

sampleTree2 : Tree String
sampleTree2 =
  Tree "l"
   [ Tree "m"
      [ Tree "o"
          [ Tree "r" [] ]
      ]
   , Tree "n"
      [ Tree "p" []
      , Tree "q" []
      ]
   ]

sampleTree3 : Tree String
sampleTree3 =
  Tree "w" [
      Tree "x" []
    , Tree "y" []
    , Tree "z" []
  ]

sampleForest : Forest String
sampleForest = [
    sampleTree,
    sampleTree2,
    sampleTree3
  ]

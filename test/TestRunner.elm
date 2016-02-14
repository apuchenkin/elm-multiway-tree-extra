module Main where

import ElmTest exposing (consoleRunner)
import Console exposing (IO, run)
import Task exposing (Task)
import Signal exposing (Signal)

import Test.MultiwayTreeUtil exposing (testSuite)

console : IO ()
console = consoleRunner testSuite

port runner : Signal (Task x ())
port runner = run console

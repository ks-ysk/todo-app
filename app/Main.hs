module Main where

import Item (interactWithUser)

main :: IO ()
main = do
  putStrLn "ToDo App"
  let initList = []
  interactWithUser initList
  putStrLn "Thank you for using"

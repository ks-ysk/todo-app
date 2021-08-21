module Item
  ( interactWithUser,
  )
where

type Item = String

type Items = [Item]

data Command
  = Exit
  | Display
  | Add String
  | Finish Int

interactWithUser :: Items -> IO ()
interactWithUser items = do
  putStrLn "Commands: exit, items, add - {item}, fisnish - {index}"
  inputLine <- getLine
  case parseCommand inputLine of
    Right Exit -> do
      putStrLn "See You!"
    Right Display -> do
      putStrLn "Now Your List is :"
      putStrLn (displayItems items)
      interactWithUser items
    Right (Add item) -> do
      let newItems = addItem item items
      putStrLn "added successfully!"
      interactWithUser newItems
    Right (Finish index) -> do
      let result = removeItem index items
      case result of
        Right newItems -> do
          putStrLn "Finished"
          interactWithUser newItems
        Left errorMassage -> do
          putStrLn ("Error: " ++ errorMassage)
          interactWithUser items
    Left errorMassage -> do
      putStrLn ("Error: " ++ errorMassage)
      interactWithUser items

parseCommand :: String -> Either String Command
parseCommand inputLine = case words inputLine of
  ["exit"] -> Right Exit
  ["items"] -> Right Display
  "add" : "-" : item -> Right (Add $ unwords item)
  ["finish", i] -> Right (Finish $ read i)
  _ -> Left "command : Unknown"

addItem :: Item -> Items -> Items
addItem item items = item : items

displayItems :: Items -> String
displayItems items =
  unlines displayItemsList
  where
    displayItemsList = zipWith displayItem [1 ..] reversedList
    displayItem index item = show index ++ " - " ++ item
    reversedList = reverse items

removeItem :: Int -> Items -> Either String Items
removeItem index items =
  remove (length items - index) items
  where
    remove n items =
      case (n, items) of
        (0, item : rest) ->
          Right rest
        (k, []) ->
          Left "Out range of Index"
        (k, item : rest) ->
          case remove (k - 1) rest of
            Right newItems ->
              Right (item : newItems)
            Left errorMessage ->
              Left errorMessage

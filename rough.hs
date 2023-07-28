
data Brightness
  = Dark
  | Bright

data EightColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

data AnsiColor
  = AnsiColor Brightness EightColor

isBright :: AnsiColor -> Bool
isBright color =
  case color of
    AnsiColor Bright _ -> True
    _ -> False

isEmpty :: [a] -> Bool
isEmpty list =
  case  listToMaybe list of
    Nothing -> False
    Just _ -> True

isEmptyV2 :: [a] -> Bool
isEmptyV2 list =
  case list of
    [] -> True
    _ : _ -> False

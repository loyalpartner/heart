module Heart
  ( pretty
  , heart
  ) where

data Doc
  = Empty
  | Space Int
  | Line
  | Symbol Int
  | Concat Doc Doc
  deriving (Show, Eq)

(<<) :: Doc -> Doc -> Doc
Empty << y = y
x << Empty = x
x << y = x `Concat` y

pretty :: Doc -> String
pretty c = better [c]
  where
    better [] = ""
    better (x:xs) =
      case x of
        Empty -> ""
        Space n -> replicate n ' ' ++ better xs
        Symbol n -> replicate n '*' ++ better xs
        Line -> '\n' : better xs
        a `Concat` b -> better (a : b : xs)

doubleDoc :: Doc -> Doc
doubleDoc doc = doc << doc

heart :: Int -> Doc
heart n = Empty << header n << body n << footer n

{-| 
  - 获取图形的宽度
-}
getWidth :: Num a => a -> a
getWidth n = 10 * n - 21

{-| 
  - 构造头部
  - 左半部分空格计算:
  -   (c - 2 - n) * 2 + 1
  - =  2c - 2n -3
  - 左半部分符号计算:
  -   c + (n - 1) * 4 
  - = c + 4n - 4
  - 尾部空格符号计算
  -   (c - n - 1) * 2 - 2
  - = 2c - 2n - 4
-}
header :: Int -> Doc
header c = render Empty 1
  where
    render doc n
      | n > total_line = doc
      | otherwise = render (doc << lineDoc n << Line) (n + 1)
    total_line = c - 2
    leftSpaces n = Space (2 * c - 2 * n - 3)
    spaceTail n = Space (2 * c - 2 * n -4)
    symbol n = Symbol (c + 4 * n - 4)
    lineDoc n = doubleDoc $ leftSpaces n << symbol n << spaceTail n

body :: Int -> Doc
body c = render Empty 1
  where
    render doc n
      | n > total_line = doc
      | otherwise = render (doc << symbol << Line) (n + 1)
    symbol = Symbol $ getWidth c
    total_line = c - 2

{-| 
  - 构造尾部
  - 
-}
footer :: Int -> Doc
footer c = render Empty 1
  where
    render doc n
      | n > total_line = doc
      | otherwise = render (doc << space n << symbol n << Line) (n + 1)
    space n 
      | n < total_line = Space (1 + (n - 1) * 2)
      | even c = Space 1 << Space (1 + (n - 2) * 2 + 1)
      | otherwise = Space (1 + (n - 2) * 2 + 1)
    symbol n 
      | n < total_line = Symbol (width - 2 - (n - 1) * 4)
      | otherwise = Symbol ((total_line - n) * 2 + 1)
    width = getWidth c
    total_line = footerHeight c

footerHeight c = step (getWidth c - 2) 1
  where
    step w acc
      | w <= 5 = acc + 1
      | w > 3 = step (w - 4) (acc + 1)

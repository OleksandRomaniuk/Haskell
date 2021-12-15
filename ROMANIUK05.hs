{-# OPTIONS_GHC -Wall #-}
module ROMANIUK05 where

import Data.Char(isSpace, isDigit, isLetter) 

type Name       = String
type Attributes = [(String, String)]
data XML        =  Text String | Element Name Attributes [XML] deriving (Eq, Show)

-- Задача 1 -----------------------------------------
spaces :: String -> String
spaces [] = []
spaces (x:xs) | isSpace x = spaces xs
              | otherwise = x:xs 
  
  
-- Задача 2.a ----------------------------------------- 
manyT :: String -> (String,String)
manyT [] = ([],[])
manyT (x:xs) | elem x ['<', '>'] = ([], (x:xs))
             | otherwise = (x:(fst next), (snd next))
 where next = manyT xs

-- Задача 2.b ----------------------------------------- 
value :: String -> (String,String)
value [] = ([],[])
value (x:xs) | elem x ['"'] = ([], (x:xs))
             | otherwise = (x:(fst next), (snd next))
 where next = value xs

-- Задача 2.c ----------------------------------------- 
manyN :: String -> (String,String)
manyN [] = ([],[])
manyN (x:xs) | (isLetter x || isDigit x || elem x ['.', '-']) = (x:(fst next), (snd next))
             | otherwise = ([], (x:xs))
 where next = manyN xs

-- Задача 3.a -----------------------------------------
name :: String ->  Maybe(String,String) 
name [] = Nothing
name (x:xs) | isLetter x = Just(x:fst(manyN xs), snd(manyN xs))
            | otherwise = Nothing

-- Задача 3.b -----------------------------------------
text :: String ->  Maybe(String,String) 
text [] = Nothing
text xs | null (fst(manyT xs)) = Nothing
        | otherwise = Just(fst(manyT xs), snd(manyT xs))

-- Задача 3.c -----------------------------------------
fullValue :: String ->  Maybe(String,String) 
fullValue [] = Nothing
fullValue (x:xs) | x=='"' && not(null(snd(value xs))) && head(snd(value xs))=='"' 
 = Just(fst(value xs), tail(snd(value xs)))
                 | otherwise = Nothing

-- Задача 4.a -----------------------------------------
attrib :: String -> Maybe ((String,String),String) 
attrib str =
  case name (spaces str) of
    Just (attrName, rest) ->
      case value rest of
        (eq, other) ->
          case spaces eq of
            ('=':_) ->
              case fullValue other of
                Just (attrVal, remains) -> Just ((attrName, attrVal), remains)
                _ -> Nothing
            _ -> Nothing
    _ -> Nothing

-- Задача 4.b -----------------------------------------
manyAtt :: String -> Maybe (Attributes,String) 
manyAtt string = manyAttHelper [] string

manyAttHelper :: Attributes -> String -> Maybe (Attributes, String)
manyAttHelper attrs string =
  case attrib string of
    Just (attr, rest) -> manyAttHelper (attrs ++ [attr]) rest
    _ -> Just (attrs, string)

-- Задача 5.a -----------------------------------------
begTag :: String -> Maybe ((String,Attributes),String)
begTag [] = Nothing
begTag s 
  | (head s) /= '<' = Nothing
  | otherwise = case name (tail s) of
                Nothing -> Nothing
                Just (nm, rest) -> case manyAtt rest of
                                    Just (attr, ('>':leftover)) -> Just ((nm, attr), leftover)
                                    _ -> Nothing


-- Задача 5.b -----------------------------------------
endTag :: String -> Maybe (String,String) 
endTag s = if isEndTag s then
  case name (drop 2 s) of
    Just (nm, rest) -> Just (nm, (tail rest))
    _ -> Nothing
  else Nothing

isEndTag :: String -> Bool
isEndTag [_] = False
isEndTag [] = False
isEndTag (x:y:zs) = if x == '<' && y == '/' && '>' `elem` zs then True else False

-- Задача 6.a -----------------------------------------
element :: String -> Maybe (XML,String) 
element s = case begTag s of
            Just ((nm, attr), other) -> case manyXML other of
                Just (someXML, lef) -> case endTag lef of
                    Just(end, st) -> if nm == end then Just ((Element nm attr someXML), st) else Nothing
                    _ -> Nothing
                _ -> Nothing
            _ -> Nothing

-- Задача 6.b -----------------------------------------
xml :: String -> Maybe (XML,String)
xml st = case element st of
         Just (xs, rst1) -> Just (xs, rst1)
         Nothing -> case text st of
                    Just (tx, rst2) -> Just (Text tx, rst2)
                    _ -> Nothing 

-- Задача 6.c -----------------------------------------
manyXML :: String -> Maybe ([XML],String)
manyXML s | null xmls = 
  if endTag st == Nothing then Nothing else Just (xmls, st)           
    | otherwise = case last xmls of
                  Text _  ->  if endTag st == Nothing then Nothing else Just (xmls, st)
                  _  -> Just (xmls, st)
    where (xmls, st) = fxml s

fxml :: String -> ([XML], String)
fxml s = case xml s of
            Just (x, lefted) -> (x:xmls, other)
             where (xmls, other) = fxml lefted
            _ -> ([], s)

-- Задача 7 -----------------------------------------
fullXML :: String -> Maybe XML 
fullXML s = case element (spaces s) of  
            Just (xm,s1) -> if null (spaces s1) then Just xm else Nothing 
            Nothing -> Nothing  

-- Тестові дані -------------------------------------------
-- Прості тести XML-об'єктів (без проміжків)
stst1, stst2, stst3 :: String
stst1 = "<a>A</a>"
stst2 = "<a x=\"1\"><b>A</b><b>B</b></a>"
stst3 = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>" 

-- Результати аналізу попередніх XML-об'єктів
x1, x2, x3 :: XML
x1 = Element "a" [] [Text "A"]
x2 = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3 = Element "a" 
            [] 
            [Element "b" 
                     [] 
                     [Element "c"
                              [("att","att1")] 
                              [Text "text1"],
                      Element "c" 
                              [("att","att2")]
                              [Text "text2"]],
             Element "b" 
                     [] 
                     [Element "c" 
                              [("att","att3")] 
                              [Text "text3"],
                      Element "d" 
                              [] 
                              [Text "text4"]]]

casablanca :: String 
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML 
casablancaParsed
  = Element "film" 
            [("title","Casablanca")] 
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]




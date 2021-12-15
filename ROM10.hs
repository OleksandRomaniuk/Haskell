{-# OPTIONS_GHC -Wall #-}
module ROMANIUK10 where
import Data.Maybe
import Data.Char
import Data.List
import Data.Char(isDigit, digitToInt, intToDigit)
import Data.Maybe(isJust, fromJust)
import Control.Monad (void)
import Text.ParserCombinators.Parsec

data Value = I Int  | B Bool deriving (Show, Eq)
data Exp = Var String      -- Змінна
         | Const Value     -- константа
         | Op Exp Bop Exp  -- Операція
                 deriving (Show, Eq)
-- Бінарні (2-аргумента) оператори
data Bop =  Plus | Minus | Times | Div   
          | Gt | Ge | Lt | Le| Eql | And | Or
            deriving (Show, Eq)

data Stmt = Assign String Exp
          | Read String 
          | Write Exp
          | Incr String
          | If Exp Stmt 
          | While Exp Stmt       
          | For Stmt Exp Stmt Stmt
          | Block [(String,Type)] [Stmt]        
          deriving (Show, Eq)
data Type = It | Bt deriving (Show, Eq)
type Program = Stmt

type StateW = ([String], [(String,Value)], [String])

type VarEnv  = [(String,Type)]

-- Задача 1.a -----------------------------------------
getValue::  StateW -> String -> Value
getValue st iD = fromJust (lookup iD (snd3 st))
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x
thr3 :: (a, b, c) -> c
thr3 (_, _, x) = x


-- Задача 1.b -----------------------------------------
updValue :: StateW -> String -> Value -> StateW
updValue (a,b,c) name val = (a, updated, c)
  where updated = map (\x -> if fst x == name then (fst x,val) else x) b


-- Задача 2 ----------------------------------------- 
readValue :: StateW -> Type -> (StateW,Value)
readValue st It | null (fstOf3 st) = (st, I 0)
                | isNumber (strR st) = (helperRead st, I (read (strR st)))
                | otherwise = (st, I 0)
                 where isNumber [] = True
                       isNumber str = ((head str == '-') || isDigit (head str)) && isNumber (tail str)
readValue st Bt | null (fstOf3 st) = (st, B False)
                | strR st == "False" || strR st == "True" = (helperRead st, r)
                | otherwise = (st, B False)
                  where  r = if strR st == "True" then B True else B False

helperRead :: ([a], b, c) -> ([a], b, c)
helperRead st = (tail (fstOf3 st), sndOf3 st, thrOf3 st)
strR :: ([a], b, c) -> a
strR st = head (fstOf3 st)

fstOf3 :: (a, b, c) -> a
fstOf3 (x, _, _) = x

sndOf3 :: (a, b, c) -> b
sndOf3 (_, b, _) = b

thrOf3 :: (a, b, c) -> c
thrOf3 (_, _, c) = c

-- Задача 3 -----------------------------------------
writeValue :: StateW -> Value -> StateW 
writeValue (inp,ss,out) (I v) = (inp,ss,out++[toString v])
writeValue (inp,ss,out) (B True) = (inp,ss,out++["True"])
writeValue (inp,ss,out) (B False) = (inp,ss,out++["False"])


toString :: Int -> String
toString 0 = ['0']
toString x | x < 0 = '-':toString(-1*x)
           | div x 10 > 0 = (toString$div x 10)++[intToDigit(mod x 10)]
           | otherwise = [intToDigit(mod x 10)]
  
-- Задача 4.a ----------------------------------------- 
evExp :: StateW -> Exp -> Value
evExp _ (Const v) = v
evExp st (Var s) | null s = (B False)
                 | otherwise = getValue st s
evExp st (Op a Plus b) = intOp (evExp st a) (evExp st b) (+)
evExp st (Op e1 Minus e2) = intOp (evExp st e1) (evExp st e2) (-)
evExp st (Op e1 Times e2) = intOp (evExp st e1) (evExp st e2) (*)
evExp st (Op e1 Div e2) = intOp (evExp st e1) (evExp st e2) (div)
evExp st (Op e1 Gt e2) = ibOp (evExp st e1) (evExp st e2) (>)
evExp st (Op e1 Ge e2) = ibOp (evExp st e1) (evExp st e2) (>=)
evExp st (Op e1 Lt e2) = ibOp (evExp st e1) (evExp st e2) (<)
evExp st (Op e1 Le e2) = ibOp (evExp st e1) (evExp st e2) (<=)
evExp st (Op e1 Eql e2) = ibOp (evExp st e1) (evExp st e2) (==)
evExp st (Op e1 And e2) = boolOp (evExp st e1) (evExp st e2) (&&)
evExp st (Op e1 Or e2) = boolOp (evExp st e1) (evExp st e2) (||)

intOp :: Value -> Value -> (Int->Int->Int) -> Value
intOp (I v1) (I v2) f = (I (f v1 v2))
intOp _ _ _ = error "parameters must be integer"

ibOp :: Value -> Value -> (Int->Int->Bool) -> Value
ibOp (I v1) (I v2) f = (B (f v1 v2))
ibOp _ _ _ = error "parameters must be integer"

boolOp :: Value -> Value -> (Bool->Bool->Bool) -> Value
boolOp (B v1) (B v2) f = (B (f v1 v2))
boolOp _ _ _ = error "parameters must be boolean"

-- Задача 4.b -----------------------------------------
evStmt :: StateW -> Stmt -> StateW 
evStmt st (Assign s e) = updValue st s (evExp st e)
evStmt st (Write e) = writeValue st (evExp st e)
evStmt st (Read s) = fst $ readValue (updValue st s (snd $ readValue st (stringToType s))) (stringToType s)
evStmt st (Incr s) = updValue st s $ I $ val + 1 where (I val) = getValue st s
evStmt st (If e stm) = if bol then evStmt st stm else st
    where (B bol) = evExp st e
evStmt st (While e stm)
  | (evExp st e == B True) = evStmt st (While e stm)
  | otherwise = st
evStmt st (For stm1 e stm2 stm3)
  | evExp (evStmt st stm1) e == B True = evStmt newSt (For asStm e stm2 stm3)
  | otherwise = st where
    asStm = (Assign "" (Var ""))
    newSt = evStmt (evStmt (evStmt st stm1) stm2) stm3
evStmt st (Block blocks stmts) = foldl (evStmt) first stmts
    where first = ((fst3 st),[(i, val) | (i, typ) <- blocks, 
                  let val = if typ == Bt then (B False) else (I 0)] ++ (snd3 st), (thr3 st))

stringToType :: String -> Type
stringToType s 
  | s == "True" && s == "False" = Bt
  | otherwise = It

-- Задача 4.c -----------------------------------------
evProgram :: Program -> [String] -> [String]
evProgram pr inp = getOut$evStmt (inp,[],[]) pr
    where getOut (_,_,out) = out

---- Перевірка контекстних умов -----------------------
-- Задача 5.a -----------------------------------------
iswfOp :: Bop -> [Type] -> Maybe Type 
iswfOp b [Bt, Bt]| b == And || b == Or = Just Bt
iswfOp b [It, It] | b == Eql ||  b == Gt || b == Ge || b == Le ||  b == Plus || b == Lt ||  b == Times ||b == Minus || b == Div= Just Bt
iswfOp _ _ = Nothing

-- Задача 5.b -----------------------------------------
iswfExp :: Exp -> VarEnv -> Maybe Type
iswfExp (Var name) vars = lookup name vars
iswfExp (Const val) _
  | val == B False || val == B True = Just Bt
  | otherwise = Just It
iswfExp (Op exp1 binary exp2) vars =
  iswfOp binary [fromJust (iswfExp exp1 vars), fromJust (iswfExp exp2 vars)]


-- Задача 5.c -----------------------------------------
iswfStmt :: Stmt -> VarEnv -> Bool 
iswfStmt (Assign _ ex) ve = case iswfExp ex ve of Just It -> True
                                                  _       -> False
iswfStmt (Read name) ve = containsVar ve name
iswfStmt (Write ex) ve = case iswfExp ex ve of Just It -> True
                                               _       -> False
iswfStmt (Incr name) ve | containsVar ve name  = case iswfExp (Var name) ve of Just It -> True
                                                                               _       -> False
                        | otherwise = False
iswfStmt (If ex stmt) ve | iswfExp ex ve == Just Bt && iswfStmt stmt ve = True
                         | otherwise = False
iswfStmt (While ex stmt) ve | iswfExp ex ve == Just Bt && iswfStmt stmt ve = True
                            | otherwise = False
iswfStmt (For stmt1 ex stmt2 stmt3) ve | iswfStmt stmt1 ve && iswfExp ex ve == Just Bt && 
                                         iswfStmt stmt2 ve && iswfStmt stmt3 ve = True
                                       | otherwise = False
iswfStmt (Block ves stmts) ve = and$[iswfStmt s (ve++ves) | s <- stmts]



containsVar :: VarEnv -> String -> Bool 
containsVar ve name = not$null$filter (\x -> fst x == name) ve


---- Синтаксичний аналіз -------
iden :: Parser String
iden = try( do {nm <- identif;
                if (any(nm==) ["int","bool","read","write","if","while","for","true","false"])
                    then unexpected ("reserved word " ++ show nm)
                    else return nm 
               } ) 

oper  :: String -> Bop -> Parser Bop
oper str bop = do {_ <- string str; return bop}

mulOp :: Parser Bop   
mulOp = (oper "*" Times) <|> (oper "/" Div)

disOp :: Parser Bop   
disOp = (oper "&" And)

conOp :: Parser Bop   
conOp = (oper "|" Or)

--розпізнавати ВСІ порожні символи в кінці
lexem :: Parser a -> Parser a
lexem p = do {a <- p; spaces; return a}

--   :type Op -----> Exp -> Bop -> Exp -> Exp 
--   :type flip Op -------> Bop -> Exp -> Exp -> Exp         
expOp :: Parser Bop -> Parser (Exp -> Exp -> Exp)
expOp p = do {x <- lexem p; return (flip Op x)}

symbol :: Char ->  Parser ()
symbol ch = lexem (char ch >> return ())

keyword :: String -> Parser ()
keyword st = try( lexem( string st >> notFollowedBy alphaNum)) 

typev :: Parser Type 
typev = do {keyword "int"; return It}
        <|> do {keyword "bool"; return Bt} 

-- Задача 6.a -----------------------------------------
identif :: Parser String
identif = do {l <- letter; 
              ls <- many (digit <|> letter); 
              return (l:ls)}

number :: Parser Int
number = read <$> (many1 digit)
 
addOp :: Parser Bop  
addOp = oper "+" Plus <|> oper "-" Minus

relOp :: Parser Bop  
relOp = try (oper ">=" Ge) <|> (oper ">" Gt) <|> (oper "==" Eql)
    <|> try (oper "<=" Le) <|> (oper "<" Lt)



-------------------------------------------------------
factor :: Parser Exp
factor = do { symbol '('; x <- expr; symbol ')'; return x}
     <|> do {nm <- lexem number; return (Const (I nm))}
     <|> do {keyword "true"; return (Const (B True))}
     <|> do {keyword "false"; return (Const (B False))}
     <|> do {cs <- lexem iden; return (Var cs) }
     <?> "factor"

-- Задача 6.b -----------------------------------------
term :: Parser Exp     
term = chainl1 factor$expOp mulOp

relat :: Parser Exp
relat = chainl1 term$expOp addOp

conj :: Parser Exp
conj = chainl1 relat$expOp relOp

disj :: Parser Exp
disj = chainl1 conj$expOp conOp

expr :: Parser Exp
expr = chainl1 disj$expOp disOp

------------------------------------------------------
stmt :: Parser Stmt 
stmt = do {keyword "for"; forSt}
       <|> do {keyword "while"; whileSt}
       <|> do {keyword "if"; ifSt}
       <|> do {keyword "read"; inSt}
       <|> do {keyword "write"; outSt}
       <|> do {var <- lexem iden; assignSt var}
       <|> blockSt
       <?> "statement"

-- Задача 6.c -----------------------------------------
forSt :: Parser Stmt  
forSt = do
  symbol '('
  st <- lexem stmt
  symbol ';'
  se <- lexem expr
  symbol ';'
  sn <- lexem stmt
  symbol ')'
  ss <- lexem stmt
  return (For st se sn ss)

whileSt :: Parser Stmt               
whileSt = do
  symbol '('
  se <- lexem expr
  symbol ')'
  ss <-  lexem stmt
  return (While se ss)
              
ifSt :: Parser Stmt              
ifSt = do
  symbol '('
  se <- lexem expr
  symbol ')'
  ss <- lexem stmt
  return (If se ss)

inSt :: Parser Stmt              
inSt = do
  ss <- lexem iden
  return (Read ss)  

outSt :: Parser Stmt              
outSt = do
  se <- lexem expr
  return (Write se)

assignSt :: String -> Parser Stmt 
assignSt val = do {
  symbol ':';
  symbol '=';
  se <- lexem expr;
  return (Assign val se)
} <|> do {
  symbol '+';
  symbol '+';
  return (Incr val)
 }
                            
blockSt :: Parser Stmt
blockSt = do
  symbol '{'
  def <- many defin
  list <- listSt
  symbol '}'
  return (Block def list)
defin :: Parser (String, Type)
defin = do
  typ <- lexem typev
  ide <- lexem iden
  symbol ';'
  return (ide, typ)
listSt :: Parser [Stmt]
listSt = do
  ss <- lexem stmt
  sxs <- many (do
    symbol ';'
    lexem stmt)
  return (ss : sxs)

               
---------------------------------------------	
-- Головні функції
---------------------------------------------				
program :: Parser Stmt 
program = do {spaces; r <- stmt; eof; return r}

parseL :: String -> Either ParseError Program
parseL s = parse program "" s

-- Програми -------------------------------------------
squareRoot :: Program
squareRoot = Block [("a",It),("b",It)]
                   [ Read "a", Assign "b" (Const (I 0))
                   , If (Op (Var "a") Ge (Const(I 0)))
                        (Block [("c", Bt)] 
                               [Assign "c" (Const (B True)),
                                While (Var "c")
                                 (Block []
                                   [(Incr "b"), 
                                    Assign "c" (Op (Var "a") Ge (Op (Var "b") Times (Var "b")))
                                   ])
                               ]
                        )
                   , Write (Op (Var "b") Minus (Const (I 1)))
                   ]

squareRootS :: String
squareRootS =
   "{int a, b; \
   \ read a; b := 0; \
   \ if (a>= 0)\
   \    {bool c; c:=true; while(c) {b++; c:= a >= b*b}\
   \    };\
   \  write (b-1)\
   \ }"

fibonacci :: Program
fibonacci = 
    Block [("in",It), ("out",It)]
          [ Read "in",  Assign "out" (Const (I 0))
          , If (Op (Var "in") Ge (Const(I 0))) 
               (Block [("f0",It), ("f1",It), ("c",It)]
                     [Assign "f0" (Const (I 1)), Assign "f1" (Const (I 1)), Assign "out" (Const (I 1)),
                      If (Op (Var "in") Gt (Const (I 1)))
                         (For (Assign "c" (Const (I 1)))
                             (Op (Var "c") Lt (Var "in")) 
                             (Incr "c")
                             (Block []
                                    [Assign "out" (Op (Var "f0") Plus (Var "f1"))
                                    , Assign "f0" (Var "f1")
                                    , Assign "f1" (Var "out")
                                    ]
                              )
                         )
                     ])
          , Write (Var "out")
          ]

fibonacciS :: String
fibonacciS = 
 " {int in, out; read in; out := 0; \n\
   \if (in>=0){int f0, f1,c; \n\
   \           f0 := 1; f1 := 1; out := 1; \n\
   \           if(in>1) \n \
   \              for (c := 1; c < in; c++) {\n\
   \                   out := f0 + f1; f0 := f1; f1 := out\n\
   \              }\n\
   \          }; \n\
   \write out \n\
  \}"

> import Control.Applicative 
> import Data.Char 
> import System.IO 

> newtype Parser a = P { runParser :: String -> [(a, String)]} 

> item :: Parser Char 
> item = P (\inp -> case inp of 
>                      []     -> []
>                      (x:xs) -> [(x,xs)])

> instance Functor Parser where 
>    -- fmap :: (a->b) -> Parser a -> Parser b 
>    fmap g p = P (\inp -> case runParser p inp of 
>                             []        -> []
>                             [(y, ys)] -> [(g y, ys)] )

> instance Applicative Parser where 
>   -- pure :: a -> Parser a 
>   pure a = P (\inp -> [(a, inp)])
>   -- (<*>) :: Parser (a->b) -> Parser a  -> Parser b 
>   fx <*> px = P (\inp -> case runParser fx inp of 
>                            []      -> [] 
>                            [(g, ys)] -> runParser (fmap g px) ys)

> three :: Parser (Char, Char) 
> three = pure g <*> item <*> item <*> item 
>            where g x y z = (x,z)

> instance Monad Parser where 
>  -- return :: a -> Parser a 
>  return  = pure 
>  -- (>>=) :: Parser a -> Parser (a -> b) -> Parser b
>  px >>= gx  = P (\inp -> case runParser px inp of 
>                              []       -> [] 
>                              [(y,ys)] -> runParser (gx y) ys)


> neg2 :: Int -> Parser (Int -> Int)
> neg2 n = P (\inp -> case runParser (char '-') inp of 
>                      []        -> [] 
>                      [(x, ys)] -> [((*)n, ys)] ) 

> instance Alternative Parser where 
>   -- empty :: Parser a 
>   empty = P (\inp -> []) 
>   -- (<|>) :: Parser a -> Parser a -> Parser a 
>   p <|> q = P (\inp -> case runParser p inp of 
>                            []       -> runParser q inp 
>                            [(y,ys)] -> [(y,ys)])  

> sat :: (Char -> Bool) -> Parser Char 
> sat p = do x <- item 
>            if p x then return x else empty

> digit :: Parser Char 
> digit = sat isDigit 

> lower :: Parser Char 
> lower = sat isLower 

> upper :: Parser Char 
> upper = sat isUpper

> letter :: Parser Char 
> letter = sat isAlpha 

> alphanum :: Parser Char 
> alphanum = sat isAlphaNum

> char :: Char -> Parser Char 
> char x = sat (==x)

> string :: String -> Parser String 
> string []     = return [] 
> string (x:xs) = do char x 
>                    string xs 
>                    return (x:xs)

> manys :: Parser a -> Parser [a] 
> manys x = some x <|> pure [] 

> somes :: Parser a -> Parser [a] 
> somes x = pure (:) <*> x <*> manys x 

> ident :: Parser String 
> ident = do x  <- lower 
>            xs <- some alphanum
>            return (x:xs)

> nat :: Parser Int 
> nat = do xs <- some digit 
>          return (read xs)

> space :: Parser () 
> space = do many (sat isSpace)
>            return ()

> int :: Parser Int 
> int = nat <|> do char '-'
>                  n <- nat 
>                  return (-n)
>      

> int2 :: Parser Int 
> int2 = (char '-' *> fmap negate nat) <|> nat 

> int3 :: Parser Int 
> int3 = (neg2 (-1) <*> nat) <|> nat 

> int4 :: Parser Int 
> int4 = (char '-' >> fmap negate nat) <|> nat 

> token :: Parser a -> Parser a 
> token p = do space
>              v <- p 
>              space 
>              return v 

> identifier :: Parser String 
> identifier = token ident 

> natural :: Parser Int 
> natural = token nat  

> integer :: Parser Int 
> integer  = token int 

> symbol :: String -> Parser String 
> symbol xs = token (string xs)


> nats :: Parser [Int] 
> nats = do symbol "["
>           n  <- natural 
>           ns <- many (do symbol "," 
>                          natural)
>           symbol "]"
>           return (n:ns)

> expr :: Parser Int 
> expr = do t <- term 
>           do symbol "+" 
>              e <- expr 
>              return (t + e)
>            <|> return t 


> term :: Parser Int 
> term = do f <- factor 
>           do symbol "*"
>              t <- term
>              return (f * t)
>            <|> return f 

> factor :: Parser Int 
> factor = do symbol "("
>             e <- expr 
>             symbol "("
>             return e 
>          <|> natural


> eval :: String -> Int 
> eval xs = case runParser expr xs of 
>              [(n, [])]  -> n 
>              [(_, out)] -> error ("Unused input" ++ out)  
>              []         -> error "Invalid input"


> cls :: IO ()
> cls = putStr "\ESC[2J"

> type Pos = (Int,Int) 

> writeat :: Pos -> String -> IO () 
> writeat p xs = do goto p 
>                   putStr xs 

> goto :: Pos -> IO () 
> goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

> getCh :: IO Char 
> getCh = do hSetEcho stdin False 
>            x <- getChar 
>            hSetEcho stdin True 
>            return x 

> box :: [String]
> box = ["+---------------",
>        "|              |",
>        "+---+---+---+---+",
>        "| q | c | d | = |",
>        "+---+---+---+---+",
>        "| 1 | 2 | 3 | + |",
>        "+---+---+---+---+",
>        "| 4 | 5 | 6 | - |",
>        "+---+---+---+---+",
>        "| 7 | 8 | 9 | * |",
>        "+---+---+---+---+",
>        "| 0 | ( | ) | / |",
>        "+---+---+---+---+"]

> buttons :: String 
> buttons = standard ++ extra 
>            where 
>                standard = "qcd=123+456-789*0()/"
>                extra    = "QCD \ESC\BS\DEL\n"


> showbox :: IO ()
> showbox = sequence_ [writeat (1,y) b | (y,b) <- zip [1..] box] 

> display :: String -> IO ()
> display xs = do writeat (3,2) (replicate 13 ' ')
>                 writeat (3,2) (reverse (take 13 (reverse xs)))

> calc :: String -> IO () 
> calc xs = do display xs 
>              c <- getCh 
>              if elem c buttons then process c xs 
>              else 
>                   do beep 
>                      calc xs

> process :: Char -> String -> IO () 
> process c xs | elem c "qQ\ESC"    = quit 
>              | elem c "dD\BS\DEL" = delete xs 
>              | elem c "=\n"       = eval1 xs 
>              | elem c "cC"        = clear 
>              | otherwise          = press c xs 


> quit :: IO ()
> quit = goto (1,14)

> delete :: String -> IO () 
> delete [] = calc [] 
> delete xs = calc (init xs) 

> eval1 :: String -> IO () 
> eval1 xs = case runParser expr xs of 
>              [(n,[])] -> calc (show n)
>              _        ->  do beep 
>                              calc xs

> beep :: IO () 
> beep = putStr "\BEL"

> clear :: IO ()
> clear = calc [] 

> press :: Char -> String -> IO () 
> press c xs = calc (xs ++ [c])

> run :: IO () 
> run = do cls 
>          showbox 
>          clear 


> comment :: Parser () 
> comment = do string "--"
>              some comment'
>              char '\n'
>              return ()

> comment' :: Parser String 
> comment' = space *> (some alphanum <|> (special specialChar *> pure []))  
>               
>             

> specialChar :: String 
> specialChar = "`~!@#$%^&*()[]{}|\\><,.?/:=+-_;"

> special :: String -> Parser ()   
> special [] = empty  
> special (x:xs) = (char x *> pure ()) <|> special xs 

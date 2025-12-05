module Parse where
import Control.Applicative

data Parser a = Parser { parse :: String -> [(a, String)] }

-- Functor instance: apply a function to the result of a parser
instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f px = Parser $ \cs -> [(f x, cs') | (x, cs') <- parse px cs]

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \cs -> [(x, cs)]

    -- This is the parser that does nothing; it simply returns the input in an uninputted state

    -- liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
    -- liftA2 f p q = Parser $ \cs -> [(f x y, cs'') | (x, cs') <- parse p cs, (y, cs'') <- parse q cs']

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b 
    pf <*> px = Parser $ \cs ->
        [(f x, cs'') | (f, cs') <- parse pf cs, (x, cs'') <- parse px cs']

    -- this is the function that does liftA2 on a parser

char :: Char -> Parser Char
char c = Parser $ isChar
    where
        isChar :: String -> [(Char, String)]
        isChar (cs : str)
            | (cs == c) = [(cs, str)]
        isChar _ = []

string :: String -> Parser String
string "" = pure ""
string (c : cs) = (:) <$> (char c) <*> (string cs)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \cs -> []

    (<|>) :: Parser a -> Parser a -> Parser a
    pa <|> pb = Parser $ \cs -> (parse pa cs ++ parse pb cs)

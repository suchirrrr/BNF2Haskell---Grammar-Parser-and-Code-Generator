{-
 Generated at: 2025-10-26T02-36-42
-}
module Output where

import Control.Applicative (many, optional, some, (<|>))
import Instances (ParseResult (..), Parser (..))
import Parser
    ( alpha
    , charTok
    , eof
    , int
    , is
    , string
    , stringTok
    , tok
    )
-- only importing some things from prelude to minimise conflicts with builtins
import Prelude (Char, Int, Maybe, Show, String, show, (<$>), (<*), (<*>))

runParser :: Show a => Parser a -> String -> String
runParser p s = case parse (p <* eof) s of
    Result _ a -> show a
    Error _    -> "Parse Error"

trailing :: Parser ()
trailing = () <$ many ( is ' ' <|> is '\t' <|> is '\r' <|> is '\n' )

newtype Number = Number Int
    deriving Show

newtype Variable = Variable String
    deriving Show

number :: Parser Number
number = Number <$> int

variable :: Parser Variable
variable = Variable <$> (some alpha)



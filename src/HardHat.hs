
module HardHat where

import Control.Applicative hiding (optional)
import Control.Monad
import Data.Char
import Text.ParserCombinators.ReadP

tab, cr, space, nl, lt, ff :: Char
tab   = toEnum 0x0009
cr    = toEnum 0x000D
space = toEnum 0x0020
nl    = toEnum 0x000A
lt    = toEnum 0x000B
ff    = toEnum 0x000C

{- |
  A line is a sequence of zero or more characters other than newline (U+000A) or
  carriage return (U+000D), followed by a line ending or by the end of file.
-}
line :: ReadP String
line = manyTill (satisfy $ const True) lineEnding

{- |
  A line ending is a newline (U+000A), a carriage return (U+000D) not followed
  by a newline, or a carriage return and a following newline.
-}
lineEnding :: ReadP ()
lineEnding = (char nl >> pure ()) <|>
             (char cr >> optional (char nl))

{- |
  A line containing no characters, or a line containing only spaces (U+0020) or
  tabs (U+0009), is called a blank line.
-}
blankLine :: ReadP ()
blankLine = void $ manyTill (satisfy (\c -> c == space || c == tab)) lineEnding

{- |
  A whitespace character is a space (U+0020), tab (U+0009), newline (U+000A),
  line tabulation (U+000B), form feed (U+000C), or carriage return (U+000D).
-}
isWhitespaceChar :: Char -> Bool
isWhitespaceChar c
  | c == space = True
  | c == tab   = True
  | c == nl    = True
  | c == lt    = True
  | c == ff    = True
  | c == cr    = True
  | otherwise = False

{- |
  Whitespace is a sequence of one or more whitespace characters.
-}
whitespace :: ReadP ()
whitespace = void $ many1 (satisfy isWhitespaceChar)

{- |
  A Unicode whitespace character is any code point in the Unicode Zs class, or a
  tab (U+0009), carriage return (U+000D), newline (U+000A), or form feed
  (U+000C).
-}
unicodeWhitespaceChar :: Char -> Bool
unicodeWhitespaceChar c
  | c == tab = True
  | c == cr  = True
  | c == nl  = True
  | c == ff  = True
  | generalCategory c == Space = True
  | otherwise = False

{- |
  Unicode whitespace is a sequence of one or more Unicode whitespace characters.
-}
unicodeWhitespace :: ReadP ()
unicodeWhitespace = void $ many1 (satisfy unicodeWhitespaceChar)

{- |
  A space is U+0020.
-}
isSpace :: Char -> Bool
isSpace c = c == space

{- |
  A non-whitespace character is any character that is not a whitespace character.
-}
isNonWhitespaceChar :: Char -> Bool
isNonWhitespaceChar = not . isWhitespaceChar

{- |
  An ASCII punctuation character is !, ", #, $, %, &, ', (, ), *, +, ,, -, ., /,
  :, ;, <, =, >, ?, @, [, \, ], ^, _, `, {, |, }, or ~.
-}
isAsciiPunctuationChar :: Char -> Bool
isAsciiPunctuationChar =
  flip elem "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

{- |
  A punctuation character is an ASCII punctuation character or anything in the
  Unicode classes Pc, Pd, Pe, Pf, Pi, Po, or Ps.
-}
isPunctuation :: Char -> Bool
isPunctuation c =
  isAsciiPunctuationChar c || case generalCategory c of
    ConnectorPunctuation -> True -- Pc
    DashPunctuation      -> True -- Pd
    OpenPunctuation      -> True -- Ps
    ClosePunctuation     -> True -- Pe
    InitialQuote         -> True -- Pi
    FinalQuote           -> True -- Pf
    OtherPunctuation     -> True -- Po
    _                    -> False

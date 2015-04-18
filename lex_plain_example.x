
{

module Lexer where

-- needed for Unicode (UTF-8) support
import qualified Data.Bits
import Data.Word (Word8)

}

$digit  = [0-9]            -- digits
$lower  = [a-z]            -- lower-case characters
$upper  = [A-Z]            -- upper-case characters
$univ   = [\0-\255]        -- universal: any character

@rsyms =      -- reserved symbols
   \( | \) | \| \| | \^ \^ | \& \& | \! | \= \= | \! \= | \< | \< \= | \> | \> \= | \+ | \- | \* | \/ | \% | \* \* | \+ \+ | \- \- | \[ | \] | \, | \; | \= | \{ | \} | \* \= | \+ \= | \/ \= | \- \= | \& \= | \^ \= | \| \=

@rwrds =      -- reserved keywords
   "for" | "if" | "else" | "while" | "repeat" | "until" | "break" | "continue" | "return" | "bool" | "char" | "float" | "int" | "void" | "val" | "valres" | "ref" | "const" | "res" | "name"

:-

  -- Lexer rules definition

  -- The rule syntax is:
  --    `regex` { `action` }
  -- N.B. to access the ability to use startcodes you need to use a monadinc
  -- lexer.
  
  -- Actions:
  -- The rule actions should have the following signature:
  --      { ... }  :: AlexPos -> String -> Token

  $white+                                         ;
  \/\/ .*                                         ;
  \/\* ([$univ # \*] | \* [$univ # \/])* \*+ \/   ;
  @rsyms                                          { T_RSymb }
  @rwrds                                          { T_RWrds }
  $lower[$lower $upper $digit '_']*               { T_Ident }
  $digit+                                         { \p s -> T_Int p (read s) }
  $digit+ \. $digit+ ("e" "-"? $digit+)?          { \p s -> T_Double p (read s) }
  \" ([$univ # \"] | \\ $univ)* \"                { T_String }
  .                                               ;

{

-- Token type definition
-- The token type is user defined; that's why functions handling tokens
-- usually have a commented type signature (they can't rely on a user
-- defined type).
-- Each of the rules in the "rule section" will return a token; you can
-- eventually pass the token to a Happy-generated parser.

data Token = T_Ident  AlexPos String
           | T_RSymb  AlexPos String
           | T_RWrds  AlexPos String
           | T_Int    AlexPos Int
           | T_Double AlexPos Double
           | T_Char   AlexPos Char
           | T_String AlexPos String
           | T_EOF
    deriving (Eq,Show,Ord)

-- AlexPos type definition
data AlexPos = Pos !Int  -- line number
                   !Int  -- column number
    deriving (Eq, Show,Ord)

startPos = Pos 1 1

-- alexMove function definition
-- Updates the current position, given the next char in the input string.

alexMove :: AlexPos -> Char -> AlexPos
alexMove (Pos l c) '\t' = Pos  l    (((c+7) `div` 8) * 8 + 1)       -- Assumes a tab (\t) is equal to 8 spaces
alexMove (Pos l c) '\n' = Pos (l+1) 1
alexMove (Pos l c) _    = Pos  l    (c+1)

-- AlexInput type definition
-- From documentation:
-- "The generated lexer is independent of the input type, which is why
-- you have to provide a definition for the input type yourself.
-- Note that the input type needs to keep track of the previous character
-- in the input stream; this is used for implementing patterns with a
-- left-context (those that begin with ^ or set^). If you don't ever
-- use patterns with a left-context in your lexical specification,
-- then you can safely forget about the previous character in the input
-- stream, and have alexInputPrevChar return undefined."
-- In the AlexInput type you can keep track of whatever information you
-- need (for example the characters position).

type Byte = Word8
type AlexInput = (AlexPos,   -- position of char currently scanned
                  Char,      -- previous char
                  [Byte],    -- rest of the bytes for the current char (see the alexGetByte definition for a more detailed explanation)
                  String)    -- rest of the input string

-- alexGetByte function definition
-- It basically returns the next byte of the char currently been scanned.
-- The function is called by the Alex defined function 'alexScan', in order
-- make a single step in the DFA to match a rule.
-- Note that from version 3.0 Alex handles the input string in a different
-- way, in order to add the Unicode (UTF-8) support.
-- From v3.0 changelog: 
-- "An Alex lexer now takes a UTF-8 encoded byte sequence as input [...].
-- If you are using the "basic" wrapper or one of the other wrappers that
-- takes a Haskell String as input, the string is automatically encoded
-- into UTF-8 by Alex."


-- alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
-- alexGetChar is renamed to alexGetByte in the generated code.

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p, c, (b:bs), s) = Just (b, (p, c, bs, s))
alexGetByte (p, _, [], s) =
    case  s of
        []    -> Nothing
        (c:s) -> let p' = alexMove p c
                     (b:bs) = utf8Encode c
                 in p' `seq` Just (b, (p', c, bs, s))   -- 'seq' forces p' to be strict

-- alexInputPrevChar function definition
-- Returns the least characted scanned. The function can return undefined
-- if you don't use left-context patterns.

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_,_) = c

-- utf8Encode function definition
-- Encodes a Haskell String to a list of Word8 values, in UTF8 format.
-- If you are using the a wrapper that takes a Haskell String as input,
-- this function is automatically generated by Alex.

utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
    where
        go oc
           | oc <= 0x7f       = [oc]

           | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                                , 0x80 + oc Data.Bits..&. 0x3f
                                ]

           | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                                , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                                , 0x80 + oc Data.Bits..&. 0x3f
                                ]
           | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                                , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                                , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                                , 0x80 + oc Data.Bits..&. 0x3f
                                ]

-- alexScan function definition
-- This is the main lexer function; it takes a string and returns a
-- list of (user defined tokens).
-- It internally calls the Alex defined function 'alexScan', which
-- will scan a single token from the input stream, and return a value
-- of type AlexReturn (see the Alex documentation for a formal
-- definition of this type).
-- From documentation:
-- "The value returned [by alexScan] is either:
--   AlexEOF: the end-of-file was reached;
--   AlexError: a valid token could not be recognised;
--   AlexSkip: the matched token did not have an action associated
--             with it.
--   AlexToken: a token was matched, and the action associated with
--              it is returned."

alexScanTokens :: String -> [Token]
alexScanTokens str = go (startPos, '\n', [], str)
    where
      go :: AlexInput -> [Token]
      go inp@(pos, _, _, str) =
            case alexScan inp 0 of
                AlexEOF                      -> []
                AlexError ((Pos l c),_,_,_)  -> error $ "lexical error at line " ++ (show l) ++ ", column " ++ (show c)
                AlexSkip  inp' len           -> go inp'
                AlexToken inp' len act       -> act pos (take len str) : (go inp')

}

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
  -- Using monads, you gain the ability to use startcodes in order to write
  -- rules with a context.

  -- The rule syntax is:
  --    <`startcode`> `regex` { `action` }
  
  -- Actions:
  -- The rule actions should have the following signature:
  --      { ... }  :: AlexState -> Int -> Alex res

  -- Startcodes:
  -- Alex will internally handle the startcodes, considering them with an
  -- `Int` type signature. Remember that `0` startcode is the default one.
  -- Using no startcode means `all startcodes`.

        $white+                                         { skip }
  <0>   \/\/ .*                                         { skip }
  <0>   \/\* ([$univ # \*] | \* [$univ # \/])* \*+ \/   { skip }
  <0>   @rsyms                                          { \i@S{ curr_pos = p, input_str = s} l -> let s' = take l s
                                                                                                  in return $ Token p $ T_RSymb s' }
  <0>   @rwrds                                          { \i@S{ curr_pos = p, input_str = s} l -> let s' = take l s
                                                                                                  in return $ Token p $ T_RWrds s' }
  <0>   $lower[$lower $upper $digit '_']*               { \i@S{ curr_pos = p, input_str = s} l -> let s' = take l s
                                                                                                  in return $ Token p $ T_Ident s' }
  <0>   $digit+                                         { \i@S{ curr_pos = p, input_str = s} l -> let n = read (take l s)
                                                                                                  in return $ Token p $ T_Int n }
  <0>   $digit+ \. $digit+ ("e" "-"? $digit+)?          { \i@S{ curr_pos = p, input_str = s} l -> let n = read (take l s)
                                                                                                  in return $ Token p $ T_Double n }
  <0>   \" ([$univ # \"] | \\ $univ)* \"                { \i@S{ curr_pos = p, input_str = s} l -> let s' = drop 1 . take (l-1) $ s
                                                                                                  in return $ Token p $ T_String s' }
        .                                               { skip }"
{

-- Token type definition
-- The token type is user defined; that's why functions handling tokens
-- usually have a commented type signature (they can't rely on a user
-- defined type).
-- Each of the rules in the "rule section" will return a token; you can
-- eventually pass the token to a Happy-generated parser.

data TokenType = T_Ident  String
               | T_RSymb  String
               | T_RWrds  String
               | T_Int    Int
               | T_Double Double
               | T_Char   Char
               | T_String String
               | T_EOF
    deriving (Eq,Show,Ord)

data Token = Token { pos :: AlexPos, tok :: TokenType}
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

-- N.B. For some reason the `monad` wrapper uses two different structures
-- to keep track of the state informations (AlexInput and AlexState).
-- The proposed solution uses only the AlexState structure (more
-- intuitive), making an alias for AlexInput (for compatibility with
-- internal functions).

type Byte = Word8

type AlexInput = AlexState
data AlexState = S {
        curr_pos    :: !AlexPos,      -- position at current input location
        prev_char   :: !Char,         -- the character before the input
        curr_bytes  :: [Byte],       -- rest of the bytes for the current char
        input_str   :: String,        -- the current input
        start_code  :: !Int          -- the current startcode
}

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


-- alexGetChar :: AlexState -> Maybe (Char,AlexState)
-- alexGetChar is renamed to alexGetByte in the generated code.

alexGetByte :: AlexState -> Maybe (Byte,AlexState)
alexGetByte i@S{ curr_bytes = (b:bs) } = Just (b, i{ curr_bytes = bs})
alexGetByte i@S{ input_str = [] } = Nothing
alexGetByte i@S{ curr_pos = p, input_str = (c:s) } = let p' = alexMove p c
                                                         (b:bs) = utf8Encode c
                                                     in p' `seq` Just (b, i{ curr_pos = p', curr_bytes = bs, input_str = s })

-- alexInputPrevChar function definition
-- Returns the least characted scanned. The function can return undefined
-- if you don't use left-context patterns.

alexInputPrevChar :: AlexState -> Char
alexInputPrevChar S{ prev_char = c } = c

-- ignorePendingBytes function definition
-- Ignores bytes remained in the buffer after the match of a valid token.

ignorePendingBytes :: AlexState -> AlexState
ignorePendingBytes i = i{ curr_bytes = [] }

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

-- Monad Definition

-- `Alex a` monad definition
-- It's really close to the State monad definition; the Either constructor
-- will handles errors.

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }


-- `Alex a` Monad instancing

instance Monad Alex where
    return a = Alex $ \s -> Right (s,a)
    m >>= k  = Alex $ \s -> case unAlex m s of 
                                 Left msg -> Left msg
                                 Right (s',a) -> unAlex (k a) s'
    fail message = Alex $ \s -> Left message

-- Other useful functions

alexGetInput :: Alex AlexState
alexGetInput = Alex $ \s -> Right (s,s)

alexSetInput :: AlexState -> Alex ()
alexSetInput i = Alex $ \s -> Right (i,())

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@S{ start_code = sc } -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{ start_code = sc }, ())


-- alexMonadScan function definition
-- This is the main function: it operates a single step in the lexing computation
-- and will return an `Alex Token`.
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

alexEOF = return $ Token undefined T_EOF

alexMonadScan = do
    inp <- alexGetInput
    sc <- alexGetStartCode
    case alexScan inp sc of
        AlexEOF -> alexEOF
        AlexError S{ curr_pos = Pos l c } -> fail $ "lexical error at line " ++ (show l) ++ ", column " ++ (show c)
        AlexSkip  inp' len -> do
            alexSetInput inp'
            alexMonadScan
        AlexToken inp' len action -> do
            alexSetInput inp'
            action (ignorePendingBytes inp) len

-- ------------------------------------------------------------------------

-- Useful actions (use them in the rules)

-- Skips the matched string.
-- skip :: AlexAction res
skip input len = alexMonadScan

-- Applies the relative action while changing the startcode.
-- andBegin :: AlexAction res -> Int -> AlexAction res
(act `andBegin` code) input len = do alexSetStartCode code; act input len

-- Simply changes the startcode, ignoring the matched string.
-- begin :: Int -> AlexAction res
begin code = skip `andBegin` code

-- ------------------------------------------------------------------------

-- This section is only needed for a stand-alone monadic lexer.
-- N.B. If you are going to have a monadic parser, it will handle all of this.
--      You can still keep them for debugging purpose.

-- Starting state definition

startState str = S {
                    curr_pos = startPos,
                    input_str = str,
                    prev_char = '\n',
                    curr_bytes = [],
                    start_code = 0
                   }

-- runAlex function definition
-- It will compute the final state of an Alex statefull computations, starting
-- from an initial AlexState.


-- runAlex :: String -> Alex a -> Either String a
runAlex :: String -> Alex [Token] -> Either String [Token]
runAlex str (Alex f) = case f (startState str) of
                            Left msg -> Left msg
                            Right ( _, a ) -> Right a

alexScanTokens :: String -> Either String [Token]
alexScanTokens str = runAlex str $ loop []
    where
        loop acc = do tok <- alexMonadScan
                      if (tok == T_EOF)
                      then return (reverse acc)
                      else loop (tok : acc)

}
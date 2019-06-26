{
{-# LANGUAGE   OverloadedStrings
             , TupleSections
             , FlexibleContexts
             , GeneralizedNewtypeDeriving
  #-}

module Language.STLC.Lex where


import Prelude hiding (lex)
import Lens.Micro.Platform
import Control.Monad
import Control.Monad.Except
import Control.Monad.Extra (mconcatMapM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Bits (shiftR, (.&.))
import Data.Char (digitToInt, ord)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text (Text)
import Data.Word (Word8)
import Language.STLC.Lex.Error
import Language.STLC.Lex.State
import Language.STLC.Lex.Token
import Language.Syntax.Location
import Safe (headDef)
import System.FilePath (FilePath)

import qualified Data.Map            as Map
import qualified Data.Text           as T
import qualified Data.Text.Read      as T
import qualified System.FilePath     as Filesystem

}

-- -----------------------------------------------------------------------------
-- Alex "Character set macros"

$digit = 0-9


$opchar = [\!\#\$\%\&\*\+\/\<\=\>\?\@\\\^\|\-\~\:]

$small        = [a-z]
$large        = [A-Z]
$idchar       = [A-Za-z0-9]
$idcharsym    = [A-Za-z0-9\_\']

$nonwhite       = ~$white
$whiteNoNewline = $white # \n


-- -----------------------------------------------------------------------------
-- Alex "Regular expression macros"

-- Basic Ids
@varid = $small $idcharsym*
@conid = $large $idcharsym*
@primid = \# $small $idcharsym*

@qual = (@conid \.)+
@qvarid = @qual @varid
@qconid = @qual @conid

-- -----------------------------------------------------------------------------
-- Alex "Identifier"

hawk :-

-- -----------------------------------------------------------------------------
-- Alex "Rules"

<0> {
  -- Skip whitespace everywhere
  $whiteNoNewline                 { skipBreak }
  [\n\r]                          { \ _ _ -> nextLineBreak }
  "/*"                            { beginComment }
  "//"                            { beginLineComment }
  
  \"                              { beginString }
  \' .* \'                        { handleChar }
  

  \\                              { rsvp }
  \-\>                            { rsvp }
  \|                              { rsvp }
  \:                              { rsvp }
  \:\:                            { rsvp }
  \;                              { rsvp }
  \,                              { rsvp }
  \.                              { rsvp }
  \=                              { rsvp }
  \_                              { rsvp }
  \~                              { rsvp }
  \*                              { rsvp }
  \&                              { rsvp }

  \(                              { rsvp }
  \)                              { rsvp }
  \[                              { rsvp }
  \]                              { rsvp }
  \{                              { rsvp }
  \}                              { rsvp }
  \<                              { rsvp }
  \>                              { rsvp }

  "I8"                            { rsvp }
  "I32"                           { rsvp }
  "I64"                           { rsvp }
  "F32"                           { rsvp }
  "F64"                           { rsvp }
  "Bool"                          { rsvp }
  "Char"                          { rsvp }
  "Array"                         { rsvp }
  "String"                        { rsvp }
  "Void"                          { rsvp }

  "module"                        { rsvp }
  "import"                        { rsvp }

  "extern"                        { rsvp }
  "type"                          { rsvp }
  "class"                         { rsvp }
  "impl"                          { rsvp }

  "foreign"                       { rsvp }
  "export"			                  { rsvp }
  "ccall"                         { rsvp }

  "infix"                         { rsvp }
  "infixl"                        { rsvp }
  "infixr"                        { rsvp }

  "let"                           { rsvp }
  "in"                            { rsvp }
  "as"                            { rsvp }
  "case"                          { rsvp }
  "of"                            { rsvp }

  "if"                            { rsvp }
  "then"                          { rsvp }
  "elif"                          { rsvp }
  "else"                          { rsvp }

  "new"                           { rsvp }
  "resize"                        { rsvp }
  "delete"                        { rsvp }

  "True"                          { \text -> yieldTokAt (TokenBool True) text}
  "False"                         { \text -> yieldTokAt (TokenBool False) text}

  @primid                         { \text -> yieldTokAt (TokenPrimId text) text }
  @conid                          { \text -> yieldTokAt (TokenConId  text) text }
  @varid                          { \text -> yieldTokAt (TokenVarId text) text }

  $digit* \. $digit+              { \text -> yieldTokAt (TokenDouble $ readDbl text) text }
  $digit+ \. $digit*              { \text -> yieldTokAt (TokenDouble $ readDbl text) text }
  
  ($digit)+                       { \text -> yieldTokAt (TokenInteger $ readInt text) text }
}

<stringSC> {
  \\[nt\"]                        { escapeString }
  \"                              { endString }
  [.]                             { appendString }
}

<commentSC> {
  "/*"                            { continueComment }
  "*/"                            { endComment }
  [\n\r]                          { \_ _ -> nextLineContinue }
  [.]                             { skipContinue }
}

<lineCommentSC> {
  [\n\r]                          { endLineComment }
  [.]                             { skipContinue }
}

{


newtype Lex a = Lex { unLex :: StateT LexState (Except LexError) a }
  deriving ( Functor, Applicative, Monad
           , MonadState LexState
           , MonadError LexError
           )

type LexAction = Text -> Int -> Lex ()


runLexer :: FilePath -> Lex a -> Except LexError a
runLexer fp lexer = evalStateT (unLex lexer) (initialLexState fp)


tag :: Text -> TokenClass -> Lex Token
tag text tc = do
  fp <- use lexFilePath
  r <- use lexRegion
  return $ Token tc text (Loc fp r)


moveRegion :: Int -> Lex ()
moveRegion len = do
  r1 <- use regEnd
  regStart .= r1 
  regEnd . posColumn += len


growRegion :: Int -> Lex ()
growRegion len =
  lexRegion . regEnd . posColumn += len

  
nextLineBreak :: Lex ()
nextLineBreak = do
  regStart . posLine += 1
  regStart . posColumn .= 0

  regEnd . posLine += 1
  regEnd . posColumn .= 0


nextLineContinue :: Lex ()
nextLineContinue = do
  regEnd . posLine += 1
  regEnd . posColumn .= 0


yieldTokAt :: TokenClass -> LexAction
yieldTokAt c text len = do
  moveRegion len
  yieldTaggedTok c text


yieldTaggedTok :: TokenClass -> Text -> Lex ()
yieldTaggedTok c text = do
  t <- tag text c
  yieldTok t

yieldTok :: Token -> Lex ()
yieldTok t =
  lexTokAcc %= (t:)


rsvp :: LexAction
rsvp text =
  yieldTokAt (TokenRsvp text) text


skipBreak :: LexAction
skipBreak text len = do
  moveRegion len

skipContinue :: LexAction
skipContinue text len = do
  growRegion len

beginString :: LexAction
beginString text len =
  do
    moveRegion len
    lexStartcode .= stringSC
  
endString :: LexAction
endString text len = do
  buf <- do
    growRegion len
    use lexStringBuf

  yieldTaggedTok (TokenString $ reverse buf) text
  
  do
    lexStringBuf .= ""
    lexStartcode .= 0
  
appendString :: LexAction
appendString text len =
  do
    growRegion len
    let c = T.head text
    lexStringBuf %= (c:)

escapeString :: LexAction
escapeString text len = do
  let c = T.head $ T.tail text
      unesc =
        case c of
          'n' -> '\n'
          't' -> '\t'
          '"' -> '"'
  growRegion len
  lexStringBuf %= (unesc:)

    

handleChar :: LexAction
handleChar text len = do
  let trim = T.unpack . T.tail . T.init
      yieldCharAt ch = yieldTokAt (TokenChar ch) text len
  case (trim text) of
      ([])   -> yieldCharAt '\0'
      (c:_)  -> yieldCharAt '\n'
      "\t"   -> yieldCharAt '\t'
      "\r"   -> yieldCharAt '\r'
      "\'"   -> yieldCharAt '\''
      _      -> throwError $ InvalidCharLit text


beginComment :: LexAction
beginComment text len =
  do
    moveRegion len
    lexStartcode .= commentSC
    lexCommentDepth .= 1

continueComment :: LexAction
continueComment text len =
  do
    growRegion len
    lexCommentDepth += 1
         
         
endComment :: LexAction
endComment _ len =
  do
    growRegion len
    
    lexCommentDepth -= 1
    cd <- use lexCommentDepth

    lexStartcode .=
      if cd == 0
        then 0
        else commentSC

beginLineComment :: LexAction
beginLineComment text len =
  do
    moveRegion len
    lexStartcode .= lineCommentSC

endLineComment :: LexAction
endLineComment text len =
  do
    nextLineContinue
    lexStartcode .= 0



-- Helpers

forceRight :: Either a b -> b
forceRight (Right b) = b
forceRight _ = undefined

readInt :: Text -> Integer
readInt = fst . forceRight . T.decimal

readSignedInt :: Text -> Integer
readSignedInt = fst . forceRight . T.signed T.decimal

readDbl :: Text -> Double
readDbl = fst . forceRight . T.double

readSignedDbl :: Text -> Double
readSignedDbl = fst . forceRight . T.signed T.double

-- This was lifted almost intact from the @alex@ source code
encode :: Char -> (Word8, [Word8])
encode c = (fromIntegral h, map fromIntegral t)
  where
    (h, t) = go (ord c)
    go n
        | n <= 0x7f   = (n, [])
        | n <= 0x7ff  = (0xc0 + (n `shiftR` 6), [0x80 + n .&. 0x3f])
        | n <= 0xffff =
            (   0xe0 + (n `shiftR` 12)
            ,   [   0x80 + ((n `shiftR` 6) .&. 0x3f)
                ,   0x80 + n .&. 0x3f
                ]
            )
        | otherwise   =
            (   0xf0 + (n `shiftR` 18)
            ,   [   0x80 + ((n `shiftR` 12) .&. 0x3f)
                ,   0x80 + ((n `shiftR` 6) .&. 0x3f)
                ,   0x80 + n .&. 0x3f
                ]
            )


{- @alex@ does not provide a `Text` wrapper, so the following code just modifies
   the code from their @basic@ wrapper to work with `Text`

   I could not get the @basic-bytestring@ wrapper to work; it does not correctly
   recognize Unicode regular expressions.
-}
data AlexInput = AlexInput
    { prevChar  :: Char
    , currBytes :: [Word8]
    , currInput :: Text
    }

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AlexInput c bytes text) = case bytes of
    b:ytes -> Just (b, AlexInput c ytes text)
    []     -> case T.uncons text of
        Nothing       -> Nothing
        Just (t, ext) -> case encode t of
            (b, ytes) -> Just (b, AlexInput t ytes ext)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = prevChar



lex :: FilePath -> Text -> Except LexError [Token]
lex fp text = do
  runLexer fp start

  where
    start = go $ AlexInput '\n' [] text


    go input = do
      sc <- use lexStartcode
      case alexScan input sc of
        AlexEOF                         -> do
            yieldTaggedTok TokenEof ""
            reverse <$> use lexTokAcc

        AlexError (AlexInput p cs text) -> do
            fp <- use lexFilePath
            r  <- use lexRegion
            let l = Loc fp r
            throwError $ UnrecognizedToken (headDef (show p) $ words $ show text) l

        AlexSkip  input' len           -> do
            throwError IllegalLexerSkip

        AlexToken input' len act       -> do
            act (T.take (fromIntegral len) (currInput input)) (fromIntegral len)
            go input'

}

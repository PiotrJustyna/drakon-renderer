{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LINE 1 "./app/Drakon/Lexer.x" #-}
module Lexer
  (alexScanTokens,
  Token(TokenAction,
  TokenSoloIdentifier,
  TokenOCB,
  TokenCCB)) where
#include "ghcconfig.h"
import qualified Data.Array
import Data.Array.Base (unsafeAt)
import GHC.Exts (Addr#,Int#,Int(I#),(*#),(+#),(-#),(==#),(>=#),indexCharOffAddr#,indexInt16OffAddr#,indexInt32OffAddr#,int2Word#,narrow16Int#,narrow32Int#,negateInt#,or#,ord#,uncheckedShiftL#,word2Int#)
import qualified GHC.Exts
#define ALEX_BASIC 1
-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

#if defined(ALEX_MONAD) || defined(ALEX_MONAD_BYTESTRING) || defined(ALEX_MONAD_STRICT_TEXT)
import Control.Applicative as App (Applicative (..))
#endif

#if defined(ALEX_STRICT_TEXT) || defined (ALEX_POSN_STRICT_TEXT) || defined(ALEX_MONAD_STRICT_TEXT)
import qualified Data.Text
#endif

import Data.Word (Word8)

#if defined(ALEX_BASIC_BYTESTRING) || defined(ALEX_POSN_BYTESTRING) || defined(ALEX_MONAD_BYTESTRING)

import Data.Int (Int64)
import qualified Data.Char
import qualified Data.ByteString.Lazy     as ByteString
import qualified Data.ByteString.Internal as ByteString (w2c)

#elif defined(ALEX_STRICT_BYTESTRING)

import qualified Data.Char
import qualified Data.ByteString          as ByteString
import qualified Data.ByteString.Internal as ByteString hiding (ByteString)
import qualified Data.ByteString.Unsafe   as ByteString

#else

import Data.Char (ord)
import qualified Data.Bits

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = uncurry (:) . utf8Encode'

utf8Encode' :: Char -> (Word8, [Word8])
utf8Encode' c = case go (ord c) of
                  (x, xs) -> (fromIntegral x, map fromIntegral xs)
 where
  go oc
   | oc <= 0x7f       = ( oc
                        , [
                        ])

   | oc <= 0x7ff      = ( 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , [0x80 + oc Data.Bits..&. 0x3f
                        ])

   | oc <= 0xffff     = ( 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , [0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ])
   | otherwise        = ( 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , [0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ])

#endif

type Byte = Word8

-- -----------------------------------------------------------------------------
-- The input type

#if defined(ALEX_POSN) || defined(ALEX_MONAD) || defined(ALEX_GSCAN)
type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,_ps,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_p,c,_bs,_s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c
                              in case utf8Encode' c of
                                   (b, bs) -> p' `seq`  Just (b, (p', c, bs, s))
#endif

#if defined (ALEX_STRICT_TEXT)
type AlexInput = (Char,           -- previous char
                  [Byte],         -- pending bytes on current char
                  Data.Text.Text) -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (c,_ps,s) = (c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c,_bs,_s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (c,(b:bs),s) = Just (b,(c,bs,s))
alexGetByte (_,[],s) = case Data.Text.uncons s of
                            Just (c, cs) ->
                              case utf8Encode' c of
                                (b, bs) -> Just (b, (c, bs, cs))
                            Nothing ->
                              Nothing
#endif

#if defined (ALEX_POSN_STRICT_TEXT) || defined(ALEX_MONAD_STRICT_TEXT)
type AlexInput = (AlexPosn,       -- current position,
                  Char,           -- previous char
                  [Byte],         -- pending bytes on current char
                  Data.Text.Text) -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,_ps,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_p,c,_bs,_s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (p,_,[],s) = case Data.Text.uncons s of
                            Just (c, cs) ->
                              let p' = alexMove p c
                              in case utf8Encode' c of
                                   (b, bs) -> p' `seq`  Just (b, (p', c, bs, cs))
                            Nothing ->
                              Nothing
#endif

#if defined(ALEX_POSN_BYTESTRING) || defined(ALEX_MONAD_BYTESTRING)
type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  ByteString.ByteString,        -- current input string
                  Int64)           -- bytes consumed so far

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes i = i   -- no pending bytes when lexing bytestrings

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_,_) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,_,cs,n) =
    case ByteString.uncons cs of
        Nothing -> Nothing
        Just (b, cs') ->
            let c   = ByteString.w2c b
                p'  = alexMove p c
                n'  = n+1
            in p' `seq` cs' `seq` n' `seq` Just (b, (p', c, cs',n'))
#endif

#ifdef ALEX_BASIC_BYTESTRING
data AlexInput = AlexInput { alexChar :: {-# UNPACK #-} !Char,      -- previous char
                             alexStr ::  !ByteString.ByteString,    -- current input string
                             alexBytePos :: {-# UNPACK #-} !Int64}  -- bytes consumed so far

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = alexChar

alexGetByte (AlexInput {alexStr=cs,alexBytePos=n}) =
    case ByteString.uncons cs of
        Nothing -> Nothing
        Just (c, rest) ->
            Just (c, AlexInput {
                alexChar = ByteString.w2c c,
                alexStr =  rest,
                alexBytePos = n+1})
#endif

#ifdef ALEX_STRICT_BYTESTRING
data AlexInput = AlexInput { alexChar :: {-# UNPACK #-} !Char,
                             alexStr :: {-# UNPACK #-} !ByteString.ByteString,
                             alexBytePos :: {-# UNPACK #-} !Int}

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = alexChar

alexGetByte (AlexInput {alexStr=cs,alexBytePos=n}) =
    case ByteString.uncons cs of
        Nothing -> Nothing
        Just (c, rest) ->
            Just (c, AlexInput {
                alexChar = ByteString.w2c c,
                alexStr =  rest,
                alexBytePos = n+1})
#endif

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of characters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

#if defined(ALEX_POSN) || defined(ALEX_MONAD) || defined(ALEX_POSN_BYTESTRING) || defined(ALEX_MONAD_BYTESTRING) || defined(ALEX_GSCAN) || defined (ALEX_POSN_STRICT_TEXT) || defined(ALEX_MONAD_STRICT_TEXT)
data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq, Show, Ord)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (c+alex_tab_size-((c-1) `mod` alex_tab_size))
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)
#endif

-- -----------------------------------------------------------------------------
-- Monad (default and with ByteString input)

#if defined(ALEX_MONAD) || defined(ALEX_MONAD_BYTESTRING) || defined(ALEX_MONAD_STRICT_TEXT)
data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
#ifdef ALEX_MONAD_STRICT_TEXT
        alex_inp :: Data.Text.Text,
        alex_chr :: !Char,
        alex_bytes :: [Byte],
#endif /* ALEX_MONAD_STRICT_TEXT */
#ifdef ALEX_MONAD
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],
#endif /* ALEX_MONAD */
#ifdef ALEX_MONAD_BYTESTRING
        alex_bpos:: !Int64,     -- bytes consumed so far
        alex_inp :: ByteString.ByteString,      -- the current input
        alex_chr :: !Char,      -- the character before the input
#endif /* ALEX_MONAD_BYTESTRING */
        alex_scd :: !Int        -- the current startcode
#ifdef ALEX_MONAD_USER_STATE
      , alex_ust :: AlexUserState -- AlexUserState will be defined in the user program
#endif
    }

-- Compile with -funbox-strict-fields for best results!

#ifdef ALEX_MONAD
runAlex :: String -> Alex a -> Either String a
runAlex input__ (Alex f)
   = case f (AlexState {alex_bytes = [],
                        alex_pos = alexStartPos,
                        alex_inp = input__,
                        alex_chr = '\n',
#ifdef ALEX_MONAD_USER_STATE
                        alex_ust = alexInitUserState,
#endif
                        alex_scd = 0}) of Left msg -> Left msg
                                          Right ( _, a ) -> Right a
#endif

#ifdef ALEX_MONAD_BYTESTRING
runAlex :: ByteString.ByteString -> Alex a -> Either String a
runAlex input__ (Alex f)
   = case f (AlexState {alex_bpos = 0,
                        alex_pos = alexStartPos,
                        alex_inp = input__,
                        alex_chr = '\n',
#ifdef ALEX_MONAD_USER_STATE
                        alex_ust = alexInitUserState,
#endif
                        alex_scd = 0}) of Left msg -> Left msg
                                          Right ( _, a ) -> Right a
#endif

#ifdef ALEX_MONAD_STRICT_TEXT
runAlex :: Data.Text.Text -> Alex a -> Either String a
runAlex input__ (Alex f)
   = case f (AlexState {alex_bytes = [],
                        alex_pos = alexStartPos,
                        alex_inp = input__,
                        alex_chr = '\n',
#ifdef ALEX_MONAD_USER_STATE
                        alex_ust = alexInitUserState,
#endif
                        alex_scd = 0}) of Left msg -> Left msg
                                          Right ( _, a ) -> Right a
#endif

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Functor Alex where
  fmap f a = Alex $ \s -> case unAlex a s of
                            Left msg -> Left msg
                            Right (s', a') -> Right (s', f a')

instance Applicative Alex where
  pure a   = Alex $ \s -> Right (s, a)
  fa <*> a = Alex $ \s -> case unAlex fa s of
                            Left msg -> Left msg
                            Right (s', f) -> case unAlex a s' of
                                               Left msg -> Left msg
                                               Right (s'', b) -> Right (s'', f b)

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of
                                Left msg -> Left msg
                                Right (s',a) -> unAlex (k a) s'
  return = App.pure


#ifdef ALEX_MONAD
alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} ->
        Right (s, (pos,c,bs,inp__))
#endif

#ifdef ALEX_MONAD_BYTESTRING
alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_bpos=bpos,alex_chr=c,alex_inp=inp__} ->
        Right (s, (pos,c,inp__,bpos))
#endif

#ifdef ALEX_MONAD_STRICT_TEXT
alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} ->
        Right (s, (pos,c,bs,inp__))
#endif

#ifdef ALEX_MONAD
alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,bs,inp__)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} of
                    state__@(AlexState{}) -> Right (state__, ())
#endif

#ifdef ALEX_MONAD_BYTESTRING
alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,inp__,bpos)
 = Alex $ \s -> case s{alex_pos=pos,
                       alex_bpos=bpos,
                       alex_chr=c,
                       alex_inp=inp__} of
                    state__@(AlexState{}) -> Right (state__, ())
#endif

#ifdef ALEX_MONAD_STRICT_TEXT
alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,bs,inp__)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp__} of
                    state__@(AlexState{}) -> Right (state__, ())
#endif

alexError :: String -> Alex a
alexError message = Alex $ const $ Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())

#if defined(ALEX_MONAD_USER_STATE)
alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s,ust)

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState ss = Alex $ \s -> Right (s{alex_ust=ss}, ())
#endif /* defined(ALEX_MONAD_USER_STATE) */

#ifdef ALEX_MONAD
alexMonadScan = do
  inp__ <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
    AlexSkip  inp__' _len -> do
        alexSetInput inp__'
        alexMonadScan
    AlexToken inp__' len action -> do
        alexSetInput inp__'
        action (ignorePendingBytes inp__) len
#endif

#ifdef ALEX_MONAD_BYTESTRING
alexMonadScan = do
  inp__@(_,_,_,n) <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
    AlexSkip  inp__' _len -> do
        alexSetInput inp__'
        alexMonadScan
    AlexToken inp__'@(_,_,_,n') _ action -> let len = n'-n in do
        alexSetInput inp__'
        action (ignorePendingBytes inp__) len
#endif

#ifdef ALEX_MONAD_STRICT_TEXT
alexMonadScan = do
  inp__ <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp__ sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
    AlexSkip  inp__' _len -> do
        alexSetInput inp__'
        alexMonadScan
    AlexToken inp__' len action -> do
        alexSetInput inp__'
        action (ignorePendingBytes inp__) len
#endif

-- -----------------------------------------------------------------------------
-- Useful token actions

#ifdef ALEX_MONAD
type AlexAction result = AlexInput -> Int -> Alex result
#endif

#ifdef ALEX_MONAD_BYTESTRING
type AlexAction result = AlexInput -> Int64 -> Alex result
#endif

#ifdef ALEX_MONAD_STRICT_TEXT
type AlexAction result = AlexInput -> Int -> Alex result
#endif

-- just ignore this token and scan another one
-- skip :: AlexAction result
skip _input _len = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code _input _len = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input__ len = do
  alexSetStartCode code
  action input__ len

#ifdef ALEX_MONAD
token :: (AlexInput -> Int -> token) -> AlexAction token
token t input__ len = return (t input__ len)
#endif

#ifdef ALEX_MONAD_BYTESTRING
token :: (AlexInput -> Int64 -> token) -> AlexAction token
token t input__ len = return (t input__ len)
#endif

#ifdef ALEX_MONAD_STRICT_TEXT
token :: (AlexInput -> Int -> token) -> AlexAction token
token t input__ len = return (t input__ len)
#endif

#endif /* defined(ALEX_MONAD) || defined(ALEX_MONAD_BYTESTRING) || defined(ALEX_MONAD_STRICT_TEXT) */

-- -----------------------------------------------------------------------------
-- Basic wrapper

#ifdef ALEX_BASIC
type AlexInput = (Char,[Byte],String)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c,_,_) = c

-- alexScanTokens :: String -> [token]
alexScanTokens str = go ('\n',[],str)
  where go inp__@(_,_bs,s) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp__' _ln     -> go inp__'
                AlexToken inp__' len act -> act (take len s) : go inp__'

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (c,(b:bs),s) = Just (b,(c,bs,s))
alexGetByte (_,[],[])    = Nothing
alexGetByte (_,[],(c:s)) = case utf8Encode' c of
                             (b, bs) -> Just (b, (c, bs, s))
#endif


-- -----------------------------------------------------------------------------
-- Basic wrapper, ByteString version

#ifdef ALEX_BASIC_BYTESTRING

-- alexScanTokens :: ByteString.ByteString -> [token]
alexScanTokens str = go (AlexInput '\n' str 0)
  where go inp__ =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp__' _len  -> go inp__'
                AlexToken inp__' _ act ->
                  let len = alexBytePos inp__' - alexBytePos inp__ in
                  act (ByteString.take len (alexStr inp__)) : go inp__'

#endif

#ifdef ALEX_STRICT_BYTESTRING

-- alexScanTokens :: ByteString.ByteString -> [token]
alexScanTokens str = go (AlexInput '\n' str 0)
  where go inp__ =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp__' _len  -> go inp__'
                AlexToken inp__' _ act ->
                  let len = alexBytePos inp__' - alexBytePos inp__ in
                  act (ByteString.take len (alexStr inp__)) : go inp__'

#endif

#ifdef ALEX_STRICT_TEXT
-- alexScanTokens :: Data.Text.Text -> [token]
alexScanTokens str = go ('\n',[],str)
  where go inp__@(_,_bs,s) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp__' _len  -> go inp__'
                AlexToken inp__' len act -> act (Data.Text.take len s) : go inp__'
#endif

#ifdef ALEX_POSN_STRICT_TEXT
-- alexScanTokens :: Data.Text.Text -> [token]
alexScanTokens str = go (alexStartPos,'\n',[],str)
  where go inp__@(pos,_,_bs,s) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError ((AlexPn _ line column),_,_,_) -> error $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
                AlexSkip  inp__' _len  -> go inp__'
                AlexToken inp__' len act -> act pos (Data.Text.take len s) : go inp__'
#endif


-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.

#ifdef ALEX_POSN
--alexScanTokens :: String -> [token]
alexScanTokens str0 = go (alexStartPos,'\n',[],str0)
  where go inp__@(pos,_,_,str) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError ((AlexPn _ line column),_,_,_) -> error $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
                AlexSkip  inp__' _ln     -> go inp__'
                AlexToken inp__' len act -> act pos (take len str) : go inp__'
#endif


-- -----------------------------------------------------------------------------
-- Posn wrapper, ByteString version

#ifdef ALEX_POSN_BYTESTRING
--alexScanTokens :: ByteString.ByteString -> [token]
alexScanTokens str0 = go (alexStartPos,'\n',str0,0)
  where go inp__@(pos,_,str,n) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError ((AlexPn _ line column),_,_,_) -> error $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
                AlexSkip  inp__' _len       -> go inp__'
                AlexToken inp__'@(_,_,_,n') _ act ->
                  act pos (ByteString.take (n'-n) str) : go inp__'
#endif


-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.

#ifdef ALEX_GSCAN
alexGScan stop__ state__ inp__ =
  alex_gscan stop__ alexStartPos '\n' [] inp__ (0,state__)

alex_gscan stop__ p c bs inp__ (sc,state__) =
  case alexScan (p,c,bs,inp__) sc of
        AlexEOF     -> stop__ p c inp__ (sc,state__)
        AlexError _ -> stop__ p c inp__ (sc,state__)
        AlexSkip (p',c',bs',inp__') _len ->
          alex_gscan stop__ p' c' bs' inp__' (sc,state__)
        AlexToken (p',c',bs',inp__') len k ->
           k p c inp__ len (\scs -> alex_gscan stop__ p' c' bs' inp__' scs)                  (sc,state__)
#endif
alex_tab_size :: Int
alex_tab_size = 8
alex_base :: AlexAddr
alex_base = AlexA#
  "\xf8\xff\xff\xff\x6d\x00\x00\x00\xdf\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x51\x01\x00\x00\xfe\xff\xff\xff\x03\x00\x00\x00\x00\x00\x00\x00\xc3\x01\x00\x00\x35\x02\x00\x00\x00\x00\x00\x00"#

alex_table :: AlexAddr
alex_table = AlexA#
  "\x00\x00\x07\x00\x07\x00\x07\x00\x07\x00\x07\x00\x00\x00\x06\x00\x06\x00\x06\x00\x06\x00\x06\x00\x07\x00\x07\x00\x07\x00\x07\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x06\x00\x02\x00\x0a\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x03\x00\x00\x00\x04\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x01\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x01\x00\x00\x00\x00\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x06\x00\x06\x00\x06\x00\x06\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x01\x00\x00\x00\x00\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x01\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x09\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x09\x00\x00\x00\x00\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x09\x00\x00\x00\x00\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

alex_check :: AlexAddr
alex_check = AlexA#
  "\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\xff\xff\x22\x00\xff\xff\xff\xff\xff\xff\x20\x00\x27\x00\x22\x00\xff\xff\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x7b\x00\xff\xff\x7d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\x22\x00\xff\xff\xff\xff\xff\xff\xff\xff\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2c\x00\x2d\x00\xff\xff\xff\xff\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2c\x00\x2d\x00\xff\xff\xff\xff\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\x22\x00\xff\xff\xff\xff\xff\xff\xff\xff\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2c\x00\x2d\x00\xff\xff\xff\xff\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\x21\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2c\x00\x2d\x00\xff\xff\xff\xff\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

alex_deflt :: AlexAddr
alex_deflt = AlexA#
  "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

alex_accept = Data.Array.listArray (0 :: Int, 11)
  [ AlexAccNone
  , AlexAccNone
  , AlexAcc 4
  , AlexAcc 3
  , AlexAcc 2
  , AlexAccNone
  , AlexAccNone
  , AlexAccSkip
  , AlexAcc 1
  , AlexAccNone
  , AlexAccNone
  , AlexAcc 0
  ]

alex_actions = Data.Array.array (0 :: Int, 5)
  [ (4,alex_action_3)
  , (3,alex_action_4)
  , (2,alex_action_5)
  , (1,alex_action_1)
  , (0,alex_action_2)
  ]

alex_action_1 = \s -> TokenAction s
alex_action_2 = \s -> TokenAction ("# " <> s) -- # is a placeholder id that will later be replaced by a unique identifier
alex_action_3 = \s -> TokenSoloIdentifier s
alex_action_4 = \_ -> TokenOCB
alex_action_5 = \_ -> TokenCCB

#define ALEX_GHC 1
#define ALEX_NOPRED 1
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

#ifdef ALEX_GHC
#  define ILIT(n) n#
#  define IBOX(n) (I# (n))
#  define FAST_INT Int#
-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#  if __GLASGOW_HASKELL__ > 706
#    define GTE(n,m) (GHC.Exts.tagToEnum# (n >=# m))
#    define EQ(n,m) (GHC.Exts.tagToEnum# (n ==# m))
#  else
#    define GTE(n,m) (n >=# m)
#    define EQ(n,m) (n ==# m)
#  endif
#  define PLUS(n,m) (n +# m)
#  define MINUS(n,m) (n -# m)
#  define TIMES(n,m) (n *# m)
#  define NEGATE(n) (negateInt# (n))
#  define IF_GHC(x) (x)
#else
#  define ILIT(n) (n)
#  define IBOX(n) (n)
#  define FAST_INT Int
#  define GTE(n,m) (n >= m)
#  define EQ(n,m) (n == m)
#  define PLUS(n,m) (n + m)
#  define MINUS(n,m) (n - m)
#  define TIMES(n,m) (n * m)
#  define NEGATE(n) (negate (n))
#  define IF_GHC(x)
#endif

#ifdef ALEX_GHC
data AlexAddr = AlexA# Addr#
-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.

{-# INLINE alexIndexInt16OffAddr #-}
alexIndexInt16OffAddr :: AlexAddr -> Int# -> Int#
alexIndexInt16OffAddr (AlexA# arr) off =
#if __GLASGOW_HASKELL__ >= 901
  GHC.Exts.int16ToInt# -- qualified import because it doesn't exist on older GHC's
#endif
#ifdef WORDS_BIGENDIAN
  (GHC.Exts.word16ToInt16# (GHC.Exts.wordToWord16# (GHC.Exts.byteSwap16# (GHC.Exts.word16ToWord# (GHC.Exts.int16ToWord16#
#endif
  (indexInt16OffAddr# arr off)
#ifdef WORDS_BIGENDIAN
  )))))
#endif
#else
alexIndexInt16OffAddr = (Data.Array.!)
#endif

#ifdef ALEX_GHC
{-# INLINE alexIndexInt32OffAddr #-}
alexIndexInt32OffAddr :: AlexAddr -> Int# -> Int#
alexIndexInt32OffAddr (AlexA# arr) off =
#if __GLASGOW_HASKELL__ >= 901
  GHC.Exts.int32ToInt# -- qualified import because it doesn't exist on older GHC's
#endif
#ifdef WORDS_BIGENDIAN
  (GHC.Exts.word32ToInt32# (GHC.Exts.wordToWord32# (GHC.Exts.byteSwap32# (GHC.Exts.word32ToWord# (GHC.Exts.int32ToWord32#
#endif
  (indexInt32OffAddr# arr off)
#ifdef WORDS_BIGENDIAN
  )))))
#endif
#else
alexIndexInt32OffAddr = (Data.Array.!)
#endif

#ifdef ALEX_GHC
-- GHC >= 503, unsafeAt is available from Data.Array.Base.
quickIndex = unsafeAt
#else
quickIndex = (Data.Array.!)
#endif

-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input__ IBOX(sc)
  = alexScanUser (error "alex rule requiring context was invoked by alexScan; use alexScanUser instead?") input__ IBOX(sc)

-- If the generated alexScan/alexScanUser functions are called multiple times
-- in the same file, alexScanUser gets broken out into a separate function and
-- increases memory usage. Make sure GHC inlines this function and optimizes it.
{-# INLINE alexScanUser #-}

alexScanUser user__ input__ IBOX(sc)
  = case alex_scan_tkn user__ input__ ILIT(0) input__ sc AlexNone of
  (AlexNone, input__') ->
    case alexGetByte input__ of
      Nothing ->
#ifdef ALEX_DEBUG
                                   Debug.Trace.trace ("End of input.") $
#endif
                                   AlexEOF
      Just _ ->
#ifdef ALEX_DEBUG
                                   Debug.Trace.trace ("Error.") $
#endif
                                   AlexError input__'

  (AlexLastSkip input__'' len, _) ->
#ifdef ALEX_DEBUG
    Debug.Trace.trace ("Skipping.") $
#endif
    AlexSkip input__'' len

  (AlexLastAcc k input__''' len, _) ->
#ifdef ALEX_DEBUG
    Debug.Trace.trace ("Accept.") $
#endif
    AlexToken input__''' len ((Data.Array.!) alex_actions k)


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user__ orig_input len input__ s last_acc =
  input__ `seq` -- strict in the input
  let
  new_acc = (check_accs (alex_accept `quickIndex` IBOX(s)))
  in
  new_acc `seq`
  case alexGetByte input__ of
     Nothing -> (new_acc, input__)
     Just (c, new_input) ->
#ifdef ALEX_DEBUG
      Debug.Trace.trace ("State: " ++ show IBOX(s) ++ ", char: " ++ show c ++ " " ++ (show . chr . fromIntegral) c) $
#endif
      case fromIntegral c of { IBOX(ord_c) ->
        let
                base   = alexIndexInt32OffAddr alex_base s
                offset = PLUS(base,ord_c)

                new_s = if GTE(offset,ILIT(0))
                          && let check  = alexIndexInt16OffAddr alex_check offset
                             in  EQ(check,ord_c)
                          then alexIndexInt16OffAddr alex_table offset
                          else alexIndexInt16OffAddr alex_deflt s
        in
        case new_s of
            ILIT(-1) -> (new_acc, input__)
                -- on an error, we want to keep the input *before* the
                -- character that failed, not after.
            _ -> alex_scan_tkn user__ orig_input
#ifdef ALEX_LATIN1
                   PLUS(len,ILIT(1))
                   -- issue 119: in the latin1 encoding, *each* byte is one character
#else
                   (if c < 0x80 || c >= 0xC0 then PLUS(len,ILIT(1)) else len)
                   -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
#endif
                   new_input new_s new_acc
      }
  where
        check_accs (AlexAccNone) = last_acc
        check_accs (AlexAcc a  ) = AlexLastAcc a input__ IBOX(len)
        check_accs (AlexAccSkip) = AlexLastSkip  input__ IBOX(len)
#ifndef ALEX_NOPRED
        check_accs (AlexAccPred a predx rest)
           | predx user__ orig_input IBOX(len) input__
           = AlexLastAcc a input__ IBOX(len)
           | otherwise
           = check_accs rest
        check_accs (AlexAccSkipPred predx rest)
           | predx user__ orig_input IBOX(len) input__
           = AlexLastSkip input__ IBOX(len)
           | otherwise
           = check_accs rest
#endif

data AlexLastAcc
  = AlexNone
  | AlexLastAcc !Int !AlexInput !Int
  | AlexLastSkip     !AlexInput !Int

data AlexAcc user
  = AlexAccNone
  | AlexAcc Int
  | AlexAccSkip
#ifndef ALEX_NOPRED
  | AlexAccPred Int (AlexAccPred user) (AlexAcc user)
  | AlexAccSkipPred (AlexAccPred user) (AlexAcc user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user__ in1 len in2
  = p1 user__ in1 len in2 && p2 user__ in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _
alexPrevCharIs c _ input__ _ _ = c == alexInputPrevChar input__

alexPrevCharMatches f _ input__ _ _ = f (alexInputPrevChar input__)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _
alexPrevCharIsOneOf arr _ input__ _ _ = arr Data.Array.! alexInputPrevChar input__

--alexRightContext :: Int -> AlexAccPred _
alexRightContext IBOX(sc) user__ _ _ input__ =
     case alex_scan_tkn user__ input__ ILIT(0) input__ sc AlexNone of
          (AlexNone, _) -> False
          _ -> True
        -- TODO: there's no need to find the longest
        -- match when checking the right context, just
        -- the first match will do.
#endif
{-# LINE 30 "./app/Drakon/Lexer.x" #-}
data Token
  = TokenAction String
  | TokenSoloIdentifier String
  | TokenOCB
  | TokenCCB
  deriving Show

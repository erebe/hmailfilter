{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (
    getHeaders
    , parseHeader
    , parseHeaders
    , Header(..)
    , HeaderName(..)
    ) where



import           Protolude                        hiding (foldMap, fromList)
import           Data.Attoparsec.ByteString       as P hiding (takeWhile)
import           Data.Attoparsec.ByteString.Char8 as PC
import           Data.Attoparsec.ByteString.Lazy  as PL
import           Data.ByteString.Base64           as B64 hiding (decode)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as BC
import qualified Data.Char                        as C
import           Data.HashMap.Strict              
import qualified Data.Text.Encoding               as T
import qualified Data.Text.Encoding.Error         as T
import qualified Text.Regex.PCRE.Light            as Re

data HeaderName = ReturnPath
                | OriginalTo
                | DeliveredTo
                | Received
                | ContentType
                | ThreadTopic
                | Date
                | Subject
                | From
                | To
                | Cc
                | Bcc
                | ListID
                | Spam
                | Unknown !Text
                deriving (Show, Read, Eq, Ord, Generic, Hashable)

data Header = Header {
      name    :: !HeaderName
    , content :: !Text
    } deriving (Show, Read)

headerMapping :: HashMap ByteString HeaderName
headerMapping = fromList [
      ("return-path"   , ReturnPath)
    , ("x-original-to" , OriginalTo)
    , ("delivered-to"  , DeliveredTo)
    , ("received"      , Received)
    , ("content-type"  , ContentType)
    , ("thread-topic"  , ThreadTopic)
    , ("date"          , Date)
    , ("subject"       , Subject)
    , ("from"          , From)
    , ("to"            , To)
    , ("cc"            , Cc)
    , ("bcc"           , Bcc)
    , ("list-id"       , ListID)
    , ("x-spam"        , Spam)
    ]

parseHeaderName :: Parser HeaderName
parseHeaderName = do
    val <- BC.map C.toLower <$> PC.takeWhile (PC.notInClass "\r\n:")
    _ <- char ':'
    return $! lookupDefault (Unknown (T.decodeUtf8With T.ignore val)) val headerMapping

{-# INLINE parseHeaderName #-}


parseQEncodedWord :: Parser ByteString
parseQEncodedWord = do
    ch <- PC.peekChar'
    case ch of
      '_' -> P.take 1 >> return " "
      '=' -> P.take 1 >> parseHexadecimal
      _   -> consumeRegularInput

    where
      consumeRegularInput = do
          val <- PC.takeWhile (PC.inClass "=_")
          if BC.null val
            then mempty
            else return val

      parseHexadecimal = do
          hex <- P.take 2
          let ret = parseOnly hexadecimal hex
          return $! either (const hex) B.singleton ret

{-# INLINE parseQEncodedWord #-}



prettyFormatHeaderContent :: ByteString -> Text
prettyFormatHeaderContent str = fromMaybe (T.decodeUtf8 str) $ do
    [_,h, charset, encoding, payload, t] <- Re.match rx str []

    return $! T.decodeUtf8 h
           <> (decodeCharset charset . decodeEncoding encoding $ payload)
           <> prettyFormatHeaderContent t

    where
      decodeEncoding en = if BC.map C.toUpper en == "B"
                          then B64.decodeLenient
                          else \str' -> either (const str') B.concat
                                        (parseOnly (many1 parseQEncodedWord) str')
      decodeCharset ch = case BC.map C.toUpper ch of
                          "UTF-8"  -> T.decodeUtf8With T.ignore
                          "UTF-16" -> T.decodeUtf16LEWith T.ignore
                          "UTF-32" -> T.decodeUtf32LEWith T.ignore
                          "ISO-8859-1" -> T.decodeLatin1
                          _ -> T.decodeUtf8With T.ignore
      rx = flip Re.compile [Re.caseless] $ "^(.*?)=\\?"
                                        <> "([^\\?]+)\\?([QB])\\?(.*?)\\?="
                                        <> "(.*)$"
{-# INLINE prettyFormatHeaderContent #-}



parseHeaderContent :: Parser ByteString
parseHeaderContent = do
        P.skipWhile isHorizontalSpace
        value <- P.takeTill isEndOfLine
        _ <- endOfLine
        next <- P.peekWord8
        if fromMaybe False (isHorizontalSpace <$> next)
        then (\nextVal -> value <> " " <> nextVal) <$> parseHeaderContent
        else return $! value

{-# INLINE parseHeaderContent #-}



parseHeader :: HashMap HeaderName [Header] -> Parser (HashMap HeaderName [Header])
parseHeader acc = do
    hname <- parseHeaderName
    hcontent <- parseHeaderContent

    -- Force evaluation of hcontent in order to be able to be
    -- lazy for the prettyFormat without penality (Header will not be evaluated)
    let !hcontent' = hcontent
    return $! insertWith (<>) hname [Header hname (prettyFormatHeaderContent hcontent')] acc

{-# INLINE parseHeader #-}



manyM :: (a -> Parser a) -> a -> Parser a
manyM parser acc = parser acc >>= \acc' -> manyM parser acc' <|> return acc'
{-# INLINE manyM #-}



parseHeaders :: HashMap HeaderName [Header] -> Parser (HashMap HeaderName [Header])
parseHeaders acc = do
    headers <- manyM parseHeader acc
    next <- P.peekWord8
    if fromMaybe True (isEndOfLine <$> next)
    then return headers
    else skipUnknownStuff >> parseHeaders headers

  where
    skipUnknownStuff = do
        P.skipWhile (not . isEndOfLine)
        P.skipWhile isEndOfLine


getHeaders :: LByteString -> HashMap HeaderName [Header]
getHeaders = fromMaybe mempty . PL.maybeResult . PL.parse (parseHeaders mempty)

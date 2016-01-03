{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           ClassyPrelude                    hiding (foldMap, fromList)
import           Data.Attoparsec.ByteString       as P hiding (takeWhile)
import           Data.Attoparsec.ByteString.Char8 as PC
import           Data.Attoparsec.ByteString.Lazy  as PL
import           Data.ByteString.Base64           as B64 hiding (decode)
import qualified Data.ByteString.Char8            as BC
import qualified Data.Char                        as C
import           Data.HashMap.Strict              (fromList, fromListWith,
                                                   lookupDefault)
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
                | Unknown Text
                deriving (Show, Read, Eq, Ord, Generic, Hashable)

data Header = Header { name :: HeaderName, content :: Text } deriving (Show, Read)

headerMapping :: HashMap ByteString HeaderName
headerMapping = fromList $ [
    ("return-path"   , ReturnPath),
    ("x-original-to" , OriginalTo),
    ("delivered-to"  , DeliveredTo),
    ("received"      , Received),
    ("content-type"  , ContentType),
    ("thread-topic"  , ThreadTopic),
    ("date"          , Date),
    ("subject"       , Subject),
    ("from"          , From),
    ("to"            , To),
    ("cc"            , Cc),
    ("bcc"           , Bcc),
    ("list-id"       , ListID)
    ]

parseHeaderName :: Parser HeaderName
parseHeaderName = do
    val <- BC.map C.toLower <$> PC.takeWhile (PC.notInClass "\r\n:")
    _ <- char ':'
    return $ lookupDefault (Unknown (T.decodeUtf8With T.ignore val)) val headerMapping

{-# INLINE parseHeaderName #-}

parseQEncodedWord :: Parser ByteString
parseQEncodedWord = do
    value <-     (char '_' >> return " ")
             <|> (char '=' >> do
               hex <- P.take 2
               let ret = parseOnly hexadecimal hex
               return $ either (const hex) singleton ret)
             <|> PC.takeTill (PC.inClass "=_")

    if BC.null value
    then mempty
    else return value



prettyFormatHeaderContent :: ByteString -> Text
prettyFormatHeaderContent str = fromMaybe (T.decodeUtf8 str) $ do
    [_,h, charset, encoding, payload, t] <- Re.match rx str []

    return $ T.decodeUtf8 h
           <> (decodeCharset charset . decodeEncoding encoding $ payload)
           <> prettyFormatHeaderContent t

    where
      decodeEncoding en = if BC.map C.toUpper en == "B"
                          then B64.decodeLenient
                          else \str' -> either (const str') concat
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
        else return value

{-# INLINE parseHeaderContent #-}

parseHeader :: Parser Header
parseHeader = do
    header <- parseHeaderName
    value <- prettyFormatHeaderContent <$> parseHeaderContent
    return $ Header header value

{-# INLINE parseHeader #-}

parseHeaders :: Parser (HashMap HeaderName [Header])
parseHeaders = do
    headers <- toHashMap <$> many' parseHeader
    next <- P.peekWord8
    if fromMaybe True (isEndOfLine <$> next)
    then return headers
    else do
       skipUnknownStuff
       newHeaders <- parseHeaders
       return $ unionWith (<>) headers newHeaders

  where
    skipUnknownStuff = do
        P.skipWhile (not . isEndOfLine)
        P.skipWhile isEndOfLine
        return ()

toHashMap :: [Header] -> HashMap HeaderName [Header]
toHashMap headers = fromListWith (<>) [(nam, [h]) | h@(Header nam _) <- headers]

getHeaders :: LByteString -> HashMap HeaderName [Header]
getHeaders = (fromMaybe mempty) . PL.maybeResult . PL.parse parseHeaders

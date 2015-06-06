{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           ClassyPrelude                    hiding (foldMap)
import           Data.Attoparsec.ByteString       as P hiding (takeWhile)
import           Data.Attoparsec.ByteString.Char8 as PC
import           Data.Attoparsec.ByteString.Lazy  as PL
import           Data.ByteString.Base64           as B64 hiding (decode)
import qualified Data.ByteString.Char8            as BC
import qualified Data.Char                        as C
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
                deriving (Show, Read)

data Header = Header HeaderName Text deriving (Show, Read)

parseHeaderName :: Parser HeaderName
parseHeaderName =     ("Return-Path:"   >> return ReturnPath)
                  <|> ("X-Original-To:" >> return OriginalTo)
                  <|> ("Delivered-To:"  >> return DeliveredTo)
                  <|> ("Received:"      >> return Received)
                  <|> ("Content-Type:"  >> return ContentType)
                  <|> ("Thread-Topic:"  >> return ThreadTopic)
                  <|> ("Date:"          >> return Date)
                  <|> ("Subject:"       >> return Subject)
                  <|> ("From:"          >> return From)
                  <|> ("To:"            >> return To)
                  <|> ("Cc:"            >> return Cc)
                  <|> ("Bcc:"           >> return Bcc)
                  <|> ("List-Id:"       >> return ListID)
                  <|> do
                        val <- PC.takeWhile (`onotElem` asString "\r\n:")
                        _ <- char ':'
                        return $ Unknown (T.decodeUtf8With T.ignore val)

parseHeaderValue :: ByteString -> Text
parseHeaderValue str = fromMaybe (T.decodeUtf8 str) $ do
    [_,h, charset, encoding, payload, t] <- Re.match rx str []

    return $ T.decodeUtf8 h
           <> (decodeCharset charset . decodeEncoding encoding $ payload)
           <> parseHeaderValue t

    where
      decodeEncoding en = if BC.map C.toUpper en == "B" then B64.decodeLenient else id
      decodeCharset ch = case BC.map C.toUpper ch of
                          "UTF-8"  -> T.decodeUtf8With T.ignore
                          "UTF-16" -> T.decodeUtf16LEWith T.ignore
                          "UTF-32" -> T.decodeUtf32LEWith T.ignore
                          "ISO-8859-1" -> T.decodeLatin1
                          _ -> T.decodeUtf8With T.ignore
      rx = flip Re.compile [Re.caseless] $ "^(.*?)=\\?"
                                        <> "([^\\?]+)\\?([QB])\\?(.*?)\\?="
                                        <> "(.*)$"

parseHeader :: Parser Header
parseHeader = do
    header <- parseHeaderName
    value <- takeValue
    return $ Header header (parseHeaderValue value)

    where
      takeValue = do
        P.skipWhile isHorizontalSpace
        value <- P.takeTill isEndOfLine
        _ <- string "\r\n" <|> string "\n"
        next <- P.peekWord8
        if fromMaybe False (isHorizontalSpace <$> next)
        then takeValue >>= \after -> return $ value <> " " <> after
        else return value

parseHeaders :: Parser [Header]
parseHeaders = do
    headers <- many' parseHeader
    next <- P.peekWord8
    if fromMaybe True (isEndOfLine <$> next)
    then return headers
    else do
       skipUnknownStuff
       newHeaders <- parseHeaders
       return $ headers <> newHeaders

  where
    skipUnknownStuff = do
        P.skipWhile (not . isEndOfLine)
        P.skipWhile isEndOfLine
        return ()

getHeaders :: LByteString -> [Header]
getHeaders str = fromMaybe [] (PL.maybeResult $ PL.parse parseHeaders str)

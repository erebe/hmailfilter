{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           ClassyPrelude                    hiding (foldMap)
import           Data.Attoparsec.ByteString       as P hiding (takeWhile)
import           Data.Attoparsec.ByteString.Char8 as PC
import           Data.Attoparsec.ByteString.Lazy  as PL
import           Data.ByteString.Base64           as B64 hiding (decode)
import           Data.ByteString.Char8            (split)
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

decode :: ByteString -> Maybe Text
decode str = do
    [_,h,m,t] <- Re.match rx str []

    return $ T.decodeUtf8With T.ignore h
          <> decode' (split '?' m)
          <> T.decodeUtf8With T.ignore t
    where
      rx = Re.compile "^(.*?)=\\?(.*?)\\?=(.*)$" [Re.caseless]
      decode' [charset, encoding, payload] =
        let decrypt = if encoding == "B" then B64.decodeLenient else id in
        let charsetDecode = case charset of
                                "UTF-8"  -> T.decodeUtf8With T.ignore
                                "utf-8"  -> T.decodeUtf8With T.ignore
                                "UTF-16" -> T.decodeUtf16LEWith T.ignore
                                "utf-16" -> T.decodeUtf16LEWith T.ignore
                                "UTF-32" -> T.decodeUtf32LEWith T.ignore
                                "utf-32" -> T.decodeUtf32LEWith T.ignore
                                _ -> T.decodeUtf8With T.ignore
                              in
       charsetDecode . decrypt $ payload

      decode' str = T.decodeUtf8With T.ignore $ ofoldMap id str

parseHeader :: Parser Header
parseHeader = do
    header <- parseHeaderName
    value <- takeValue
    return $ Header header (fromMaybe (T.decodeUtf8 value) (decode value))

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

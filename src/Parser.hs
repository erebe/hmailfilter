{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           ClassyPrelude                    hiding (foldMap)
import           Data.Attoparsec.ByteString       as P hiding (takeWhile)
import           Data.Attoparsec.ByteString.Char8 as PC
import           Data.Attoparsec.ByteString.Lazy  as PL
import           Data.ByteString.Base64           as B64 hiding (decode)
import           Data.ByteString.Char8            (split)
import           Data.List                        (init, tail)
import           Data.Text.Encoding               as T
import           Data.Text.Encoding.Error         as T


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
decode str = if isPrefixOf "=?" str && isSuffixOf "?=" str
             then decode' . tail . init $ split '?' str
             else Nothing

    where
      decode' ["UTF-8", "B", payload] = Just $ T.decodeUtf8With T.ignore (B64.decodeLenient payload)
      decode' ["UTF-16", "B", payload] = Just $ T.decodeUtf16LEWith T.ignore (B64.decodeLenient payload)
      decode' ["UTF-32", "B", payload] = Just $ T.decodeUtf32LEWith T.ignore (B64.decodeLenient payload)
      decode' _ = Nothing

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

{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8 as PC
import           Data.Attoparsec.ByteString       as P hiding (takeWhile)
import           Data.ByteString.Char8            
import           Control.Monad
import           Prelude                          hiding (takeWhile)
import           Data.Monoid

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
                | Unknown ByteString
                deriving (Show, Read)

data Header = Header HeaderName ByteString deriving (Show, Read)

parseHeaderName :: Parser HeaderName
parseHeaderName =     ("Return-Path"   >> return ReturnPath)
                  <|> ("X-Original-To" >> return OriginalTo)
                  <|> ("Delivered-To"  >> return DeliveredTo)
                  <|> ("Received"      >> return Received)
                  <|> ("Content-Type"  >> return ContentType)
                  <|> ("Thread-Topic"  >> return ThreadTopic)
                  <|> ("Date"          >> return Date)
                  <|> ("Subject"       >> return Subject)
                  <|> ("From"          >> return From)
                  <|> ("To"            >> return To)
                  <|> ("CC"            >> return Cc)
                  <|> ("BCC"           >> return Bcc)
                  <|> liftM Unknown (PC.takeWhile (\c -> c /= '\r' && c /= ':'))

parseHeader :: Parser Header
parseHeader = do
    header <- parseHeaderName
    _ <- char ':'
    value <- takeValue
    return $ Header header value

    where
      takeValue = do
        P.skipWhile isHorizontalSpace
        value <- P.takeTill isEndOfLine
        P.skipWhile isEndOfLine
        next <- P.peekWord8'
        if isHorizontalSpace next
        then takeValue >>= \after -> return $ value <> " " <> after
        else return value

parseHeaders :: Parser [Header]
parseHeaders = do
    headers <- many' parseHeader
    isEnd <- atEnd
    if isEnd
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

getHeaders :: ByteString -> [Header]
getHeaders str = either (const []) id (parseOnly parseHeaders str)

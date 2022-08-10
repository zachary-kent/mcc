module Mcc.Lexer.Internal
  ( readBytestring,
    decode,
    firstChar,
    makeType,
    makeBool,
    escapedChar,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import Data.Text.Lazy.Encoding (decodeUtf8)
import Mcc.Ast.Type (Type)
import Mcc.Lexer.Token (Token (..))

-- | Same as 'read', but from lazy 'Text'
readLazyText :: Read a => Text.Lazy.Text -> a
readLazyText = read . Text.Lazy.unpack

-- | Parse a lazy bytestring
readBytestring :: Read a => ByteString -> a
readBytestring = readLazyText . decodeUtf8

-- | Decode a utf8 encoded lazy Bytestring to strict Text
decode :: ByteString -> Text
decode = Text.Lazy.toStrict . decodeUtf8

-- | Get the first character of a utf-8 encoded Bytestring
firstChar :: ByteString -> Int
firstChar bytes = ord $ Text.Lazy.index (decodeUtf8 bytes) 0

-- | Create a type token, given the specified type
makeType :: Type -> a -> Token
makeType = const . Type

-- | Create a bool token, given the specified boolean value
makeBool :: Bool -> a -> Token
makeBool = const . Bool

-- | Get the integer value of an escaped character in the form \nn
escapedChar :: ByteString -> Int
escapedChar = readLazyText . Text.Lazy.drop 1 . decodeUtf8

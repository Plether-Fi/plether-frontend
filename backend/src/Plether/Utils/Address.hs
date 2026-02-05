module Plether.Utils.Address
  ( Address
  , mkAddress
  , toHex
  , fromHex
  , isValidAddress
  , zeroAddress
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

newtype Address = Address ByteString
  deriving stock (Show, Eq)

mkAddress :: Text -> Maybe Address
mkAddress txt
  | isValidAddress txt = Just $ Address $ decodeHex $ stripPrefix txt
  | otherwise = Nothing

toHex :: Address -> Text
toHex (Address bs) = "0x" <> TE.decodeUtf8 (B16.encode bs)

fromHex :: Text -> Maybe Address
fromHex = mkAddress

isValidAddress :: Text -> Bool
isValidAddress txt =
  let stripped = stripPrefix txt
   in T.length stripped == 40 && T.all isHexChar stripped

zeroAddress :: Address
zeroAddress = Address $ BS.replicate 20 0

stripPrefix :: Text -> Text
stripPrefix txt
  | T.isPrefixOf "0x" txt = T.drop 2 txt
  | T.isPrefixOf "0X" txt = T.drop 2 txt
  | otherwise = txt

isHexChar :: Char -> Bool
isHexChar c =
  (c >= '0' && c <= '9')
    || (c >= 'a' && c <= 'f')
    || (c >= 'A' && c <= 'F')

decodeHex :: Text -> ByteString
decodeHex txt = case B16.decode (TE.encodeUtf8 $ T.toLower txt) of
  Right bs -> bs
  Left _ -> BS.empty

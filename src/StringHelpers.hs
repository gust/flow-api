module StringHelpers(lazyByteStringToString) where

import qualified Data.ByteString.Char8 as BCH
import qualified Data.ByteString.Lazy as BL
lazyByteStringToString = BCH.unpack . BL.toStrict

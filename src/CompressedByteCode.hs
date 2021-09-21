module CompressedByteCode where

import qualified Data.ByteString as B
import Control.Monad.ST
import Data.STRef
import Data.Word
import Data.Foldable (foldl1)
import Data.Bits

-- Now we define the 8 bytecodes for our instructions
-- const -> 0
-- boundLookup -> 1
-- freeLookup -> 2
-- abs -> 3
-- app -> 4
-- fix -> 5
-- if -> 6
-- builtin Sum         -> 7
-- builtin Sub         -> 8
-- builtin Prod        -> 9
-- builtin Div         -> 10
-- builtin And         -> 11
-- builtin Or          -> 12
-- builtin Not         -> 13
-- builtin LessThan    -> 14
-- builtin GreaterThan -> 15
-- builtin Equal       -> 16

type ProgCounter s = STRef s Word64

readAddress :: B.ByteString -> ProgCounter s -> ST s Word64
readAddress bs counter = do
    startIndex <- readSTRef counter
    let bytes = map (fromIntegral . B.index bs . fromIntegral . (startIndex +)) [0..7]
    let word = foldl1 (\acc b -> b .|. shift 8 acc) bytes
    return word




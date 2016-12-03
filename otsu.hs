-- otsu algorithm for binarization

import qualified Data.Array.Repa as R
import qualified Data.Map as Map
import Data.Array.Repa (U, D, Z (..), (:.)(..))

histogram :: Array r sh a -> Int -> Map.Map
histogram arr buckets =
    foldP foldFun arr

-- otsu algorithm for binarization

import qualified Data.Array.Repa as R
import Data.Array.Repa (U, D, Z (..), (:.)(..))

binarizeOtsu :: R.DIM2 -> (R.DIM2 -> RGB8) -> R.DIM2 -> RGB8
binarizeOtsu dim f coords = 


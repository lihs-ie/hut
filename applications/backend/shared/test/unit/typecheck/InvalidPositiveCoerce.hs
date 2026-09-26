module InvalidPositiveCoerce where

import Data.Coerce (coerce)
import Numeric.Natural (Natural)
import Shared.Domain.Common.Primitive

invalid :: Natural -> PositiveInteger
invalid = coerce

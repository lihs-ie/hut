module InvalidPositiveRead where

import Shared.Domain.Common.Primitive

invalid :: PositiveInteger
invalid = read "0"

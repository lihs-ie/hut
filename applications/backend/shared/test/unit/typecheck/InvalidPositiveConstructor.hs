module InvalidPositiveConstructor where

import Shared.Domain.Common.Primitive

invalid :: PositiveInteger
invalid = PositiveInteger 0

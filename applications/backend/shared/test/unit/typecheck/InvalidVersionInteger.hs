module InvalidVersionInteger where

import Shared.Infrastructure.Versioning

invalid :: Integer -> Version
invalid = newVersion

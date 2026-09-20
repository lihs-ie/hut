module ValidVersioning where

import Shared.Domain.Common.Primitive
import Shared.Infrastructure.Versioning

version :: Version
version = newVersion one

context :: VersionContext Int
context = recordPersisted 1 version (emptyVersionContext "Test" (const "one"))

module Main (main) where

import Control.Monad (unless)
import Shared.Domain.Common.PrimitiveSpec qualified as PrimitiveSpec
import Shared.Domain.PagerSpec qualified as PagerSpec
import Shared.Domain.CommonSpec qualified as DomainCommonSpec
import Shared.Domain.IdentifierSpec qualified as IdentifierSpec
import Shared.FFI.SecureRandomSpec qualified as SecureRandomSpec
import Shared.UseCase.CommonSpec qualified as UseCaseCommonSpec
import Shared.UseCase.IdentifierSpec qualified as UseCaseIdentifierSpec
import System.Exit (exitFailure)

main :: IO ()
main = do
    results <-
        sequence
            [ DomainCommonSpec.run
            , PrimitiveSpec.run
            , PagerSpec.run
            , IdentifierSpec.run
            , SecureRandomSpec.run
            , UseCaseCommonSpec.run
            , UseCaseIdentifierSpec.run
            ]
    unless (and results) exitFailure

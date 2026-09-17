module Main (main) where

import Control.Monad (unless)
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
            , IdentifierSpec.run
            , SecureRandomSpec.run
            , UseCaseCommonSpec.run
            , UseCaseIdentifierSpec.run
            ]
    unless (and results) exitFailure

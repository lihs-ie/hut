module Main (main) where

import Control.Monad (unless)
import Shared.Domain.Common.PrimitiveSpec qualified as PrimitiveSpec
import Shared.Domain.CommonSpec qualified as DomainCommonSpec
import Shared.Domain.IdentifierSpec qualified as IdentifierSpec
import Shared.Domain.PagerSpec qualified as PagerSpec
import Shared.Domain.TagSpec qualified as TagSpec
import Shared.FFI.SecureRandomSpec qualified as SecureRandomSpec
import Shared.Infrastructure.VersioningSpec qualified as VersioningSpec
import Shared.Transaction.BoundarySpec qualified as TransactionBoundary
import Shared.Transaction.CompositionSpec qualified as TransactionComposition
import Shared.Transaction.FailureSpec qualified as TransactionFailure
import Shared.UseCase.CommonSpec qualified as UseCaseCommonSpec
import Shared.UseCase.IdentifierSpec qualified as UseCaseIdentifierSpec
import System.Exit (exitFailure)

main :: IO ()
main = do
    results <-
        sequence
            [ DomainCommonSpec.run
            , PrimitiveSpec.run
            , VersioningSpec.run
            , PagerSpec.run
            , TransactionComposition.run
            , TransactionFailure.run
            , TransactionBoundary.run
            , IdentifierSpec.run
            , TagSpec.run
            , SecureRandomSpec.run
            , UseCaseCommonSpec.run
            , UseCaseIdentifierSpec.run
            ]
    unless (and results) exitFailure

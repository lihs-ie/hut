module Presentation.Handler.API.Error (
    publicError,
    domainErrorResponse,
) where

import Data.Text (Text)
import Servant.Cloudflare.Workers.Error (ServerError (ServerError))
import Shared.Domain.Error (DomainError (..))

publicError :: Int -> Text -> Text -> ServerError
publicError status code correlation =
    ServerError
        status
        "The article operation could not be completed."
        [ ("X-Correlation-Identifier", correlation)
        , ("X-Article-Error-Code", code)
        ]
        Nothing

domainErrorResponse :: Text -> DomainError -> ServerError
domainErrorResponse correlation errorValue =
    case errorValue of
        InvariantViolation _ -> publicError 400 "invalid_article" correlation
        AggregateNotFound _ -> publicError 404 "article_not_found" correlation
        OperationNotAllowed _ -> publicError 409 "operation_not_allowed" correlation
        ProcessingTargetChanged _ -> publicError 409 "processing_target_changed" correlation
        ServiceUnavailable _ -> publicError 503 "service_unavailable" correlation
        TransactionOutcomeUnknown _ -> publicError 500 "transaction_outcome_unknown" correlation
        UnexpectedError _ -> publicError 500 "unexpected_error" correlation

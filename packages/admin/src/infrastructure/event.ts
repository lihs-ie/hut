import { Event, EventBroker, EventType } from "@shared/domains/common";
import { Topic } from "@google-cloud/pubsub";
import { fromPromise } from "@shared/aspects/result";
import {
  UnexpectedError,
  unexpectedError,
  PermissionDeniedError,
  permissionDeniedError,
  ResourceExhaustedError,
  resourceExhaustedError,
  ServiceUnavailableError,
  serviceUnavailableError,
  AggregateNotFoundError,
  aggregateNotFoundError,
  UnauthenticatedError,
  unauthenticatedError,
} from "@shared/aspects/error";

const GrpcStatus = {
  OK: 0,
  CANCELLED: 1,
  UNKNOWN: 2,
  INVALID_ARGUMENT: 3,
  DEADLINE_EXCEEDED: 4,
  NOT_FOUND: 5,
  ALREADY_EXISTS: 6,
  PERMISSION_DENIED: 7,
  RESOURCE_EXHAUSTED: 8,
  FAILED_PRECONDITION: 9,
  ABORTED: 10,
  OUT_OF_RANGE: 11,
  UNIMPLEMENTED: 12,
  INTERNAL: 13,
  UNAVAILABLE: 14,
  DATA_LOSS: 15,
  UNAUTHENTICATED: 16,
} as const;

type GrpcServiceError = {
  code: number;
  details?: string;
  message?: string;
};

export type PubSubError =
  | UnexpectedError
  | PermissionDeniedError
  | ResourceExhaustedError
  | ServiceUnavailableError
  | AggregateNotFoundError<"Topic">
  | UnauthenticatedError;

const isGrpcServiceError = (error: unknown): error is GrpcServiceError =>
  error !== null &&
  typeof error === "object" &&
  "code" in error &&
  typeof (error as GrpcServiceError).code === "number";

export const mapPubSubError = (error: unknown): PubSubError => {
  if (!isGrpcServiceError(error)) {
    const message =
      error instanceof Error ? error.message : "Unknown PubSub error";
    return unexpectedError(message, error);
  }

  const message = error.details ?? error.message ?? "PubSub operation failed";

  switch (error.code) {
    case GrpcStatus.PERMISSION_DENIED:
      return permissionDeniedError(message);

    case GrpcStatus.UNAUTHENTICATED:
      return unauthenticatedError(message);

    case GrpcStatus.NOT_FOUND:
      return aggregateNotFoundError("Topic", message);

    case GrpcStatus.RESOURCE_EXHAUSTED:
      return resourceExhaustedError(message);

    case GrpcStatus.UNAVAILABLE:
      return serviceUnavailableError(message, true);

    case GrpcStatus.DEADLINE_EXCEEDED:
      return serviceUnavailableError(message, true);

    case GrpcStatus.ABORTED:
      return serviceUnavailableError(message, true);

    case GrpcStatus.CANCELLED:
      return serviceUnavailableError(message, false);

    case GrpcStatus.FAILED_PRECONDITION:
      return unexpectedError(message, error);

    case GrpcStatus.INVALID_ARGUMENT:
      return unexpectedError(message, error);

    case GrpcStatus.OUT_OF_RANGE:
      return unexpectedError(message, error);

    case GrpcStatus.UNIMPLEMENTED:
      return unexpectedError(message, error);

    case GrpcStatus.INTERNAL:
      return unexpectedError(message, error);

    case GrpcStatus.DATA_LOSS:
      return unexpectedError(message, error);

    case GrpcStatus.UNKNOWN:
    default:
      return unexpectedError(message, error);
  }
};

export type Options = {
  topic: string;
};

export const PubSubEventBroker = (topic: Topic): EventBroker => {
  const publish: EventBroker["publish"] = <T extends EventType, P>(
    event: Event<T, P>,
  ) =>
    fromPromise(
      (async () => {
        const serialized = Buffer.from(JSON.stringify(event), "utf8");

        await topic.publishMessage({ data: serialized });
      })(),
      mapPubSubError,
    );

  return {
    publish,
  };
};

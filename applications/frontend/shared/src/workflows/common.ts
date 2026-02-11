import {
  AggregateNotFoundError,
  aggregateNotFoundError,
} from "@shared/aspects/error";
import { ok, err, Result } from "@shared/aspects/result";
import { PublishStatus } from "@shared/domains/common";

export type Command<T> = {
  now: Date;
  payload: T;
};

export const createPassthroughFilter =
  <T, N extends string>() =>
  (entity: T): Result<T, AggregateNotFoundError<N>> =>
    ok(entity);

export const createPublishedOnlyFilter =
  <T extends { status: PublishStatus }, N extends string>(
    aggregateName: N,
  ) =>
  (entity: T): Result<T, AggregateNotFoundError<N>> => {
    if (entity.status !== PublishStatus.PUBLISHED) {
      return err(
        aggregateNotFoundError(
          aggregateName,
          `${aggregateName} is not published`,
        ),
      );
    }
    return ok(entity);
  };

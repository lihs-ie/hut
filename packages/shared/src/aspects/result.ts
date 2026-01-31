export interface Result<T, E> {
  readonly _tag: "Ok" | "Err";
  readonly isOk: boolean;
  readonly isErr: boolean;
  map<U>(fn: (value: T) => U): Result<U, E>;
  mapError<F>(fn: (error: E) => F): Result<T, F>;
  unwrap(): T;
  unwrapOr(defaultValue: T): T;
  unwrapError(): E;
  toAsync(): AsyncResult<T, E>;
  andThen<U, F = E>(fn: (value: T) => Result<U, F>): Result<U, E | F>;
  orElse<F>(fn: (error: E) => Result<T, F>): Result<T, F>;
  match<U>(handlers: { ok: (value: T) => U; err: (error: E) => U }): U;
  tap(fn: (value: T) => void): Result<T, E>;
  tapError(fn: (error: E) => void): Result<T, E>;
}

export interface AsyncResult<T, E> {
  readonly _tag: "AsyncResult";
  map<U>(fn: (value: T) => U | Promise<U>): AsyncResult<U, E>;
  mapError<F>(fn: (error: E) => F | Promise<F>): AsyncResult<T, F>;
  unwrap(): Promise<T>;
  unwrapOr(defaultValue: T): Promise<T>;
  unwrapError(): Promise<E>;
  andThen<U, F = E>(
    fn: (value: T) => Result<U, F> | Promise<Result<U, F>> | AsyncResult<U, F>
  ): AsyncResult<U, E | F>;
  orElse<F>(
    fn: (error: E) => Result<T, F> | Promise<Result<T, F>> | AsyncResult<T, F>
  ): AsyncResult<T, F>;
  match<U>(handlers: {
    ok: (value: T) => U | Promise<U>;
    err: (error: E) => U | Promise<U>;
  }): Promise<U>;
  tap(fn: (value: T) => void | Promise<void>): AsyncResult<T, E>;
  tapError(fn: (error: E) => void | Promise<void>): AsyncResult<T, E>;
}

export type Some<T> = {
  readonly _tag: "Some";
  readonly value: T;
};

export type None = {
  readonly _tag: "None";
};

export type Option<T> = Some<T> | None;

const createAsyncResult = <T, E>(
  promise: Promise<Result<T, E>>
): AsyncResult<T, E> => ({
  _tag: "AsyncResult" as const,

  map<U>(fn: (value: T) => U | Promise<U>): AsyncResult<U, E> {
    return createAsyncResult(
      promise.then(async (result) => {
        if (result.isOk) {
          const newValue = await fn(result.unwrap());
          return ok<U, E>(newValue);
        }
        return err<U, E>(result.unwrapError());
      })
    );
  },

  mapError<F>(fn: (error: E) => F | Promise<F>): AsyncResult<T, F> {
    return createAsyncResult(
      promise.then(async (result) => {
        if (result.isErr) {
          const newError = await fn(result.unwrapError());
          return err<T, F>(newError);
        }
        return ok<T, F>(result.unwrap());
      })
    );
  },

  async unwrap(): Promise<T> {
    const result = await promise;
    return result.unwrap();
  },

  async unwrapOr(defaultValue: T): Promise<T> {
    const result = await promise;
    return result.unwrapOr(defaultValue);
  },

  async unwrapError(): Promise<E> {
    const result = await promise;
    return result.unwrapError();
  },

  andThen<U, F = E>(
    fn: (value: T) => Result<U, F> | Promise<Result<U, F>> | AsyncResult<U, F>
  ): AsyncResult<U, E | F> {
    return createAsyncResult(
      promise.then(async (result) => {
        if (result.isOk) {
          const nextResult = fn(result.unwrap());
          if (isAsyncResult(nextResult)) {
            return nextResult.match({
              ok: (v) => ok<U, E | F>(v),
              err: (e) => err<U, E | F>(e),
            });
          }
          return nextResult as Result<U, E | F>;
        }
        return err<U, E | F>(result.unwrapError());
      })
    );
  },

  orElse<F>(
    fn: (error: E) => Result<T, F> | Promise<Result<T, F>> | AsyncResult<T, F>
  ): AsyncResult<T, F> {
    return createAsyncResult(
      promise.then(async (result) => {
        if (result.isErr) {
          const nextResult = fn(result.unwrapError());
          if (isAsyncResult(nextResult)) {
            return nextResult.match({
              ok: (v) => ok<T, F>(v),
              err: (e) => err<T, F>(e),
            });
          }
          return nextResult;
        }
        return ok<T, F>(result.unwrap());
      })
    );
  },

  async match<U>(handlers: {
    ok: (value: T) => U | Promise<U>;
    err: (error: E) => U | Promise<U>;
  }): Promise<U> {
    const result = await promise;
    if (result.isOk) {
      return handlers.ok(result.unwrap());
    }
    return handlers.err(result.unwrapError());
  },

  tap(fn: (value: T) => void | Promise<void>): AsyncResult<T, E> {
    return createAsyncResult(
      promise.then(async (result) => {
        if (result.isOk) {
          await fn(result.unwrap());
        }
        return result;
      })
    );
  },

  tapError(fn: (error: E) => void | Promise<void>): AsyncResult<T, E> {
    return createAsyncResult(
      promise.then(async (result) => {
        if (result.isErr) {
          await fn(result.unwrapError());
        }
        return result;
      })
    );
  },
});

const createOk = <T, E>(value: T): Result<T, E> => ({
  _tag: "Ok" as const,
  isOk: true as const,
  isErr: false as const,

  map<U>(fn: (value: T) => U): Result<U, E> {
    return createOk(fn(value));
  },

  mapError<F>(_fn: (error: E) => F): Result<T, F> {
    void _fn;
    return createOk(value);
  },

  unwrap(): T {
    return value;
  },

  unwrapOr(_defaultValue: T): T {
    void _defaultValue;
    return value;
  },

  unwrapError(): E {
    throw new Error("Called unwrapError on an Ok value");
  },

  toAsync(): AsyncResult<T, E> {
    return createAsyncResult(Promise.resolve(createOk<T, E>(value)));
  },

  andThen<U, F = E>(fn: (value: T) => Result<U, F>): Result<U, E | F> {
    return fn(value);
  },

  orElse<F>(_fn: (error: E) => Result<T, F>): Result<T, F> {
    void _fn;
    return createOk(value);
  },

  match<U>(handlers: { ok: (value: T) => U; err: (error: E) => U }): U {
    return handlers.ok(value);
  },

  tap(fn: (value: T) => void): Result<T, E> {
    fn(value);
    return createOk(value);
  },

  tapError(_fn: (error: E) => void): Result<T, E> {
    void _fn;
    return createOk(value);
  },
});

const createErr = <T, E>(error: E): Result<T, E> => ({
  _tag: "Err" as const,
  isOk: false as const,
  isErr: true as const,

  map<U>(_fn: (value: T) => U): Result<U, E> {
    void _fn;
    return createErr(error);
  },

  mapError<F>(fn: (error: E) => F): Result<T, F> {
    return createErr(fn(error));
  },

  unwrap(): T {
    throw new Error(`Called unwrap on an Err value: ${JSON.stringify(error)}`);
  },

  unwrapOr(defaultValue: T): T {
    return defaultValue;
  },

  unwrapError(): E {
    return error;
  },

  toAsync(): AsyncResult<T, E> {
    return createAsyncResult(Promise.resolve(createErr<T, E>(error)));
  },

  andThen<U, F = E>(_fn: (value: T) => Result<U, F>): Result<U, E | F> {
    void _fn;
    return createErr(error);
  },

  orElse<F>(fn: (error: E) => Result<T, F>): Result<T, F> {
    return fn(error);
  },

  match<U>(handlers: { ok: (value: T) => U; err: (error: E) => U }): U {
    return handlers.err(error);
  },

  tap(_fn: (value: T) => void): Result<T, E> {
    void _fn;
    return createErr(error);
  },

  tapError(fn: (error: E) => void): Result<T, E> {
    fn(error);
    return createErr(error);
  },
});

export function ok<T, E = never>(value: T): Result<T, E> {
  return createOk(value);
}

export function err<T = never, E = unknown>(error: E): Result<T, E> {
  return createErr(error);
}

export function isAsyncResult<T, E>(
  result: Result<T, E> | AsyncResult<T, E> | Promise<Result<T, E>>
): result is AsyncResult<T, E> {
  return (
    result !== null &&
    typeof result === "object" &&
    "_tag" in result &&
    result._tag === "AsyncResult"
  );
}

export function fromPromise<T, E>(
  promise: Promise<T>,
  errorMapper: (error: unknown) => E
): AsyncResult<T, E> {
  return createAsyncResult(
    promise
      .then((value) => ok<T, E>(value))
      .catch((error) => err<T, E>(errorMapper(error)))
  );
}

export function asyncResult<T, E>(
  promise: Promise<Result<T, E>>
): AsyncResult<T, E> {
  return createAsyncResult(promise);
}

export function fromThrowable<T, A extends unknown[], E>(
  fn: (...args: A) => T,
  errorMapper: (error: unknown) => E
): (...args: A) => Result<T, E> {
  return (...args: A): Result<T, E> => {
    try {
      return ok(fn(...args));
    } catch (error) {
      return err(errorMapper(error));
    }
  };
}

export function fromAsyncThrowable<T, A extends unknown[], E>(
  fn: (...args: A) => Promise<T>,
  errorMapper: (error: unknown) => E
): (...args: A) => AsyncResult<T, E> {
  return (...args: A): AsyncResult<T, E> => {
    return fromPromise(fn(...args), errorMapper);
  };
}

export function combine<T extends readonly Result<unknown, unknown>[]>(
  results: T
): Result<
  { [K in keyof T]: T[K] extends Result<infer U, unknown> ? U : never },
  T[number] extends Result<unknown, infer E> ? E : never
> {
  const values: unknown[] = [];

  for (const result of results) {
    if (result.isErr) {
      return err(result.unwrapError()) as Result<
        { [K in keyof T]: T[K] extends Result<infer U, unknown> ? U : never },
        T[number] extends Result<unknown, infer E> ? E : never
      >;
    }
    values.push(result.unwrap());
  }

  return ok(values) as Result<
    { [K in keyof T]: T[K] extends Result<infer U, unknown> ? U : never },
    T[number] extends Result<unknown, infer E> ? E : never
  >;
}

export function combineAsync<
  T extends readonly AsyncResult<unknown, unknown>[]
>(
  asyncResults: T
): AsyncResult<
  { [K in keyof T]: T[K] extends AsyncResult<infer U, unknown> ? U : never },
  T[number] extends AsyncResult<unknown, infer E> ? E : never
> {
  type CombinedValue = {
    [K in keyof T]: T[K] extends AsyncResult<infer U, unknown> ? U : never;
  };
  type CombinedError = T[number] extends AsyncResult<unknown, infer E>
    ? E
    : never;

  return createAsyncResult(
    Promise.all(
      asyncResults.map((ar) =>
        ar.match({
          ok: (v) => ok<unknown, unknown>(v),
          err: (e) => err<unknown, unknown>(e),
        })
      )
    ).then(
      (results) =>
        combine(results as Result<unknown, unknown>[]) as Result<
          CombinedValue,
          CombinedError
        >
    )
  );
}

export function isOk<T, E>(
  result: Result<T, E>
): result is Result<T, never> & { isOk: true } {
  return result.isOk;
}

export function isErr<T, E>(
  result: Result<T, E>
): result is Result<never, E> & { isErr: true } {
  return result.isErr;
}

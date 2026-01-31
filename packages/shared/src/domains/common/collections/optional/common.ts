class NoSuchElementException extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'NoSuchElementException';
  }
}

type ErrorConstructor<E extends Error, A extends unknown[]> = new (...args: A) => E;

export interface Some<T> {
  isPresent: () => true;
  get: () => T;
}

export const Some = <T>(value: T): Some<T> => ({
  isPresent: () => true,
  get: () => value,
});

export const None = (): None => ({
  isPresent: () => false,
  get: () => {
    throw new NoSuchElementException('No value present');
  },
});

export interface None {
  isPresent: () => false;
  get: () => never;
}

export interface Optional<T> {
  isPresent: () => boolean;
  get: () => T;
  ifPresent: <R>(callback: (value: T) => R) => R;
  ifPresentOrElse: <P, NP>(ifPresent: (value: T) => P, ifNotPresent: () => NP) => P | NP;
  orElse: (alternative: T) => T;
  orElseGet: (supplier: () => T) => T;
  orElseThrow: <E extends Error, A extends unknown[]>(
    error?: ErrorConstructor<E, A>,
    ...args: A
  ) => T;
  or: (supplier: () => Optional<T>) => Optional<T>;
  map: <R>(mapper: (value: T) => R) => Optional<R>;
}

const empty = <T>(): Optional<T> => Optional<T>(undefined);

const nullable = <T>(value?: T | null): Optional<T> => {
  if (value === null || value === undefined) {
    return empty<T>();
  }

  return Optional<T>(value);
};

export const OptionalImplementation = <T>(value?: T): Optional<T> => {
  const inner: Some<T> | None = value !== undefined ? Some(value) : None();

  const isPresent = (): boolean => inner.isPresent();

  const get = (): T => {
    return inner.get();
  };

  const ifPresent = <R>(callback: (value: T) => R): R => {
    if (isPresent()) {
      return callback(inner.get());
    }

    throw new NoSuchElementException('No value present');
  };

  const ifPresentOrElse = <P, NP>(ifPresent: (value: T) => P, ifNotPresent: () => NP): P | NP => {
    if (isPresent()) {
      return ifPresent(inner.get());
    }

    return ifNotPresent();
  };

  const orElse = (alternative: T): T => {
    if (isPresent()) {
      return inner.get();
    }

    return alternative;
  };

  const orElseGet = (supplier: () => T): T => {
    if (isPresent()) {
      return inner.get();
    }

    return supplier();
  };

  const orElseThrow = <E extends Error, A extends unknown[]>(
    error?: ErrorConstructor<E, A>,
    ...args: A
  ): T => {
    if (isPresent()) {
      return inner.get();
    }

    throw error ? new error(...args) : new NoSuchElementException('No value present');
  };

  const or = (supplier: () => Optional<T>): Optional<T> => {
    if (isPresent()) {
      return OptionalImplementation(inner.get());
    }

    return supplier();
  };

  const map = <R>(mapper: (value: T) => R): Optional<R> => {
    if (isPresent()) {
      return OptionalImplementation(mapper(inner.get()));
    }

    return empty<R>();
  };

  return {
    isPresent,
    get,
    ifPresent,
    ifPresentOrElse,
    orElse,
    orElseGet,
    orElseThrow,
    or,
    map,
  };
};

export interface OptionalConstructor {
  <T>(value?: T | null): Optional<T>;
  empty<T>(): Optional<T>;
  nullable<T>(value?: T | null): Optional<T>;
}

export const Optional: OptionalConstructor = Object.assign(OptionalImplementation, {
  empty: <T>(): Optional<T> => OptionalImplementation<T>(undefined),
  nullable,
} as OptionalConstructor);

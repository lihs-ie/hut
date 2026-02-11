import { z } from 'zod';

import { Optional } from '../optional/common';

import type { createConverters } from '../converters';
import type { Hasher } from '../hamt/hash';
import type { ImmutableMap } from '../map/common';
import type { IndexedSequence } from '../sequence/common';
import type { ImmutableSet } from '../set/common';

export interface ImmutableList<T> {
  size: () => number;
  toArray: () => T[];
  toSet: (hasher: Hasher) => ImmutableSet<T>;
  toSeq: () => IndexedSequence<T>;
  toMap(hasher: Hasher): ImmutableMap<number, T>;
  toMap<K>(
    keyMapper: (value: T, index: number) => K,
    hasher: Hasher,
  ): ImmutableMap<K, T>;
  addFirst: (value: T) => ImmutableList<T>;
  addFirstAll: (...values: T[]) => ImmutableList<T>;
  addLast: (value: T) => ImmutableList<T>;
  addLastAll: (...values: T[]) => ImmutableList<T>;
  remove: (value: T) => ImmutableList<T>;
  get: (index: number) => Optional<T>;
  find: (predicate: (value: T, index: number) => boolean) => Optional<T>;
  findIndex: (predicate: (value: T, index: number) => boolean) => number;
  first: () => Optional<T>;
  last: () => Optional<T>;
  map: <R>(mapper: (value: T, index: number) => R) => ImmutableList<R>;
  filter: (predicate: (value: T, index: number) => boolean) => ImmutableList<T>;
  reduce: <R>(callback: (accumulator: R, value: T) => R, initial: R) => R;
  zip: <V>(other: ImmutableList<V>) => ImmutableList<[T, V]>;
  reverse: () => ImmutableList<T>;
  sort: (comparer?: (left: T, right: T) => number) => ImmutableList<T>;
  drop: (count: number) => ImmutableList<T>;
  foreach: (callback: (value: T, index: number) => void) => void;
  isEmpty: () => boolean;
  isNotEmpty: () => boolean;
  equals: (
    comparison: ImmutableList<T>,
    callback?: (left: T, right: T) => boolean,
  ) => boolean;
  exists: (predicate: (value: T, index: number) => boolean) => boolean;
  forall: (predicate: (value: T, index: number) => boolean) => boolean;
  when<R extends ImmutableList<T>>(
    condition: boolean | (() => boolean),
    callback: (self: ImmutableList<T>) => R,
  ): R | ImmutableList<T>;
  readonly __brand: 'ImmutableList';
}

const ImmutableListImpl = <T>(values: T[] = []): ImmutableList<T> => {
  const items = [...values];

  const size = () => items.length;

  const toArray = () => [...items];

  const toSet = (hasher: Hasher): ImmutableSet<T> => {
    // Use the global converters to avoid circular imports
    const converters = (globalThis as Record<string, unknown>)
      .__conversionHelpers as ReturnType<typeof createConverters>;
    return converters.setFactory(hasher)(toArray());
  };

  const toSeq = (): IndexedSequence<T> => {
    const converters = (globalThis as Record<string, unknown>)
      .__conversionHelpers as ReturnType<typeof createConverters>;
    return converters.sequenceFactory(toArray());
  };

  // Overloaded toMap implementation
  function toMap(hasher: Hasher): ImmutableMap<number, T>;
  function toMap<K>(
    keyMapper: (value: T, index: number) => K,
    hasher: Hasher,
  ): ImmutableMap<K, T>;
  function toMap<K>(
    hasherOrKeyMapper: Hasher | ((value: T, index: number) => K),
    hasherParam?: Hasher,
  ): ImmutableMap<number, T> | ImmutableMap<K, T> {
    const converters = (globalThis as Record<string, unknown>)
      .__conversionHelpers as ReturnType<typeof createConverters>;

    if (typeof hasherOrKeyMapper === 'function' && hasherParam) {
      // Key mapper version
      const entries: [K, T][] = toArray().map((item, index) => [
        hasherOrKeyMapper(item, index),
        item,
      ]);
      return converters.mapFactory(hasherParam)(entries);
    } else {
      // Index-based version
      const h = hasherOrKeyMapper as Hasher;
      const entries: [number, T][] = toArray().map((item, index) => [
        index,
        item,
      ]);
      return converters.mapFactory(h)(entries);
    }
  }

  const addFirst = (value: T): ImmutableList<T> =>
    ImmutableListImpl([value, ...items]);

  const addFirstAll = (...values: T[]): ImmutableList<T> =>
    ImmutableListImpl([...values, ...items]);

  const addLast = (value: T): ImmutableList<T> =>
    ImmutableListImpl([...items, value]);

  const addLastAll = (...values: T[]): ImmutableList<T> =>
    ImmutableListImpl([...items, ...values]);

  const remove = (value: T): ImmutableList<T> => {
    const index = items.indexOf(value);

    if (index === -1) {
      return ImmutableListImpl(items);
    }

    return ImmutableListImpl([
      ...items.slice(0, index),
      ...items.slice(index + 1),
    ]);
  };

  const get = (index: number): Optional<T> => Optional(items[index]);

  const find = (
    predicate: (value: T, index: number) => boolean,
  ): Optional<T> => {
    const target = items.find(predicate);

    return Optional(target);
  };

  const findIndex = (
    predicate: (value: T, index: number) => boolean,
  ): number => {
    for (let i = 0; i < items.length; i++) {
      if (predicate(items[i], i)) {
        return i;
      }
    }
    return -1;
  };

  const first = (): Optional<T> => Optional.nullable(items[0]);

  const last = (): Optional<T> => Optional.nullable(items[items.length - 1]);

  const map = <R>(mapper: (value: T, index: number) => R): ImmutableList<R> => {
    const mapped = items.map(mapper);

    return ImmutableListImpl(mapped);
  };

  const filter = (
    predicate: (value: T, index: number) => boolean,
  ): ImmutableList<T> => {
    const filtered = items.filter(predicate);

    return ImmutableListImpl(filtered);
  };

  const reduce = <R>(
    callback: (accumulator: R, value: T) => R,
    initial: R,
  ): R => {
    return items.reduce(callback, initial);
  };

  const zip = <V>(other: ImmutableList<V>): ImmutableList<[T, V]> => {
    const count = Math.min(items.length, other.size());

    const zipped = [...Array<number>(count)].map((_, index): [T, V] => [
      items[index],
      other.get(index).get(),
    ]);

    return ImmutableListImpl(zipped);
  };

  const reverse = (): ImmutableList<T> => {
    const reversed = Array.from(items).reverse();

    return ImmutableListImpl(reversed);
  };

  const sort = (comparer?: (left: T, right: T) => number): ImmutableList<T> => {
    const sorted = Array.from(items).sort(comparer);

    return ImmutableListImpl(sorted);
  };

  const drop = (count: number): ImmutableList<T> => {
    if (count <= 0) {
      return ImmutableListImpl(items);
    }

    const dropped = items.slice(count);

    return ImmutableListImpl(dropped);
  };

  const foreach = (callback: (value: T, index: number) => void): void => {
    items.forEach(callback);
  };

  const isEmpty = () => items.length === 0;

  const isNotEmpty = () => !isEmpty();

  const equals = (
    comparison: ImmutableList<T>,
    callback: (left: T, right: T) => boolean = (left, right) => left === right,
  ): boolean => {
    const selfSize = size();

    if (selfSize !== comparison.size()) {
      return false;
    }

    return zip(comparison)
      .map(([left, right]) => {
        return callback(left, right);
      })
      .filter((value) => !value)
      .isEmpty();
  };

  const exists = (predicate: (value: T, index: number) => boolean): boolean => {
    return items.some(predicate);
  };

  const forall = (predicate: (value: T, index: number) => boolean): boolean => {
    return items.every(predicate);
  };

  const when = <R extends ImmutableList<T>>(
    condition: boolean | (() => boolean),
    callback: (self: ImmutableList<T>) => R,
  ): R | ImmutableList<T> => {
    const conditionResult =
      typeof condition === 'function' ? condition() : condition;
    return conditionResult ? callback(instance) : instance;
  };

  const instance: ImmutableList<T> = {
    size,
    isEmpty,
    isNotEmpty,
    toArray,
    toSet,
    toSeq,
    toMap,
    addFirst,
    addFirstAll,
    addLast,
    addLastAll,
    remove,
    get,
    find,
    findIndex,
    first,
    last,
    map,
    filter,
    reduce,
    zip,
    reverse,
    sort,
    drop,
    foreach,
    equals,
    exists,
    forall,
    when,
    __brand: 'ImmutableList',
  };

  return instance;
};

export interface ImmutableListConstructor {
  <T>(values?: T[]): ImmutableList<T>;
  fromArray<T>(values: T[]): ImmutableList<T>;
  of<T>(...values: T[]): ImmutableList<T>;
  empty<T>(): ImmutableList<T>;
  isList<T>(value: unknown): value is ImmutableList<T>;
}

export const ImmutableList: ImmutableListConstructor = Object.assign(
  ImmutableListImpl,
  {
    fromArray: <T>(values: T[]): ImmutableList<T> => ImmutableListImpl(values),
    of: <T>(...values: T[]): ImmutableList<T> => ImmutableListImpl(values),
    empty: <T>(): ImmutableList<T> => ImmutableListImpl<T>(),
    isList: (value: unknown): value is ImmutableList<unknown> => {
      return (
        typeof value === 'object' &&
        value !== null &&
        typeof (value as Record<string, unknown>).size === 'function' &&
        typeof (value as Record<string, unknown>).toArray === 'function' &&
        typeof (value as Record<string, unknown>).addFirst === 'function' &&
        typeof (value as Record<string, unknown>).addLast === 'function'
      );
    },
  },
) as ImmutableListConstructor;

export function immutableListSchema<T>(
  elementSchema: z.ZodType<T>,
): z.ZodType<ImmutableList<T>> {
  return z.any().superRefine((candidate, context) => {
    if (
      candidate === null ||
      candidate === undefined ||
      typeof candidate !== 'object'
    ) {
      context.addIssue({
        code: 'custom',
        message: 'Expected an object for ImmutableList',
      });

      return;
    }

    if (
      !Object.prototype.hasOwnProperty.call(candidate, '__brand') &&
      (candidate as { __brand: unknown }).__brand !== 'ImmutableList'
    ) {
      context.addIssue({
        code: 'custom',
        message: 'Expected ImmutableList',
      });

      return;
    }

    const values = (candidate as ImmutableList<unknown>).toArray();

    values.forEach((value, index) => {
      const result = elementSchema.safeParse(value);

      if (!result.success) {
        context.addIssue({
          code: 'custom',
          message: `Invalid element at index ${index}: ${result.error.message}`,
        });
      }
    });

    return;
  });
}

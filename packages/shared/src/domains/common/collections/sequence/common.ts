import type { createConverters } from '../converters';
import type { Hasher } from '../hamt/hash';
import type { ImmutableList } from '../list/common';
import type { ImmutableSet } from '../set/common';

export interface IndexedSequence<T> {
  size: () => number;
  toArray: () => T[];
  get: (index: number) => T | undefined;
  map: <R>(mapper: (value: T, index: number) => R) => IndexedSequence<R>;
  filter: (predicate: (value: T, index: number) => boolean) => IndexedSequence<T>;
  first: () => T | undefined;
  last: () => T | undefined;
  foreach: (callback: (value: T, index: number) => void) => void;
  find: (predicate: (value: T, index: number) => boolean) => T | undefined;
  includes: (value: T) => boolean;
  isEmpty: () => boolean;
  isNotEmpty: () => boolean;
  reverse: () => IndexedSequence<T>;
  slice: (begin?: number, end?: number) => IndexedSequence<T>;
  take: (amount: number) => IndexedSequence<T>;
  skip: (amount: number) => IndexedSequence<T>;
  toList: () => ImmutableList<T>;
  toSet: (hasher: Hasher) => ImmutableSet<T>;
}

export const IndexedSequence = <T>(values: T[] = []): IndexedSequence<T> => {
  const items = [...values];

  const size = (): number => items.length;

  const toArray = (): T[] => [...items];

  const get = (index: number): T | undefined => {
    if (index < 0 || index >= items.length) {
      return undefined;
    }
    return items[index];
  };

  const map = <R>(mapper: (value: T, index: number) => R): IndexedSequence<R> => {
    const mapped = items.map((value, index) => mapper(value, index));
    return IndexedSequence(mapped);
  };

  const filter = (predicate: (value: T, index: number) => boolean): IndexedSequence<T> => {
    const filtered = items.filter((value, index) => predicate(value, index));
    return IndexedSequence(filtered);
  };

  const first = (): T | undefined => items[0];

  const last = (): T | undefined => items[items.length - 1];

  const foreach = (callback: (value: T, index: number) => void): void => {
    items.forEach((value, index) => callback(value, index));
  };

  const find = (predicate: (value: T, index: number) => boolean): T | undefined => {
    return items.find((value, index) => predicate(value, index));
  };

  const includes = (value: T): boolean => {
    return items.includes(value);
  };

  const isEmpty = (): boolean => items.length === 0;

  const isNotEmpty = (): boolean => !isEmpty();

  const reverse = (): IndexedSequence<T> => {
    return IndexedSequence([...items].reverse());
  };

  const slice = (begin = 0, end = items.length): IndexedSequence<T> => {
    return IndexedSequence(items.slice(begin, end));
  };

  const take = (amount: number): IndexedSequence<T> => {
    return IndexedSequence(items.slice(0, amount));
  };

  const skip = (amount: number): IndexedSequence<T> => {
    return IndexedSequence(items.slice(amount));
  };

  const toList = (): ImmutableList<T> => {
    const converters = (globalThis as Record<string, unknown>).__conversionHelpers as ReturnType<
      typeof createConverters
    >;
    return converters.listFactory(toArray());
  };

  const toSet = (hasher: Hasher): ImmutableSet<T> => {
    const converters = (globalThis as Record<string, unknown>).__conversionHelpers as ReturnType<
      typeof createConverters
    >;
    return converters.setFactory(hasher)(toArray());
  };

  return {
    size,
    toArray,
    get,
    map,
    filter,
    first,
    last,
    foreach,
    find,
    includes,
    isEmpty,
    isNotEmpty,
    reverse,
    slice,
    take,
    skip,
    toList,
    toSet,
  };
};

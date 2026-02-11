import { createConverters } from '../converters';
import {
  BitmapIndexedNode,
  Hasher as HasherFactory,
  LeafNode,
  type HAMTNode,
  type Hasher,
} from '../hamt';
import { ImmutableList } from '../list';
import { ImmutableMap } from '../map';
import { Optional } from '../optional';
import { IndexedSequence } from '../sequence';

export interface ImmutableSet<K> {
  size: () => number;
  toArray: () => K[];
  toList: () => ImmutableList<K>;
  toSeq: () => IndexedSequence<K>;
  toMap(hasher: Hasher): ImmutableMap<number, K>;
  toMap<V>(keyMapper: (value: K, index: number) => V, hasher: Hasher): ImmutableMap<V, K>;
  isEmpty: () => boolean;
  isNotEmpty: () => boolean;
  add: (key: K) => ImmutableSet<K>;
  addAll: (...keys: K[]) => ImmutableSet<K>;
  remove: (key: K) => ImmutableSet<K>;
  contains: (key: K) => boolean;
  find: (predicate: (key: K) => boolean) => Optional<K>;
  reduce: <R>(callback: (accumulator: R, key: K) => R, initial: R) => R;
  map: <R>(mapper: (key: K) => R) => ImmutableSet<R>;
  filter: (predicate: (key: K) => boolean) => ImmutableSet<K>;
  foreach: (callback: (key: K) => void) => void;
  equals: (other: ImmutableSet<K>) => boolean;
  exists: (predicate: (key: K) => boolean) => boolean;
  subtract: (...iterables: Iterable<K>[]) => ImmutableSet<K>;
}

type Void = undefined;
const voidValue: Void = undefined;

const ImmutableSetImpl =
  (hasher: Hasher) =>
  <K>(root: HAMTNode<K, Void> | null = null): ImmutableSet<K> => {
    const toArray = (): K[] => root?.toArray().map(([key]) => key) ?? [];

    // Use the global converters to avoid circular imports
    const converters = (globalThis as Record<string, unknown>).__conversionHelpers as ReturnType<
      typeof createConverters
    >;
    const toList = (): ImmutableList<K> => {
      return converters.listFactory(toArray());
    };

    const toSeq = (): IndexedSequence<K> => {
      return converters.sequenceFactory(toArray());
    };

    // Overloaded toMap implementation
    function toMap(hasher: Hasher): ImmutableMap<number, K>;
    function toMap<V>(
      keyMapper: (value: K, index: number) => V,
      hasher: Hasher
    ): ImmutableMap<V, K>;
    function toMap<V>(
      hasherOrKeyMapper: Hasher | ((value: K, index: number) => V),
      hasherParam?: Hasher
    ): ImmutableMap<number, K> | ImmutableMap<V, K> {
      if (typeof hasherOrKeyMapper === 'function' && hasherParam) {
        // Key mapper version
        const entries: [V, K][] = toArray().map((item, index) => [
          hasherOrKeyMapper(item, index),
          item,
        ]);
        return converters.mapFactory(hasherParam)(entries);
      } else {
        // Index-based version
        const h = hasherOrKeyMapper as Hasher;
        const entries: [number, K][] = toArray().map((item, index) => [index, item]);
        return converters.mapFactory(h)(entries);
      }
    }

    const size = (): number => toArray().length;

    const isEmpty = (): boolean => size() === 0;

    const isNotEmpty = (): boolean => !isEmpty();

    const add = (key: K): ImmutableSet<K> => {
      const hash = hasher.hash(key);

      if (root === null) {
        return ImmutableSetImpl(hasher)(LeafNode(hash, key, voidValue));
      }

      return ImmutableSetImpl(hasher)(root.add(hash, 0, key, voidValue));
    };

    const addAll = (...keys: K[]): ImmutableSet<K> => {
      const nextRoot = keys.reduce(
        (carry, current): HAMTNode<K, Void> =>
          carry.add(hasher.hash(current), 0, current, voidValue),
        root ?? BitmapIndexedNode<K, Void>()
      );

      return ImmutableSetImpl(hasher)(nextRoot);
    };

    const remove = (key: K): ImmutableSet<K> => {
      const hash = hasher.hash(key);

      if (root === null) {
        return ImmutableSetImpl(hasher)(null);
      }

      return ImmutableSetImpl(hasher)(root.remove(hash, 0));
    };

    const contains = (key: K): boolean => {
      const hash = hasher.hash(key);

      if (root === null) {
        return false;
      }

      return root.contains(hash, 0);
    };

    const find = (predicate: (key: K) => boolean): Optional<K> => {
      return Optional(root?.find(predicate)?.key());
    };

    const reduce = <R>(callback: (accumulator: R, key: K) => R, initial: R): R => {
      return toArray().reduce<R>((carry, key): R => {
        return callback(carry, key);
      }, initial);
    };

    const map = <R>(mapper: (key: K) => R): ImmutableSet<R> => {
      const mapped = root?.toArray().map(([key]) => mapper(key)) ?? [];

      return fromArrayImpl(hasher)(mapped);
    };

    const filter = (predicate: (key: K) => boolean): ImmutableSet<K> => {
      const filtered = toArray().filter(predicate);

      return fromArrayImpl(hasher)(filtered);
    };

    const foreach = (callback: (key: K) => void): void => {
      const items = toArray();

      items.forEach((key): void => callback(key));
    };

    const equals = (comparison: ImmutableSet<K>): boolean => {
      if (size() !== comparison.size()) {
        return false;
      }

      return toArray().every((key): boolean => comparison.contains(key));
    };

    const exists = (predicate: (key: K) => boolean): boolean => {
      return root?.exists(predicate) ?? false;
    };

    const subtract = (...iterables: Iterable<K>[]): ImmutableSet<K> => {
      if (iterables.length === 0) {
        return ImmutableSetImpl(hasher)(root);
      }

      // Convert iterables to arrays for easier processing
      const iterableArrays = iterables.map(iterable => Array.from(iterable));

      // Get elements to remove - those that exist in current set AND in any of the iterables
      const toRemove: K[] = [];
      const currentItems = toArray();

      currentItems.forEach(value => {
        if (iterableArrays.some(iterableArray => iterableArray.includes(value))) {
          toRemove.push(value);
        }
      });

      // Create new set by filtering out the elements to remove
      const remaining = currentItems.filter(value => !toRemove.includes(value));

      return fromArrayImpl(hasher)(remaining);
    };

    return {
      size,
      toArray,
      toList,
      toSeq,
      toMap,
      isEmpty,
      isNotEmpty,
      add,
      addAll,
      remove,
      contains,
      find,
      reduce,
      map,
      filter,
      foreach,
      equals,
      exists,
      subtract,
    };
  };

const fromArrayImpl =
  (hasher: Hasher) =>
  <T>(keys: T[]): ImmutableSet<T> => {
    const root = keys.reduce<HAMTNode<T, Void>>(
      (carry, current): HAMTNode<T, Void> => carry.add(hasher.hash(current), 0, current, voidValue),
      BitmapIndexedNode<T, Void>()
    );

    return ImmutableSetImpl(hasher)(root);
  };

export interface ImmutableSetConstructor {
  (hasher: Hasher): <K>(root?: HAMTNode<K, Void> | null) => ImmutableSet<K>;
  fromArray<T>(items: T[]): ImmutableSet<T>;
  of<T>(...items: T[]): ImmutableSet<T>;
  empty<T>(): ImmutableSet<T>;
  isSet<T>(value: unknown): value is ImmutableSet<T>;
}

export const ImmutableSet: ImmutableSetConstructor = Object.assign(ImmutableSetImpl, {
  fromArray: <T>(items: T[]): ImmutableSet<T> => fromArrayImpl(HasherFactory())(items),
  of: <T>(...items: T[]): ImmutableSet<T> => fromArrayImpl(HasherFactory())(items),
  empty: <T>(): ImmutableSet<T> => ImmutableSetImpl(HasherFactory())<T>(),
  isSet: (value: unknown): value is ImmutableSet<unknown> => {
    return (
      typeof value === 'object' &&
      value !== null &&
      typeof (value as Record<string, unknown>).size === 'function' &&
      typeof (value as Record<string, unknown>).toArray === 'function' &&
      typeof (value as Record<string, unknown>).add === 'function' &&
      typeof (value as Record<string, unknown>).contains === 'function'
    );
  },
}) as ImmutableSetConstructor;

export const fromArray = fromArrayImpl;

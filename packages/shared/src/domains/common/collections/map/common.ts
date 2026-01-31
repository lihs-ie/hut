import { z } from 'zod';

import {
  BitmapIndexedNode,
  Hasher as HasherConstructor,
  LeafNode,
  type HAMTNode,
  type Hasher,
} from '../hamt';
import { ImmutableList } from '../list';
import { Optional } from '../optional/common';
import { IndexedSequence } from '../sequence';
import { ImmutableSet, SetFromArray } from '../set';

type ObjectKey = string | number | symbol;

export interface ImmutableMap<K, V> {
  toArray: () => [K, V][];
  toObject: () => Record<string, V>;
  toList: () => ImmutableList<V>;
  toSet: () => ImmutableSet<K>;
  add: (key: K, value: V) => ImmutableMap<K, V>;
  remove: (key: K) => ImmutableMap<K, V>;
  get: (key: K) => Optional<V>;
  find: (predicate: (key: K, value: V) => boolean) => Optional<V>;
  reduce: <R>(callback: (accumulator: R, key: K, value: V) => R, initial: R) => R;
  keys: () => K[];
  keySeq: () => IndexedSequence<K>;
  values: () => V[];
  contains: (key: K) => boolean;
  size: () => number;
  isEmpty: () => boolean;
  isNotEmpty: () => boolean;
  foreach: (callback: (key: K, value: V, index: number) => void) => void;
  exists: (predicate: (key: K, value: V) => boolean) => boolean;
  equals: (comparison: ImmutableMap<K, V>, callback?: (left: V, right: V) => boolean) => boolean;
  map: <RK, RV>(mapper: (key: K, value: V) => [RK, RV]) => ImmutableMap<RK, RV>;
  mapKeys: <RK>(mapper: (key: K) => RK) => ImmutableMap<RK, V>;
  mapValues: <RV>(mapper: (value: V) => RV) => ImmutableMap<K, RV>;
  filter: (predicate: (key: K, value: V) => boolean) => ImmutableMap<K, V>;
  readonly __brand: 'ImmutableMap';
}

const isObjectKey = (key: unknown): key is ObjectKey => {
  return typeof key === 'string' || typeof key === 'number' || typeof key === 'symbol';
};

const ImmutableMapImpl =
  (hasher: Hasher) =>
  <K, V>(root: HAMTNode<K, V> | null = null): ImmutableMap<K, V> => {
    const toArray = (): [K, V][] => root?.toArray() ?? [];

    const toObject = (): Record<string, V> => {
      return reduce(
        (carry, key, value) => {
          if (isObjectKey(key)) {
            carry[key.toString()] = value;
          } else {
            carry[JSON.stringify(key)] = value;
          }
          return carry;
        },
        {} as Record<string, V>
      );
    };

    const toList = (): ImmutableList<V> => {
      return ImmutableList(values());
    };

    const toSet = (): ImmutableSet<K> => {
      return SetFromArray(hasher)(keys());
    };

    const size = (): number => toArray().length;

    const isEmpty = (): boolean => size() === 0;

    const isNotEmpty = (): boolean => !isEmpty();

    const add = (key: K, value: V): ImmutableMap<K, V> => {
      const hash = hasher.hash(key);

      if (root === null) {
        return ImmutableMapImpl(hasher)(LeafNode(hash, key, value));
      }

      return ImmutableMapImpl(hasher)(root.add(hash, 0, key, value));
    };

    const remove = (key: K): ImmutableMap<K, V> =>
      ImmutableMapImpl(hasher)(root?.remove(hasher.hash(key), 0));

    const get = (key: K): Optional<V> => {
      const hash = hasher.hash(key);

      return Optional<V>(root?.get(hash, 0));
    };

    const reduce = <R>(callback: (accumulator: R, key: K, value: V) => R, initial: R): R => {
      return toArray().reduce<R>((carry, [key, value]): R => {
        return callback(carry, key, value);
      }, initial);
    };

    const keys = (): K[] => toArray().map(([key]) => key);

    // Returns IndexedSequence of keys for immutable-js API compatibility
    const keySeq = (): IndexedSequence<K> => IndexedSequence(keys());

    const values = (): V[] => toArray().map(([, value]) => value);

    const find = (predicate: (key: K, value: V) => boolean): Optional<V> => {
      return Optional(root?.find(predicate)?.value());
    };

    const contains = (key: K): boolean => {
      const hash = hasher.hash(key);

      return root?.contains(hash, 0) === true;
    };

    const foreach = (callback: (key: K, value: V, index: number) => void): void => {
      const items = toArray();

      items.forEach(([key, value], index): void => callback(key, value, index));
    };

    const exists = (predicate: (key: K, value: V) => boolean): boolean => {
      return root?.exists(predicate) === true;
    };

    const equals = (
      comparison: ImmutableMap<K, V>,
      callback: (left: V, right: V) => boolean = (left, right) => left === right
    ): boolean => {
      if (size() !== comparison.size()) {
        return false;
      }

      const selfItems = toArray();

      return selfItems.every(([key, value]): boolean => {
        if (!comparison.contains(key)) {
          return false;
        }

        const comparisonValue = comparison.get(key).get();

        return callback(value, comparisonValue);
      });
    };

    const map = <RK, RV>(mapper: (key: K, value: V) => [RK, RV]): ImmutableMap<RK, RV> => {
      const mapped = toArray().map(([key, value]): [RK, RV] => mapper(key, value));

      return fromArrayImpl(hasher)(mapped);
    };

    const mapKeys = <RK>(mapper: (key: K) => RK): ImmutableMap<RK, V> => {
      const mapped = toArray().map(([key, value]): [RK, V] => [mapper(key), value]);

      return fromArrayImpl(hasher)(mapped);
    };

    const mapValues = <RV>(mapper: (value: V) => RV): ImmutableMap<K, RV> => {
      const mapped = toArray().map(([key, value]): [K, RV] => [key, mapper(value)]);

      return fromArrayImpl(hasher)(mapped);
    };

    const filter = (predicate: (key: K, value: V) => boolean): ImmutableMap<K, V> => {
      const filtered = toArray().filter(([key, value]): boolean => predicate(key, value));

      return fromArrayImpl(hasher)(filtered);
    };

    return {
      toArray,
      toObject,
      toList,
      toSet,
      add,
      remove,
      get,
      find,
      reduce,
      keys,
      keySeq,
      values,
      contains,
      size,
      isEmpty,
      isNotEmpty,
      foreach,
      exists,
      equals,
      map,
      mapKeys,
      mapValues,
      filter,
      __brand: 'ImmutableMap',
    };
  };

const fromArrayImpl =
  (hasher: Hasher) =>
  <K, V>(items: [K, V][]): ImmutableMap<K, V> => {
    const root = items.reduce<HAMTNode<K, V>>((carry, [key, value]) => {
      const hash = hasher.hash(key);

      return carry.add(hash, 0, key, value);
    }, BitmapIndexedNode<K, V>());

    return ImmutableMapImpl(hasher)(root);
  };

const fromObjectImpl =
  (hasher: Hasher) =>
  <K extends ObjectKey, V>(items: Record<K, V>): ImmutableMap<K, V> => {
    const root = Object.entries<V>(items).reduce<HAMTNode<K, V>>((carry, [key, value]) => {
      const hash = hasher.hash(key);

      return carry.add(hash, 0, key as K, value);
    }, BitmapIndexedNode<K, V>());

    return ImmutableMapImpl(hasher)(root);
  };

export interface ImmutableMapConstructor {
  (hasher: Hasher): <K, V>(root?: HAMTNode<K, V> | null) => ImmutableMap<K, V>;
  // fromArray overloads - both signatures adjacent
  fromArray<K, V>(items: [K, V][]): ImmutableMap<K, V>;
  fromArray<K, V>(hasher: Hasher): (items: [K, V][]) => ImmutableMap<K, V>;
  // fromObject overloads - both signatures adjacent
  fromObject<K extends ObjectKey, V>(items: Record<K, V>): ImmutableMap<K, V>;
  fromObject<K extends ObjectKey, V>(hasher: Hasher): (items: Record<K, V>) => ImmutableMap<K, V>;
  // empty overloads - both signatures adjacent
  empty<K, V>(): ImmutableMap<K, V>;
  empty<K, V>(hasher: Hasher): () => ImmutableMap<K, V>;
}

// Create default hasher instance for convenience methods
const defaultHasher = HasherConstructor();

// Store original implementations to avoid circular reference
const originalFromArrayImpl = fromArrayImpl;
const originalFromObjectImpl = fromObjectImpl;

// Create overloaded functions using function overload syntax
function fromArrayOverload<K, V>(items: [K, V][]): ImmutableMap<K, V>;
function fromArrayOverload<K, V>(hasher: Hasher): (items: [K, V][]) => ImmutableMap<K, V>;
function fromArrayOverload<K, V>(
  arg: [K, V][] | Hasher
): ImmutableMap<K, V> | ((items: [K, V][]) => ImmutableMap<K, V>) {
  if (Array.isArray(arg)) {
    // Direct array input - use default hasher
    return originalFromArrayImpl(defaultHasher)(arg);
  } else {
    // Original hasher-based version - return curried function
    return originalFromArrayImpl(arg);
  }
}

function fromObjectOverload<K extends ObjectKey, V>(items: Record<K, V>): ImmutableMap<K, V>;
function fromObjectOverload<K extends ObjectKey, V>(
  hasher: Hasher
): (items: Record<K, V>) => ImmutableMap<K, V>;
function fromObjectOverload<K extends ObjectKey, V>(
  arg: Record<K, V> | Hasher
): ImmutableMap<K, V> | ((items: Record<K, V>) => ImmutableMap<K, V>) {
  if (
    typeof arg === 'object' &&
    arg !== null &&
    !Array.isArray(arg) &&
    typeof (arg as Hasher).hash !== 'function'
  ) {
    // Direct object input - use default hasher
    return originalFromObjectImpl(defaultHasher)(arg as Record<K, V>);
  } else {
    // Original hasher-based version - return curried function
    return originalFromObjectImpl(arg as Hasher);
  }
}

function emptyOverload<K, V>(): ImmutableMap<K, V>;
function emptyOverload<K, V>(hasher: Hasher): () => ImmutableMap<K, V>;
function emptyOverload<K, V>(hasher?: Hasher): ImmutableMap<K, V> | (() => ImmutableMap<K, V>) {
  if (hasher === undefined) {
    // No arguments - use default hasher
    return ImmutableMapImpl(defaultHasher)<K, V>();
  } else {
    // Original hasher-based version - return curried function
    return () => ImmutableMapImpl(hasher)<K, V>();
  }
}

export const ImmutableMap: ImmutableMapConstructor = Object.assign(ImmutableMapImpl, {
  // Use overloaded functions that support both patterns
  fromArray: fromArrayOverload,
  fromObject: fromObjectOverload,
  empty: emptyOverload,
});

export const fromArray = fromArrayImpl;
export const fromObject = fromObjectImpl;

export function immutableMapSchema<K, V>(
  keySchema: z.ZodType<K>,
  valueSchema: z.ZodType<V>
): z.ZodType<ImmutableMap<K, V>> {
  return z.any().superRefine((candidate, context) => {
    if (candidate === null || candidate === undefined || typeof candidate !== 'object') {
      context.addIssue({
        code: 'custom',
        message: 'Value must be an object',
      });
      return;
    }

    if (
      !Object.prototype.hasOwnProperty.call(candidate, '__brand') &&
      (candidate as { __brand: string }).__brand !== 'ImmutableMap'
    ) {
      context.addIssue({
        code: 'custom',
        message: 'Value must be an ImmutableMap',
      });
      return;
    }

    (candidate as ImmutableMap<unknown, unknown>).foreach((key, value, index) => {
      const keyValid = keySchema.safeParse(key);
      const valueValid = valueSchema.safeParse(value);

      if (!keyValid.success) {
        context.addIssue({
          code: 'custom',
          message: `Invalid key at index ${index}: ${keyValid.error.message}`,
        });
      }

      if (!valueValid.success) {
        context.addIssue({
          code: 'custom',
          message: `Invalid value at index ${index}: ${valueValid.error.message}`,
        });
      }
    });

    if (context.issues.length > 0) {
      return;
    }

    return;
  });
}

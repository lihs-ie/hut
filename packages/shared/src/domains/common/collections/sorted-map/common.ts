import { Hasher as HasherConstructor } from '../hamt';
import { ImmutableList } from '../list';
import { type ImmutableMap } from '../map/common';
import { Optional } from '../optional/common';
import { IndexedSequence } from '../sequence';
import { ImmutableSet, SetFromArray } from '../set';
import {
  type Comparator,
  type RBTreeNode,
  calculateSize,
  insertNode,
  buildTreeFromArray,
  buildTreeFromObject,
  collectInOrder,
} from '../tree/red-black';

type ObjectKey = string | number | symbol;

// SortedImmutableMap interface (extends ImmutableMap functionality)
export interface SortedImmutableMap<K, V> extends ImmutableMap<K, V> {
  // Override methods to return SortedImmutableMap instead of ImmutableMap
  add: (key: K, value: V) => SortedImmutableMap<K, V>;
  remove: (key: K) => SortedImmutableMap<K, V>;
  equals: (
    comparison: SortedImmutableMap<K, V>,
    callback?: (left: V, right: V) => boolean
  ) => boolean;
  map: <RK, RV>(mapper: (key: K, value: V) => [RK, RV]) => SortedImmutableMap<RK, RV>;
  mapKeys: <RK>(mapper: (key: K) => RK) => SortedImmutableMap<RK, V>;
  mapValues: <RV>(mapper: (value: V) => RV) => SortedImmutableMap<K, RV>;
  filter: (predicate: (key: K, value: V) => boolean) => SortedImmutableMap<K, V>;
}

// Default comparator for common types
const createDefaultComparator = <K>(): Comparator<K> => {
  return (left: K, right: K): number => {
    if (left < right) return -1;
    if (left > right) return 1;
    return 0;
  };
};

// SortedImmutableMap constructor interface
export interface SortedImmutableMapConstructor {
  // Empty map factory methods
  empty<K, V>(): SortedImmutableMap<K, V>;
  empty<K, V>(comparator: Comparator<K>): () => SortedImmutableMap<K, V>;
  // fromArray overloads - both signatures adjacent
  fromArray<K, V>(items: [K, V][]): SortedImmutableMap<K, V>;
  fromArray<K, V>(comparator: Comparator<K>): (items: [K, V][]) => SortedImmutableMap<K, V>;
  // fromObject overloads - both signatures adjacent
  fromObject<K extends ObjectKey, V>(items: Record<K, V>): SortedImmutableMap<K, V>;
  fromObject<K extends ObjectKey, V>(
    comparator: Comparator<K>
  ): (items: Record<K, V>) => SortedImmutableMap<K, V>;
}

// Export Comparator type for external use
export type { Comparator };

// Helper function to find value in tree
const findInTree = <K, V>(
  node: RBTreeNode<K, V> | null,
  key: K,
  comparator: Comparator<K>
): V | undefined => {
  if (node === null) return undefined;

  const compareResult = comparator(key, node.key);
  if (compareResult < 0) {
    return findInTree(node.left, key, comparator);
  } else if (compareResult > 0) {
    return findInTree(node.right, key, comparator);
  } else {
    return node.value;
  }
};

// Main implementation
const SortedImmutableMapImpl = <K, V>(
  comparator: Comparator<K>,
  root: RBTreeNode<K, V> | null = null
): SortedImmutableMap<K, V> => {
  const isEmpty = (): boolean => root === null;
  const size = (): number => calculateSize(root);

  // Implementation using in-order traversal to maintain sorted order
  const toArray = (): [K, V][] => collectInOrder(root);

  const toObject = (): Record<string, V> => {
    return reduce(
      (carry, key, value) => {
        if (typeof key === 'string' || typeof key === 'number' || typeof key === 'symbol') {
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
    const hasher = HasherConstructor();
    return SetFromArray(hasher)(keys());
  };

  const add = (key: K, value: V): SortedImmutableMap<K, V> => {
    const newRoot = insertNode(root, key, value, comparator);
    return SortedImmutableMapImpl(comparator, newRoot);
  };

  const remove = (_key: K): SortedImmutableMap<K, V> => {
    // TODO: Implement remove operation for Red-Black Tree
    return SortedImmutableMapImpl(comparator, root);
  };

  const get = (key: K): Optional<V> => {
    const result = findInTree(root, key, comparator);
    return Optional<V>(result);
  };

  const find = (predicate: (key: K, value: V) => boolean): Optional<V> => {
    const items = toArray();
    for (const [key, value] of items) {
      if (predicate(key, value)) {
        return Optional<V>(value);
      }
    }
    return Optional<V>(undefined);
  };

  const reduce = <R>(callback: (accumulator: R, key: K, value: V) => R, initial: R): R => {
    return toArray().reduce<R>((carry, [key, value]): R => {
      return callback(carry, key, value);
    }, initial);
  };

  const keys = (): K[] => toArray().map(([key]) => key);
  const keySeq = (): IndexedSequence<K> => IndexedSequence(keys());
  const values = (): V[] => toArray().map(([, value]) => value);

  const contains = (key: K): boolean => {
    return findInTree(root, key, comparator) !== undefined;
  };

  const isNotEmpty = (): boolean => !isEmpty();

  const foreach = (callback: (key: K, value: V, index: number) => void): void => {
    const items = toArray();
    items.forEach(([key, value], index): void => callback(key, value, index));
  };

  const exists = (predicate: (key: K, value: V) => boolean): boolean => {
    const items = toArray();
    return items.some(([key, value]) => predicate(key, value));
  };

  const equals = (
    comparison: SortedImmutableMap<K, V>,
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

  const map = <RK, RV>(mapper: (key: K, value: V) => [RK, RV]): SortedImmutableMap<RK, RV> => {
    const mapped = toArray().map(([key, value]): [RK, RV] => mapper(key, value));

    return SortedImmutableMapImpl(
      createDefaultComparator<RK>(),
      buildTreeFromArray(mapped, createDefaultComparator<RK>())
    );
  };

  const mapKeys = <RK>(mapper: (key: K) => RK): SortedImmutableMap<RK, V> => {
    const mapped = toArray().map(([key, value]): [RK, V] => [mapper(key), value]);

    return SortedImmutableMapImpl(
      createDefaultComparator<RK>(),
      buildTreeFromArray(mapped, createDefaultComparator<RK>())
    );
  };

  const mapValues = <RV>(mapper: (value: V) => RV): SortedImmutableMap<K, RV> => {
    const mapped = toArray().map(([key, value]): [K, RV] => [key, mapper(value)]);

    return SortedImmutableMapImpl(comparator, buildTreeFromArray(mapped, comparator));
  };

  const filter = (predicate: (key: K, value: V) => boolean): SortedImmutableMap<K, V> => {
    const filtered = toArray().filter(([key, value]): boolean => predicate(key, value));

    return SortedImmutableMapImpl(comparator, buildTreeFromArray(filtered, comparator));
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

// Factory method implementations
function emptyOverload<K, V>(): SortedImmutableMap<K, V>;
function emptyOverload<K, V>(comparator: Comparator<K>): () => SortedImmutableMap<K, V>;
function emptyOverload<K, V>(
  comparator?: Comparator<K>
): SortedImmutableMap<K, V> | (() => SortedImmutableMap<K, V>) {
  if (comparator === undefined) {
    // Use default comparator
    return SortedImmutableMapImpl(createDefaultComparator<K>(), null);
  } else {
    // Return curried function
    return () => SortedImmutableMapImpl(comparator, null);
  }
}

// Placeholder implementations for fromArray and fromObject
function fromArrayOverload<K, V>(items: [K, V][]): SortedImmutableMap<K, V>;
function fromArrayOverload<K, V>(
  comparator: Comparator<K>
): (items: [K, V][]) => SortedImmutableMap<K, V>;
function fromArrayOverload<K, V>(
  arg: [K, V][] | Comparator<K>
): SortedImmutableMap<K, V> | ((items: [K, V][]) => SortedImmutableMap<K, V>) {
  if (Array.isArray(arg)) {
    // Direct array input - use default comparator
    const items = arg;
    const comparator = createDefaultComparator<K>();
    const root = buildTreeFromArray(items, comparator);
    return SortedImmutableMapImpl(comparator, root);
  } else {
    // Comparator input - return curried function
    const comparator = arg;
    return (items: [K, V][]) => {
      const root = buildTreeFromArray(items, comparator);
      return SortedImmutableMapImpl(comparator, root);
    };
  }
}

function fromObjectOverload<K extends ObjectKey, V>(items: Record<K, V>): SortedImmutableMap<K, V>;
function fromObjectOverload<K extends ObjectKey, V>(
  comparator: Comparator<K>
): (items: Record<K, V>) => SortedImmutableMap<K, V>;
function fromObjectOverload<K extends ObjectKey, V>(
  arg: Record<K, V> | Comparator<K>
): SortedImmutableMap<K, V> | ((items: Record<K, V>) => SortedImmutableMap<K, V>) {
  if (typeof arg === 'function') {
    // Comparator input - return curried function
    const comparator = arg;
    return (items: Record<K, V>) => {
      const root = buildTreeFromObject(items, comparator);
      return SortedImmutableMapImpl(comparator, root);
    };
  } else {
    // Direct object input - use default comparator
    const items = arg;
    const comparator = createDefaultComparator<K>();
    const root = buildTreeFromObject(items, comparator);
    return SortedImmutableMapImpl(comparator, root);
  }
}

export const SortedImmutableMap: SortedImmutableMapConstructor = {
  empty: emptyOverload,
  fromArray: fromArrayOverload,
  fromObject: fromObjectOverload,
};

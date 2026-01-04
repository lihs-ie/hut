/**
 * Collection conversion utilities to avoid circular dependencies
 * Provides conversion functions between different collection types
 */

import { type Hasher } from './hamt';
import { type ImmutableList } from './list/common';
import { type ImmutableMap } from './map/common';
import { type IndexedSequence } from './sequence/common';
import { type ImmutableSet } from './set/common';

// Forward declarations to avoid circular imports
type ListFactory = <T>(values?: T[]) => ImmutableList<T>;
type SetFactory = (hasher: Hasher) => <T>(values: T[]) => ImmutableSet<T>;
type MapFactory = (hasher: Hasher) => <K, V>(entries: [K, V][]) => ImmutableMap<K, V>;
type SequenceFactory = <T>(values?: T[]) => IndexedSequence<T>;

// Conversion functions
export const createConverters = (
  listFactory: ListFactory,
  setFactory: SetFactory,
  mapFactory: MapFactory,
  sequenceFactory: SequenceFactory
) => {
  const listToSet =
    <T>(hasher: Hasher) =>
    (list: ImmutableList<T>): ImmutableSet<T> => {
      return setFactory(hasher)(list.toArray());
    };

  const listToMap =
    <T>(hasher: Hasher) =>
    (list: ImmutableList<T>): ImmutableMap<number, T> => {
      const entries: [number, T][] = list.toArray().map((item, index) => [index, item]);
      return mapFactory(hasher)(entries);
    };

  const listToMapWithKeyMapper =
    <T, K>(hasher: Hasher, keyMapper: (value: T, index: number) => K) =>
    (list: ImmutableList<T>): ImmutableMap<K, T> => {
      const entries: [K, T][] = list.toArray().map((item, index) => [keyMapper(item, index), item]);
      return mapFactory(hasher)(entries);
    };

  const setToList = <T>(set: ImmutableSet<T>): ImmutableList<T> => {
    return listFactory(set.toArray());
  };

  const setToMap =
    <T>(hasher: Hasher) =>
    (set: ImmutableSet<T>): ImmutableMap<number, T> => {
      const entries: [number, T][] = set.toArray().map((item, index) => [index, item]);
      return mapFactory(hasher)(entries);
    };

  const setToMapWithKeyMapper =
    <T, K>(hasher: Hasher, keyMapper: (value: T, index: number) => K) =>
    (set: ImmutableSet<T>): ImmutableMap<K, T> => {
      const entries: [K, T][] = set.toArray().map((item, index) => [keyMapper(item, index), item]);
      return mapFactory(hasher)(entries);
    };

  const listToSeq = <T>(list: ImmutableList<T>): IndexedSequence<T> => {
    return sequenceFactory(list.toArray());
  };

  const setToSeq = <T>(set: ImmutableSet<T>): IndexedSequence<T> => {
    return sequenceFactory(set.toArray());
  };

  const mapToList = <K, V>(map: ImmutableMap<K, V>): ImmutableList<V> => {
    return listFactory(map.values());
  };

  const mapToSet =
    <K, V>(hasher: Hasher) =>
    (map: ImmutableMap<K, V>): ImmutableSet<K> => {
      return setFactory(hasher)(map.keys());
    };

  return {
    listToSet,
    listToMap,
    listToMapWithKeyMapper,
    listToSeq,
    setToList,
    setToMap,
    setToMapWithKeyMapper,
    setToSeq,
    mapToList,
    mapToSet,
    // Export factories for direct use
    listFactory,
    setFactory,
    mapFactory,
    sequenceFactory,
  };
};

import { ImmutableList } from '../list/common';
import { Optional } from '../optional/common';
import { ImmutableSet } from '../set/common';

import type { Hasher } from '../hamt';

// ImmutableRange interface
export interface ImmutableRange {
  // Core methods
  size: () => number;
  isEmpty: () => boolean;
  isNotEmpty: () => boolean;

  // Access methods
  get: (index: number) => Optional<number>;
  first: () => Optional<number>;
  last: () => Optional<number>;
  indexOf: (value: number) => number;

  // Conversion methods
  toArray: () => number[];
  toList: () => ImmutableList<number>;
  toSet: (hasher: Hasher) => ImmutableSet<number>;

  // Slice operations
  slice: (begin?: number, end?: number) => ImmutableRange;

  // Functional operations
  map: <R>(mapper: (value: number, index: number) => R) => ImmutableList<R>;
  filter: (predicate: (value: number, index: number) => boolean) => ImmutableList<number>;
  reduce: <R>(callback: (accumulator: R, value: number) => R, initial: R) => R;
}

// ImmutableRange implementation
const ImmutableRangeImpl = (start: number, end: number, step = 1): ImmutableRange => {
  // Validation
  if (step === 0) {
    throw new Error('Cannot step a Range by 0');
  }
  if (start === undefined) {
    throw new Error('You must define a start value when using Range');
  }
  if (end === undefined) {
    throw new Error('You must define an end value when using Range');
  }

  const _start = start;
  const _end = end;
  const _step = step;

  const size = (): number => {
    if (_start === _end) return 0;

    const stepValue = Math.abs(_step);
    const range = Math.abs(_end - _start);
    return Math.max(0, Math.ceil(range / stepValue));
  };

  const isEmpty = (): boolean => size() === 0;

  const isNotEmpty = (): boolean => !isEmpty();

  const get = (index: number): Optional<number> => {
    if (index < 0 || index >= size()) {
      return Optional<number>(undefined);
    }

    const actualStep = _end < _start ? -Math.abs(_step) : Math.abs(_step);
    const value = _start + index * actualStep;
    return Optional(value);
  };

  const first = (): Optional<number> => get(0);

  const last = (): Optional<number> => {
    const lastIndex = size() - 1;
    return lastIndex >= 0 ? get(lastIndex) : Optional<number>(undefined);
  };

  const indexOf = (value: number): number => {
    if (isEmpty()) return -1;

    const actualStep = _end < _start ? -Math.abs(_step) : Math.abs(_step);
    const offsetValue = value - _start;

    // Check if value can be reached by stepping
    if (actualStep !== 0 && offsetValue % actualStep === 0) {
      const index = offsetValue / actualStep;
      if (index >= 0 && index < size() && Math.floor(index) === index) {
        // Additional range check
        const isInRange =
          _end > _start ? value >= _start && value < _end : value <= _start && value > _end;
        if (isInRange) {
          return index;
        }
      }
    }
    return -1;
  };

  const toArray = (): number[] => {
    const result: number[] = [];
    const rangeSize = size();

    for (let i = 0; i < rangeSize; i++) {
      const valueOpt = get(i);
      if (valueOpt.isPresent()) {
        result.push(valueOpt.get());
      }
    }
    return result;
  };

  const toList = (): ImmutableList<number> => {
    return ImmutableList.fromArray(toArray());
  };

  const toSet = (hasher: Hasher): ImmutableSet<number> => {
    const array = toArray();
    return ImmutableSet(hasher)().addAll(...array) as ImmutableSet<number>;
  };

  const slice = (begin = 0, end?: number): ImmutableRange => {
    const rangeSize = size();
    const actualBegin = begin < 0 ? Math.max(0, rangeSize + begin) : Math.min(begin, rangeSize);
    const actualEnd =
      end === undefined
        ? rangeSize
        : end < 0
          ? Math.max(0, rangeSize + end)
          : Math.min(end, rangeSize);

    if (actualEnd <= actualBegin) {
      return ImmutableRangeImpl(0, 0, 1);
    }

    const newStart = get(actualBegin).get();
    const newEnd = get(actualEnd).orElse(_end);
    return ImmutableRangeImpl(newStart, newEnd, _step);
  };

  const map = <R>(mapper: (value: number, index: number) => R): ImmutableList<R> => {
    const result: R[] = [];
    const rangeSize = size();

    for (let i = 0; i < rangeSize; i++) {
      const valueOpt = get(i);
      if (valueOpt.isPresent()) {
        result.push(mapper(valueOpt.get(), i));
      }
    }
    return ImmutableList.fromArray(result);
  };

  const filter = (predicate: (value: number, index: number) => boolean): ImmutableList<number> => {
    const result: number[] = [];
    const rangeSize = size();

    for (let i = 0; i < rangeSize; i++) {
      const valueOpt = get(i);
      if (valueOpt.isPresent()) {
        const value = valueOpt.get();
        if (predicate(value, i)) {
          result.push(value);
        }
      }
    }
    return ImmutableList.fromArray(result);
  };

  const reduce = <R>(callback: (accumulator: R, value: number) => R, initial: R): R => {
    let accumulator = initial;
    const rangeSize = size();

    for (let i = 0; i < rangeSize; i++) {
      const valueOpt = get(i);
      if (valueOpt.isPresent()) {
        accumulator = callback(accumulator, valueOpt.get());
      }
    }
    return accumulator;
  };

  return {
    size,
    isEmpty,
    isNotEmpty,
    get,
    first,
    last,
    indexOf,
    toArray,
    toList,
    toSet,
    slice,
    map,
    filter,
    reduce,
  };
};

// Factory interface
export interface ImmutableRangeConstructor {
  (start: number, end: number, step?: number): ImmutableRange;
  of(start: number, end: number, step?: number): ImmutableRange;
}

// Factory implementation
export const ImmutableRange: ImmutableRangeConstructor = Object.assign(
  (start: number, end: number, step = 1): ImmutableRange => {
    return ImmutableRangeImpl(start, end, step);
  },
  {
    of: (start: number, end: number, step = 1): ImmutableRange => {
      return ImmutableRangeImpl(start, end, step);
    },
  }
);

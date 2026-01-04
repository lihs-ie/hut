import { scramble } from "./math";
import {
  ImmutableList,
  ImmutableMap,
  ImmutableRange,
  ImmutableSet,
} from "@/domains/common/collections";

type FactoryDelegate<T, P extends object> = {
  instantiate: (properties: P) => T;
  prepare: (overrides: Partial<P>, seed: number) => P;
  retrieve: (instance: T) => P;
};

type Factory<T, P extends object> = FactoryDelegate<T, P> & {
  create: (overrides: Partial<P>, seed: number) => T;
  duplicate: (instance: T, overrides: Partial<P>) => T;
};

type Builder<T> = {
  build: (overrides?: Partial<T>) => T;
  buildList: (size: number, overrides?: Partial<T>) => T[];
  buildWith: (seed: number, overrides?: Partial<T>) => T;
  buildListWith: (size: number, seed: number, overrides?: Partial<T>) => T[];
  duplicate: (instance: T, overrides?: Partial<T>) => T;
};

export const Builder = <T, P extends object>(factory: Factory<T, P>) => {
  const seeds = ImmutableSet.empty<number>();

  const nextSeed = (): number => {
    return nextSeeds(1).first().get();
  };

  const nextSeeds = (size: number): ImmutableList<number> => {
    const next = ImmutableRange(0, seeds.size() + size)
      .toList()
      .map(() => Math.floor(Math.random() * Number.MAX_SAFE_INTEGER))
      .filter((candidate) => !seeds.contains(candidate));

    next.foreach((seed) => {
      seeds.add(seed);
    });

    return next;
  };

  const build = (overrides: Partial<P> = {}): T => {
    return factory.create(overrides, nextSeed());
  };

  const buildList = (
    size: number,
    overrides: Partial<P> = {}
  ): ImmutableList<T> => {
    return nextSeeds(size).map((seed) => factory.create(overrides, seed));
  };

  const buildWith = (seed: number, overrides: Partial<P> = {}): T => {
    return factory.create(overrides, seed);
  };

  const buildListWith = (
    size: number,
    seed: number,
    overrides: Partial<P> = {}
  ): ImmutableList<T> => {
    return ImmutableRange(0, size)
      .toList()
      .map((index) => buildWith(seed + index, overrides));
  };

  const duplicate = (instance: T, overrides: Partial<P> = {}): T => {
    return factory.duplicate(instance, overrides);
  };

  return {
    build,
    buildList,
    buildWith,
    buildListWith,
    duplicate,
  };
};

export const Factory = <T, P extends object>(
  delegate: FactoryDelegate<T, P>
): Factory<T, P> => {
  const create = (overrides: Partial<P>, seed: number): T => {
    return delegate.instantiate(delegate.prepare(overrides, seed));
  };

  const duplicate = (instance: T, overrides: Partial<P>): T => {
    return delegate.instantiate({
      ...delegate.retrieve(instance),
      ...overrides,
    });
  };

  return {
    ...delegate,
    create,
    duplicate,
  };
};

export const Characters = {
  ALPHANUMERIC: ImmutableSet.fromArray([
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "i",
    "j",
    "k",
    "l",
    "m",
    "n",
    "o",
    "p",
    "q",
    "r",
    "s",
    "t",
    "u",
    "v",
    "w",
    "x",
    "y",
    "z",
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M",
    "N",
    "O",
    "P",
    "Q",
    "R",
    "S",
    "T",
    "U",
    "V",
    "W",
    "X",
    "Y",
    "Z",
    "0",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
  ]),
  ALPHA: ImmutableSet.fromArray([
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "i",
    "j",
    "k",
    "l",
    "m",
    "n",
    "o",
    "p",
    "q",
    "r",
    "s",
    "t",
    "u",
    "v",
    "w",
    "x",
    "y",
    "z",
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M",
    "N",
    "O",
    "P",
    "Q",
    "R",
    "S",
    "T",
    "U",
    "V",
    "W",
    "X",
    "Y",
    "Z",
  ]),
  SLUG: ImmutableSet.fromArray([
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "i",
    "j",
    "k",
    "l",
    "m",
    "n",
    "o",
    "p",
    "q",
    "r",
    "s",
    "t",
    "u",
    "v",
    "w",
    "x",
    "y",
    "z",
    "0",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "-",
  ]),
  NUMERIC: ImmutableSet.fromArray([
    "0",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
  ]),
  SYMBOL: ImmutableSet.fromArray([
    "!",
    '"',
    "#",
    "$",
    "%",
    "&",
    "'",
    "(",
    ")",
    "*",
    "+",
    ",",
    "-",
    ".",
    "/",
    ":",
    ";",
    "<",
    "=",
    ">",
    "?",
    "@",
    "[",
    "\\",
    "]",
    "^",
    "_",
    "`",
    "{",
    " |",
    "}",
    "~",
  ]),
};

type StringProperties = {
  value: string;
};

export const StringFactory = (
  min?: number | null,
  max?: number | null,
  candidates?: ImmutableSet<string>
) => {
  const minLength = min ?? 1;
  const maxLength = max ?? 255;
  const characters = (candidates ?? Characters.ALPHANUMERIC).toList();

  return Factory<string, StringProperties>({
    instantiate: (properties: StringProperties): string => properties.value,
    prepare: (
      overrides: Partial<StringProperties>,
      seed: number
    ): StringProperties => {
      const offset = seed % (maxLength - minLength + 1);
      const length = minLength + offset;

      const value = ImmutableRange(0, length)
        .toList()
        .map((index) =>
          characters.get(scramble(seed + index) % characters.size()).get()
        )
        .toArray()
        .join("");

      return { value, ...overrides };
    },
    retrieve: (instance: string): StringProperties => ({
      value: instance,
    }),
  });
};

export const EnumFactory = <T extends object>(choices: T) => {
  type Choice = T[keyof T];
  type Properties = {
    value: Choice;
    exclusion?: Choice | ImmutableSet<Choice>;
  };

  const candidates = ImmutableSet.fromArray<Choice>(Object.values(choices));

  const determineExclusions = (
    exclusions?: Choice | ImmutableSet<Choice>
  ): ImmutableSet<Choice> => {
    return exclusions === undefined
      ? ImmutableSet.empty()
      : ImmutableSet.isSet<Choice>(exclusions)
      ? exclusions
      : ImmutableSet.fromArray<Choice>([exclusions]);
  };

  return Factory<Choice, Properties>({
    instantiate: (properties: Properties): Choice => {
      return properties.value;
    },
    prepare: (overrides: Partial<Properties>, seed: number): Properties => {
      const exclusions = determineExclusions(overrides.exclusion);

      const actuals = candidates.subtract(exclusions.toArray()).toList();

      if (actuals.isEmpty()) {
        throw new Error("Candidates does not exist.");
      }

      return { value: actuals.get(seed % actuals.size()).get(), ...overrides };
    },
    retrieve: (instance: Choice): Properties => {
      return { value: instance };
    },
  });
};

export type MapProperties<K, V> = {
  entries: [K, V][];
};

export const MapFactory = <K, KP extends object, V, VP extends object>(
  key: Factory<K, KP>,
  value: Factory<V, VP>
) =>
  Factory<ImmutableMap<K, V>, MapProperties<K, V>>({
    instantiate: (properties: MapProperties<K, V>): ImmutableMap<K, V> =>
      ImmutableMap.fromArray(properties.entries),
    prepare: (
      overrides: Partial<MapProperties<K, V>>,
      seed: number
    ): MapProperties<K, V> => {
      const entries = ImmutableRange(0, (seed % 10) + 1)
        .toList()
        .map((index): [K, V] => {
          const keyInstance = key.create({}, seed + index);
          const valueInstance = value.create({}, seed + index);

          return [keyInstance, valueInstance];
        })
        .toArray();

      return { entries, ...overrides };
    },
    retrieve: (instance: ImmutableMap<K, V>): MapProperties<K, V> => ({
      entries: instance.toArray(),
    }),
  });

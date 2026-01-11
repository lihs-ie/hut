import { Characters, Forger, StringMold } from "@lihs-ie/forger-ts";
import type { Mold as MoldType } from "@lihs-ie/forger-ts";
import { ImmutableList } from "@shared/domains/common";

export { Characters };

export type Factory<T, P extends object> = MoldType<T, P>;

/**
 * Wrap a mold as a factory for consistent naming in tests.
 */
export const Factory = <T, P extends object>(
  mold: Factory<T, P>,
): Factory<T, P> => {
  return mold;
};

type BuilderApi<T, P extends object> = {
  build: (overrides?: Partial<P>) => T;
  buildWith: (seed: number, overrides?: Partial<P>) => T;
  buildList: (size: number, overrides?: Partial<P>) => ImmutableList<T>;
  buildListWith: (
    size: number,
    seed: number,
    overrides?: Partial<P>,
  ) => ImmutableList<T>;
  duplicate: (base: Partial<P>, overrides?: Partial<P>) => T;
};

type StringProperties = {
  value: string;
};

/**
 * Create a string factory with optional length constraints.
 */
export const StringFactory = (
  minimumLength?: number | null,
  maximumLength?: number | null,
  candidates?: readonly string[],
): Factory<string, StringProperties> => {
  return StringMold(minimumLength, maximumLength, candidates);
};

/**
 * Build entities from a factory with deterministic or random seeds.
 */
export const Builder = <T, P extends object>(
  factory: Factory<T, P>,
): BuilderApi<T, P> => {
  const forger = Forger(factory);

  /**
   * Build a single entity using random seed values.
   */
  const build = (overrides?: Partial<P>): T => {
    return forger.forge(overrides);
  };

  /**
   * Build a single entity with a fixed seed.
   */
  const buildWith = (seed: number, overrides?: Partial<P>): T => {
    return forger.forgeWithSeed(seed, overrides);
  };

  /**
   * Build a list of entities using random seed values.
   */
  const buildList = (
    size: number,
    overrides?: Partial<P>,
  ): ImmutableList<T> => {
    return ImmutableList.fromArray(forger.forgeMulti(size, overrides));
  };

  /**
   * Build a list of entities with a fixed starting seed.
   */
  const buildListWith = (
    size: number,
    seed: number,
    overrides?: Partial<P>,
  ): ImmutableList<T> => {
    return ImmutableList.fromArray(
      forger.forgeMultiWithSeed(size, seed, overrides),
    );
  };

  /**
   * Duplicate an entity while overriding selected properties.
   */
  const duplicate = (base: Partial<P>, overrides?: Partial<P>): T => {
    return forger.forge({
      ...base,
      ...(overrides ?? {}),
    });
  };

  return {
    build,
    buildWith,
    buildList,
    buildListWith,
    duplicate,
  };
};

/**
 * Global converters setup to avoid circular import issues
 */
import { createConverters } from './converters';
import { Hasher } from './hamt/hash';
import { ImmutableList } from './list/common';
import { MapFromArray } from './map';
import { IndexedSequence } from './sequence/common';
import { fromArray as setFromArray } from './set/common';

// Initialize converters once and store globally
const converters = createConverters(
  ImmutableList,
  (hasher: Hasher) =>
    <T>(values: T[]) =>
      setFromArray(hasher)(values),
  (hasher: Hasher) =>
    <K, V>(entries: [K, V][]) =>
      MapFromArray(hasher)(entries),
  IndexedSequence
);

// Store in global for access without circular imports
(globalThis as Record<string, unknown>).__conversionHelpers = converters;

export { converters };

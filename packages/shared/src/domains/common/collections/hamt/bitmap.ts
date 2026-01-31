export interface Bitmap {
  bitpos(hash: number, offset: number): number;
  next: (bitpos: number) => Bitmap;
  without: (bitpos: number) => Bitmap;
  has: (bitpos: number) => boolean;
  index: (bitpos: number) => number;
}

export const Bitmap = (value = 0, shiftWidth = 5): Bitmap => {
  const mask = 2 ** shiftWidth - 1;

  const bitpos = (hash: number, offset: number): number => {
    const shift = offset * shiftWidth;

    const shifted = hash >> shift;

    const masked = shifted & mask;

    return 1 << masked;
  };

  const next = (bitpos: number): Bitmap => {
    return Bitmap(value | bitpos);
  };

  const without = (bitpos: number): Bitmap => {
    return Bitmap(value & ~bitpos);
  };

  const has = (bitpos: number): boolean => {
    return (value & bitpos) !== 0;
  };

  const index = (bitpos: number): number => {
    let x = value & (bitpos - 1);

    let count = 0;
    while (x !== 0) {
      x &= x - 1;
      count++;
    }

    return count;
  };

  return {
    bitpos,
    next,
    without,
    has,
    index,
  };
};

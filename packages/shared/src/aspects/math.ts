const asUint32 = (original: bigint): bigint => BigInt.asUintN(32, original & 0xffffffffn);

const invert = (original: bigint): bigint => {
  const masks = [0x55555555n, 0x33333333n, 0x0f0f0f0fn, 0x00ff00ffn, 0xffffffffn];

  return masks.reduce((carry, mask, index) => {
    const padding = BigInt(1 << index);

    const left = (carry >> padding) & mask;
    const right = (carry & mask) << padding;

    return asUint32(left | right);
  }, original);
};

const egcd = (a: bigint, b: bigint): [bigint, bigint, bigint] => {
  if (a === 0n) {
    return [b, 0n, 1n];
  }

  const [g, y, x] = egcd(asUint32(b % a), a);

  return [g, x - asUint32(asUint32(b / a) * y), y];
};

const modinv = (a: bigint, b: bigint): bigint => {
  const [g, x] = egcd(a, b);

  if (g !== 1n) {
    throw new Error(`No inverse is found for ${a} on ${b}.`);
  }

  return asUint32(x % b);
};

const salt = 0x17654321n;
const invertedSalt = modinv(salt, 0xffffffffn + 1n); // saltのモジュラ逆数

export const scramble = (original: number): number => {
  const normalized = asUint32(BigInt(original));
  const base = asUint32(normalized * salt);
  const inverted = invert(base);

  return Number(asUint32(inverted * invertedSalt));
};

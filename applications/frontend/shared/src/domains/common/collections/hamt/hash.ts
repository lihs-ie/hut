const rotateLeft32 = (value: number, bits: number): number =>
  ((value << bits) | (value >>> (32 - bits))) >>> 0;

const readUint32LittleEndian = (byteArray: Uint8Array, offset: number): number =>
  ((byteArray[offset]! || 0) |
    ((byteArray[offset + 1]! || 0) << 8) |
    ((byteArray[offset + 2]! || 0) << 16) |
    ((byteArray[offset + 3]! || 0) << 24)) >>>
  0;

export const xxHash32 = (input: Uint8Array | string, seed = 0): number => {
  const PRIME_32_1 = 0x9e3779b1 >>> 0;
  const PRIME_32_2 = 0x85ebca77 >>> 0;
  const PRIME_32_3 = 0xc2b2ae3d >>> 0;
  const PRIME_32_4 = 0x27d4eb2f >>> 0;
  const PRIME_32_5 = 0x165667b1 >>> 0;

  const inputBytes: Uint8Array =
    typeof input === 'string' ? new TextEncoder().encode(input) : input;
  const inputLength = inputBytes.length;

  let hash32: number;
  let p = 0;

  if (inputLength >= 16) {
    let v1 = (seed + PRIME_32_1 + PRIME_32_2) >>> 0;
    let v2 = (seed + PRIME_32_2) >>> 0;
    let v3 = seed >>> 0;
    let v4 = (seed - PRIME_32_1) >>> 0;

    const limit = inputLength - 16;
    while (p <= limit) {
      v1 = rotateLeft32(v1 + readUint32LittleEndian(inputBytes, p) * PRIME_32_2, 13);
      v1 = (v1 * PRIME_32_1) >>> 0;
      p += 4;
      v2 = rotateLeft32(v2 + readUint32LittleEndian(inputBytes, p) * PRIME_32_2, 13);
      v2 = (v2 * PRIME_32_1) >>> 0;
      p += 4;
      v3 = rotateLeft32(v3 + readUint32LittleEndian(inputBytes, p) * PRIME_32_2, 13);
      v3 = (v3 * PRIME_32_1) >>> 0;
      p += 4;
      v4 = rotateLeft32(v4 + readUint32LittleEndian(inputBytes, p) * PRIME_32_2, 13);
      v4 = (v4 * PRIME_32_1) >>> 0;
      p += 4;
    }
    hash32 =
      (rotateLeft32(v1, 1) + rotateLeft32(v2, 7) + rotateLeft32(v3, 12) + rotateLeft32(v4, 18)) >>>
      0;
  } else {
    hash32 = (seed + PRIME_32_5) >>> 0;
  }

  hash32 = (hash32 + inputLength) >>> 0;

  while (p + 4 <= inputLength) {
    hash32 = (hash32 + readUint32LittleEndian(inputBytes, p) * PRIME_32_3) >>> 0;
    hash32 = (rotateLeft32(hash32, 17) * PRIME_32_4) >>> 0;
    p += 4;
  }

  while (p < inputLength) {
    hash32 = (hash32 + inputBytes[p]! * PRIME_32_5) >>> 0;
    hash32 = (rotateLeft32(hash32, 11) * PRIME_32_1) >>> 0;
    p++;
  }

  hash32 ^= hash32 >>> 15;
  hash32 = Math.imul(hash32, PRIME_32_2);
  hash32 ^= hash32 >>> 13;
  hash32 = Math.imul(hash32, PRIME_32_3);
  hash32 ^= hash32 >>> 16;

  return hash32 >>> 0;
};

// -------- canonical encoder --------
type EncodeOptions = {
  functionPolicy?: 'name' | 'toString' | 'reject' | ((fn: () => void) => Uint8Array);
  symbolPolicy?: 'key' | 'description' | 'reject';
};

class ByteSink {
  private chunks: Uint8Array[] = [];
  private size = 0;

  writeBytes(bytes: Uint8Array) {
    this.chunks.push(bytes);
    this.size += bytes.length;
  }
  writeU8(v: number) {
    this.writeBytes(Uint8Array.of(v & 0xff));
  }
  writeU32(v: number) {
    const b = new Uint8Array(4);
    const dv = new DataView(b.buffer);
    dv.setUint32(0, v >>> 0, true);
    this.writeBytes(b);
  }
  writeI32(v: number) {
    const b = new Uint8Array(4);
    const dv = new DataView(b.buffer);
    dv.setInt32(0, v | 0, true);
    this.writeBytes(b);
  }
  writeF64(v: number) {
    const b = new Uint8Array(8);
    const dv = new DataView(b.buffer);
    dv.setFloat64(0, v, true);
    this.writeBytes(b);
  }
  writeStr(s: string) {
    const bytes = new TextEncoder().encode(s);
    this.writeU32(bytes.length);
    this.writeBytes(bytes);
  }
  toUint8Array(): Uint8Array {
    const out = new Uint8Array(this.size);
    let o = 0;
    for (const c of this.chunks) {
      out.set(c, o);
      o += c.length;
    }
    return out;
  }
}

const enum Tag {
  Null = 0x4e, // 'N'
  Undefined = 0x55, // 'U'
  Bool = 0x42, // 'B'
  Number = 0x64, // 'd'
  BigInt = 0x69, // 'i'
  String = 0x73, // 's'
  Symbol = 0x79, // 'y'
  Function = 0x66, // 'f'
  Array = 0x61, // 'a'
  Object = 0x6f, // 'o'
  Map = 0x6d, // 'm'
  Set = 0x74, // 't'
  Date = 0x44, // 'D'
  RegExp = 0x52, // 'R'
  TypedArray = 0x54, // 'T'
  ArrayBuffer = 0x41, // 'A'
  DataView = 0x56, // 'V'
  Ref = 0x72, // 'r' (cycle/reference)
}

const encodeBigInt = (n: bigint): Uint8Array => {
  const neg = n < 0n;
  let x = neg ? -n : n;
  const bytes: number[] = [];
  while (x > 0n) {
    bytes.push(Number(x & 0xffn));
    x >>= 8n;
  }
  if (bytes.length === 0) bytes.push(0);
  const out = new Uint8Array(1 + bytes.length);
  out[0] = neg ? 1 : 0;
  out.set(bytes, 1);
  return out;
};

const encodeRegExp = (re: RegExp, sink: ByteSink) => {
  sink.writeU8(Tag.RegExp);
  sink.writeStr(re.source);
  sink.writeStr(re.flags);
};

const isTypedArray = (
  value: unknown
): value is
  | Int8Array
  | Uint8Array
  | Uint8ClampedArray
  | Int16Array
  | Uint16Array
  | Int32Array
  | Uint32Array
  | Float32Array
  | Float64Array
  | BigInt64Array
  | BigUint64Array => ArrayBuffer.isView(value) && !(value instanceof DataView);

const typedArrayName = (value: unknown): string =>
  Object.prototype.toString.call(value).slice(8, -1);

function encodeCanonical(
  value: unknown,
  options: EncodeOptions,
  sink: ByteSink,
  seen: WeakMap<object, number>,
  nextId: { v: number }
) {
  // Handle primitives & null/undefined fast
  if (value === null) {
    sink.writeU8(Tag.Null);
    return;
  }
  const t = typeof value;

  switch (t) {
    case 'undefined':
      sink.writeU8(Tag.Undefined);
      return;
    case 'boolean':
      sink.writeU8(Tag.Bool);
      sink.writeU8(value ? 1 : 0);
      return;
    case 'number': {
      // Normalize -0 and NaN families to deterministic bit patterns via F64
      sink.writeU8(Tag.Number);
      // For NaN, standardize to canonical NaN
      const isNaN = Number.isNaN(value);
      const candidate = isNaN ? Number.NaN : Object.is(value, -0) ? -0 : value;
      sink.writeF64(candidate as number);
      return;
    }
    case 'bigint': {
      sink.writeU8(Tag.BigInt);
      const bytes = encodeBigInt(value as bigint);
      sink.writeU32(bytes.length);
      sink.writeBytes(bytes);
      return;
    }
    case 'string':
      sink.writeU8(Tag.String);
      sink.writeStr(value as string);
      return;
    case 'symbol': {
      if (options.symbolPolicy === 'reject')
        throw new TypeError('Cannot hash Symbol (policy=reject)');
      sink.writeU8(Tag.Symbol);
      const key = Symbol.keyFor(value as symbol);
      const tag =
        key != null
          ? `global:${key}`
          : options.symbolPolicy === 'key'
            ? 'local'
            : `desc:${String((value as symbol).description ?? '')}`;
      sink.writeStr(tag);
      return;
    }
    case 'function': {
      const policy = options.functionPolicy ?? 'name';
      if (policy === 'reject') throw new TypeError('Cannot hash Function (policy=reject)');
      sink.writeU8(Tag.Function);
      if (typeof policy === 'function') {
        const bytes = policy(value as () => void);
        sink.writeU32(bytes.length);
        sink.writeBytes(bytes);
      } else if (policy === 'toString') {
        const s = Function.prototype.toString.call(value);
        sink.writeStr(s);
      } else {
        // default: function name only (more stable)
        sink.writeStr((value as () => void).name || '');
      }
      return;
    }
  }

  // Objects & complex
  if (typeof value === 'object') {
    // Cycle detection
    const refTarget = value;
    const existed = seen.get(refTarget);
    if (existed !== undefined) {
      sink.writeU8(Tag.Ref);
      sink.writeU32(existed);
      return;
    }
    const id = nextId.v++;
    seen.set(refTarget, id);

    // Array
    if (Array.isArray(value)) {
      sink.writeU8(Tag.Array);
      sink.writeU32(value.length);
      for (const el of value) encodeCanonical(el, options, sink, seen, nextId);
      return;
    }

    // Date
    if (value instanceof Date) {
      sink.writeU8(Tag.Date);
      sink.writeF64(value.getTime());
      return;
    }

    // RegExp
    if (value instanceof RegExp) {
      encodeRegExp(value, sink);
      return;
    }

    // ArrayBuffer
    if (value instanceof ArrayBuffer) {
      sink.writeU8(Tag.ArrayBuffer);
      const bytes = new Uint8Array(value);
      sink.writeU32(bytes.length);
      sink.writeBytes(bytes);
      return;
    }

    // DataView
    if (value instanceof DataView) {
      sink.writeU8(Tag.DataView);
      const bytes = new Uint8Array(value.buffer, value.byteOffset, value.byteLength);
      sink.writeU32(bytes.length);
      sink.writeBytes(bytes);
      return;
    }

    // TypedArray
    if (isTypedArray(value)) {
      sink.writeU8(Tag.TypedArray);
      sink.writeStr(typedArrayName(value));
      const bytes = new Uint8Array(value.buffer, value.byteOffset, value.byteLength);
      sink.writeU32(bytes.length);
      sink.writeBytes(bytes);
      return;
    }

    // Map
    if (value instanceof Map) {
      sink.writeU8(Tag.Map);
      // Sort entries by encoded key to get deterministic order
      const encodedEntries: { k: unknown; kb: Uint8Array; v: unknown }[] = [];
      for (const [k, v] of value) {
        const ksink = new ByteSink();
        encodeCanonical(k, options, ksink, new WeakMap(), { v: 1 }); // fresh seen for key-only
        encodedEntries.push({ k, kb: ksink.toUint8Array(), v });
      }
      encodedEntries.sort((a, b) => {
        const ka = a.kb,
          kb2 = b.kb;
        const len = Math.min(ka.length, kb2.length);
        for (let i = 0; i < len; i++) {
          const d = ka[i]! - kb2[i]!;
          if (d) return d;
        }
        return ka.length - kb2.length;
      });
      sink.writeU32(encodedEntries.length);
      for (const e of encodedEntries) {
        sink.writeU32(e.kb.length);
        sink.writeBytes(e.kb);
        encodeCanonical(e.v, options, sink, seen, nextId);
      }
      return;
    }

    // Set
    if (value instanceof Set) {
      sink.writeU8(Tag.Set);
      const items: Uint8Array[] = [];
      for (const el of value) {
        const es = new ByteSink();
        encodeCanonical(el, options, es, new WeakMap(), { v: 1 });
        items.push(es.toUint8Array());
      }
      items.sort((a, b) => {
        const len = Math.min(a.length, b.length);
        for (let i = 0; i < len; i++) {
          const d = a[i]! - b[i]!;
          if (d) return d;
        }
        return a.length - b.length;
      });
      sink.writeU32(items.length);
      for (const kb of items) {
        sink.writeU32(kb.length);
        sink.writeBytes(kb);
      }
      return;
    }

    // Plain/Object or class instance: encode constructor name + sorted props
    sink.writeU8(Tag.Object);
    sink.writeStr(value.constructor?.name ?? 'Object');

    // collect string keys and symbol keys
    const stringKeys = Object.keys(value);
    const symbolKeys = Object.getOwnPropertySymbols(value);

    // sort keys deterministically
    stringKeys.sort(); // lexicographic
    symbolKeys.sort((a, b) => {
      const ad = String(a.description ?? '');
      const bd = String(b.description ?? '');
      return ad < bd ? -1 : ad > bd ? 1 : 0;
    });

    // write string-keyed props
    sink.writeU32(stringKeys.length);
    for (const key of stringKeys) {
      sink.writeStr(key);
      encodeCanonical(value[key as keyof typeof value], options, sink, seen, nextId);
    }

    // write symbol-keyed props
    sink.writeU32(symbolKeys.length);
    for (const s of symbolKeys) {
      // encode symbol identity per policy
      const sp = options.symbolPolicy ?? 'description';
      if (sp === 'reject') throw new TypeError('Cannot hash symbol-keyed property (policy=reject)');
      const tag =
        Symbol.keyFor(s) != null
          ? `global:${Symbol.keyFor(s)}`
          : sp === 'key'
            ? 'local'
            : `desc:${String(s.description ?? '')}`;
      sink.writeStr(tag);
      encodeCanonical(value[s as keyof typeof value], options, sink, seen, nextId);
    }
    return;
  }

  // Fallback (should not reach)
  sink.writeU8(Tag.Undefined);
}

export interface Hasher {
  hash: <T>(value: T) => number;
}

export function createStableHasher(options: EncodeOptions = {}): Hasher {
  return {
    hash<T>(value: T): number {
      const sink = new ByteSink();
      const seen = new WeakMap<object, number>();
      encodeCanonical(value as unknown, options, sink, seen, { v: 1 });
      const bytes = sink.toUint8Array();
      return xxHash32(bytes, 0) >>> 0;
    },
  };
}

export const Hasher = (): Hasher => ({
  hash: <T>(value: T): number => {
    const sink = new ByteSink();
    const seen = new WeakMap<object, number>();
    encodeCanonical(value, {}, sink, seen, { v: 1 });
    const bytes = sink.toUint8Array();
    return xxHash32(bytes, 0) >>> 0;
  },
});

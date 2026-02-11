import { Bitmap } from './bitmap';

type Option<T> = T | undefined;

const Type = {
  LEAF: 'leaf',
  BITMAP_INDEXED: 'bitmapIndexed',
} as const;

type Type = (typeof Type)[keyof typeof Type];

export interface HAMTNode<K, V> {
  readonly type: Type;
  key: () => Option<K>;
  value: () => Option<V>;
  get: (hash: number, offset: number) => Option<V>;
  find: (predicate: (key: K, value: V) => boolean) => Option<HAMTNode<K, V>>;
  add: (hash: number, offset: number, key: K, value: V) => HAMTNode<K, V>;
  remove: (hash: number, offset: number) => Option<HAMTNode<K, V>>;
  contains: (hash: number, offset: number) => boolean;
  exists: (predicate: (key: K, value: V) => boolean) => boolean;
  toArray: () => [K, V][];
}

export interface LeafNode<K, V> extends HAMTNode<K, V> {
  readonly type: typeof Type.LEAF;
  readonly key: () => K;
  readonly value: () => V;
}

export const LeafNode = <K, V>(nodeHash: number, nodeKey: K, nodeValue: V): HAMTNode<K, V> => {
  const self = () => LeafNode<K, V>(nodeHash, nodeKey, nodeValue);

  const key = () => nodeKey;

  const value = () => nodeValue;

  const get = (hash: number, _: number): Option<V> => {
    return nodeHash === hash ? nodeValue : undefined;
  };

  const find = (predicate: (key: K, value: V) => boolean): Option<HAMTNode<K, V>> => {
    return predicate(nodeKey, nodeValue) ? self() : undefined;
  };

  const add = (hash: number, offset: number, key: K, value: V): HAMTNode<K, V> => {
    if (nodeHash === hash) {
      // If the hash is the same, we can just update the value
      return LeafNode<K, V>(nodeHash, nodeKey, value);
    }

    return BitmapIndexedNode<K, V>()
      .add(nodeHash, offset, nodeKey, nodeValue)
      .add(hash, offset, key, value);
  };

  const remove = (hash: number, _: number): Option<HAMTNode<K, V>> => {
    return nodeHash === hash ? undefined : self();
  };

  const exists = (predicate: (key: K, value: V) => boolean): boolean => {
    return predicate(nodeKey, nodeValue);
  };

  const contains = (hash: number, _: number): boolean => {
    return nodeHash === hash;
  };

  const toArray = (): [K, V][] => [[nodeKey, nodeValue]];

  return {
    type: Type.LEAF,
    key,
    value,
    get,
    find,
    add,
    contains,
    remove,
    exists,
    toArray,
  };
};

export interface BitmapIndexedNode<K, V> extends HAMTNode<K, V> {
  readonly type: typeof Type.BITMAP_INDEXED;
  key: () => undefined;
  value: () => undefined;
}

export const BitmapIndexedNode = <K, V>(
  bitmap: Bitmap = Bitmap(),
  nodes: HAMTNode<K, V>[] = []
): BitmapIndexedNode<K, V> => {
  const self = () => BitmapIndexedNode<K, V>(bitmap, nodes);

  const replaceNode = (index: number, node: HAMTNode<K, V>): HAMTNode<K, V>[] => {
    return [...nodes.slice(0, index), node, ...nodes.slice(index + 1)];
  };

  const insertNode = (index: number, node: HAMTNode<K, V>): HAMTNode<K, V>[] => {
    return [...nodes.slice(0, index), node, ...nodes.slice(index)];
  };

  const removeNode = (index: number): HAMTNode<K, V>[] => {
    return [...nodes.slice(0, index), ...nodes.slice(index + 1)];
  };

  const key = () => undefined;

  const value = () => undefined;

  const get = (hash: number, offset: number): Option<V> => {
    const bitpos = bitmap.bitpos(hash, offset);

    if (!bitmap.has(bitpos)) {
      return undefined;
    }

    const index = bitmap.index(bitpos);

    return nodes[index]!.get(hash, offset + 1);
  };

  const find = (predicate: (key: K, value: V) => boolean): Option<HAMTNode<K, V>> => {
    for (const node of nodes) {
      const target = node.find(predicate);

      if (target !== undefined) {
        return target;
      }
    }

    return undefined;
  };

  const add = (hash: number, offset: number, key: K, value: V): HAMTNode<K, V> => {
    const bitpos = bitmap.bitpos(hash, offset);
    const index = bitmap.index(bitpos);

    if (bitmap.has(bitpos)) {
      const target = nodes[index];

      const nextNode = target!.add(hash, offset + 1, key, value);

      if (target === nextNode) {
        return self();
      }

      return BitmapIndexedNode<K, V>(bitmap, replaceNode(index, nextNode));
    }

    const nextNodes = insertNode(index, LeafNode(hash, key, value));

    return BitmapIndexedNode<K, V>(bitmap.next(bitpos), nextNodes);
  };

  const remove = (hash: number, offset: number): Option<HAMTNode<K, V>> => {
    const bitpos = bitmap.bitpos(hash, offset);

    if (!bitmap.has(bitpos)) {
      return self();
    }

    const index = bitmap.index(bitpos);
    const target = nodes[index];
    const nextNode = target!.remove(hash, offset + 1);

    if (target === nextNode) {
      return self();
    }

    if (nextNode === undefined) {
      const nextBitmap = bitmap.without(bitpos);
      const nextNodes = removeNode(index);

      if (nextNodes.length === 0) {
        return undefined;
      }

      return BitmapIndexedNode<K, V>(nextBitmap, nextNodes);
    }

    return BitmapIndexedNode<K, V>(bitmap, replaceNode(index, nextNode));
  };

  const exists = (predicate: (key: K, value: V) => boolean): boolean => {
    for (const node of nodes) {
      if (node.exists(predicate)) {
        return true;
      }
    }

    return false;
  };

  const contains = (hash: number, offset: number): boolean => {
    const bitpos = bitmap.bitpos(hash, offset);

    if (!bitmap.has(bitpos)) {
      return false;
    }

    const index = bitmap.index(bitpos);

    return nodes[index]!.contains(hash, offset + 1);
  };

  const toArray = (): [K, V][] => {
    return nodes.flatMap(node => node.toArray());
  };

  return {
    type: Type.BITMAP_INDEXED,
    key,
    value,
    get,
    add,
    remove,
    contains,
    find,
    exists,
    toArray,
  };
};

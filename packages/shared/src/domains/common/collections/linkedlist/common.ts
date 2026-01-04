import { createConverters } from '../converters';
import { type Hasher } from '../hamt/hash';
import { type ImmutableList } from '../list/common';
import { Optional } from '../optional/common';
import { type ImmutableSet } from '../set/common';

// LinkedListNode interface
export interface LinkedListNode<T> {
  readonly id: string;
  readonly value: T;
  readonly next: LinkedListNode<T> | null;
  readonly prev: LinkedListNode<T> | null;
}

// LinkedList interface
export interface LinkedList<T> {
  readonly head: LinkedListNode<T> | null;
  readonly tail: LinkedListNode<T> | null;

  // Basic operations
  size(): number;
  isEmpty(): boolean;
  isNotEmpty(): boolean;

  // Add operations
  addFirst(value: T): LinkedList<T>;
  addLast(value: T): LinkedList<T>;
  addAfter(node: LinkedListNode<T>, value: T): LinkedList<T>;
  addBefore(node: LinkedListNode<T>, value: T): LinkedList<T>;
  addAt(index: number, value: T): LinkedList<T>;

  // Remove operations
  removeFirst(): LinkedList<T>;
  removeLast(): LinkedList<T>;
  remove(value: T): LinkedList<T>;
  removeNode(node: LinkedListNode<T>): LinkedList<T>;
  removeAt(index: number): LinkedList<T>;

  // Access operations
  first(): Optional<T>;
  last(): Optional<T>;
  get(index: number): Optional<T>;
  getNode(index: number): Optional<LinkedListNode<T>>;
  findNode(predicate: (value: T) => boolean): Optional<LinkedListNode<T>>;

  // Update operations
  updateNode(node: LinkedListNode<T>, value: T): LinkedList<T>;
  updateAt(index: number, value: T): LinkedList<T>;

  // Functional operations
  map<R>(mapper: (value: T, node: LinkedListNode<T>) => R): LinkedList<R>;
  filter(
    predicate: (value: T, node: LinkedListNode<T>) => boolean,
  ): LinkedList<T>;
  find(predicate: (value: T, node: LinkedListNode<T>) => boolean): Optional<T>;
  findIndex(predicate: (value: T, node: LinkedListNode<T>) => boolean): number;
  foreach(callback: (value: T, node: LinkedListNode<T>) => void): void;
  reduce<R>(
    callback: (accumulator: R, value: T, node: LinkedListNode<T>) => R,
    initial: R,
  ): R;

  // Conversion operations
  toArray(): T[];
  toList(): ImmutableList<T>;
  toSet(hasher: Hasher): ImmutableSet<T>;
  reverse(): LinkedList<T>;

  // Utility operations
  equals(other: LinkedList<T>, equalityFn?: (a: T, b: T) => boolean): boolean;
}

// Helper function to generate unique IDs
let nodeIdCounter = 0;
const generateNodeId = (): string => `node_${++nodeIdCounter}_${Date.now()}`;

// Internal mutable node interface for construction
interface MutableLinkedListNode<T> {
  id: string;
  value: T;
  next: MutableLinkedListNode<T> | null;
  prev: MutableLinkedListNode<T> | null;
}

// Create a new LinkedListNode
const createNode = <T>(
  value: T,
  next: MutableLinkedListNode<T> | null = null,
  prev: MutableLinkedListNode<T> | null = null,
): MutableLinkedListNode<T> => ({
  id: generateNodeId(),
  value,
  next,
  prev,
});

// Create mutable working node interface for constructing immutable nodes
interface MutableWorkingNode<T> {
  id: string;
  value: T;
  next: MutableWorkingNode<T> | null;
  prev: MutableWorkingNode<T> | null;
}

// Create immutable snapshot of all nodes from the chain
const createNodesSnapshot = <T>(
  mutableHead: MutableLinkedListNode<T> | null,
): Map<string, LinkedListNode<T>> => {
  const nodeMap = new Map<string, MutableWorkingNode<T>>();

  // First pass: create all nodes without references
  let current = mutableHead;
  while (current) {
    nodeMap.set(current.id, {
      id: current.id,
      value: current.value,
      next: null,
      prev: null,
    });
    current = current.next;
  }

  // Second pass: establish references
  current = mutableHead;
  while (current) {
    const workingNode = nodeMap.get(current.id);
    if (workingNode) {
      workingNode.next = current.next
        ? (nodeMap.get(current.next.id) ?? null)
        : null;
      workingNode.prev = current.prev
        ? (nodeMap.get(current.prev.id) ?? null)
        : null;
    }
    current = current.next;
  }

  // Convert to read-only LinkedListNode map
  const immutableMap = new Map<string, LinkedListNode<T>>();
  for (const [id, workingNode] of nodeMap) {
    immutableMap.set(id, workingNode as LinkedListNode<T>);
  }

  return immutableMap;
};

const LinkedListImpl = <T>(values: T[] = []): LinkedList<T> => {
  // Build initial node chain using mutable nodes internally
  let mutableHead: MutableLinkedListNode<T> | null = null;
  let mutableTail: MutableLinkedListNode<T> | null = null;
  let nodeCount = 0;

  if (values.length > 0) {
    mutableHead = createNode(values[0]);
    mutableTail = mutableHead;
    nodeCount = 1;

    for (let i = 1; i < values.length; i++) {
      const newNode: MutableLinkedListNode<T> = createNode(
        values[i],
        null,
        mutableTail,
      );
      if (mutableTail !== null) {
        mutableTail.next = newNode;
      }
      mutableTail = newNode;
      nodeCount++;
    }
  }

  // Convert to immutable interface for external access
  const nodesSnapshot = createNodesSnapshot(mutableHead);
  const head: LinkedListNode<T> | null = mutableHead
    ? (nodesSnapshot.get(mutableHead.id) ?? null)
    : null;
  const tail: LinkedListNode<T> | null = mutableTail
    ? (nodesSnapshot.get(mutableTail.id) ?? null)
    : null;

  const size = (): number => nodeCount;
  const isEmpty = (): boolean => nodeCount === 0;
  const isNotEmpty = (): boolean => nodeCount > 0;

  const addFirst = (value: T): LinkedList<T> => {
    return LinkedListImpl([value, ...toArray()]);
  };

  const addLast = (value: T): LinkedList<T> => {
    return LinkedListImpl([...toArray(), value]);
  };

  const addAfter = (node: LinkedListNode<T>, value: T): LinkedList<T> => {
    const array = toArray();
    const nodeIndex = findNodeIndex(node);
    if (nodeIndex === -1) {
      throw new Error('Node not found in this LinkedList');
    }

    array.splice(nodeIndex + 1, 0, value);
    return LinkedListImpl(array);
  };

  const addBefore = (node: LinkedListNode<T>, value: T): LinkedList<T> => {
    const array = toArray();
    const nodeIndex = findNodeIndex(node);
    if (nodeIndex === -1) {
      throw new Error('Node not found in this LinkedList');
    }

    array.splice(nodeIndex, 0, value);
    return LinkedListImpl(array);
  };

  const addAt = (index: number, value: T): LinkedList<T> => {
    if (index < 0 || index > nodeCount) {
      throw new Error('Index out of bounds');
    }

    const array = toArray();
    array.splice(index, 0, value);
    return LinkedListImpl(array);
  };

  const removeFirst = (): LinkedList<T> => {
    if (isEmpty()) {
      return LinkedListImpl<T>([]);
    }
    return LinkedListImpl(toArray().slice(1));
  };

  const removeLast = (): LinkedList<T> => {
    if (isEmpty()) {
      return LinkedListImpl<T>([]);
    }
    return LinkedListImpl(toArray().slice(0, -1));
  };

  const remove = (value: T): LinkedList<T> => {
    const array = toArray();
    const index = array.indexOf(value);
    if (index !== -1) {
      array.splice(index, 1);
    }
    return LinkedListImpl(array);
  };

  const removeNode = (node: LinkedListNode<T>): LinkedList<T> => {
    const array = toArray();
    const nodeIndex = findNodeIndex(node);
    if (nodeIndex !== -1) {
      array.splice(nodeIndex, 1);
    }
    return LinkedListImpl(array);
  };

  const removeAt = (index: number): LinkedList<T> => {
    if (index < 0 || index >= nodeCount) {
      throw new Error('Index out of bounds');
    }

    const array = toArray();
    array.splice(index, 1);
    return LinkedListImpl(array);
  };

  const first = (): Optional<T> => {
    return head ? Optional(head.value) : Optional<T>();
  };

  const last = (): Optional<T> => {
    return tail ? Optional(tail.value) : Optional<T>();
  };

  const get = (index: number): Optional<T> => {
    if (index < 0 || index >= nodeCount) {
      return Optional<T>();
    }

    let current = head;
    for (let i = 0; i < index && current; i++) {
      current = current.next;
    }

    return current ? Optional(current.value) : Optional<T>();
  };

  const getNode = (index: number): Optional<LinkedListNode<T>> => {
    if (index < 0 || index >= nodeCount) {
      return Optional<LinkedListNode<T>>();
    }

    let current = head;
    for (let i = 0; i < index && current; i++) {
      current = current.next;
    }

    return current ? Optional(current) : Optional<LinkedListNode<T>>();
  };

  const findNode = (
    predicate: (value: T) => boolean,
  ): Optional<LinkedListNode<T>> => {
    let current = head;
    while (current) {
      if (predicate(current.value)) {
        return Optional(current);
      }
      current = current.next;
    }
    return Optional<LinkedListNode<T>>();
  };

  const findNodeIndex = (targetNode: LinkedListNode<T>): number => {
    let current = head;
    let index = 0;
    while (current) {
      if (current.id === targetNode.id) {
        return index;
      }
      current = current.next;
      index++;
    }
    return -1;
  };

  const updateNode = (node: LinkedListNode<T>, value: T): LinkedList<T> => {
    const array = toArray();
    const nodeIndex = findNodeIndex(node);
    if (nodeIndex !== -1) {
      array[nodeIndex] = value;
    }
    return LinkedListImpl(array);
  };

  const updateAt = (index: number, value: T): LinkedList<T> => {
    if (index < 0 || index >= nodeCount) {
      throw new Error('Index out of bounds');
    }

    const array = toArray();
    array[index] = value;
    return LinkedListImpl(array);
  };

  const map = <R>(
    mapper: (value: T, node: LinkedListNode<T>) => R,
  ): LinkedList<R> => {
    const results: R[] = [];
    let current = head;
    while (current) {
      results.push(mapper(current.value, current));
      current = current.next;
    }
    return LinkedListImpl(results);
  };

  const filter = (
    predicate: (value: T, node: LinkedListNode<T>) => boolean,
  ): LinkedList<T> => {
    const results: T[] = [];
    let current = head;
    while (current) {
      if (predicate(current.value, current)) {
        results.push(current.value);
      }
      current = current.next;
    }
    return LinkedListImpl(results);
  };

  const find = (
    predicate: (value: T, node: LinkedListNode<T>) => boolean,
  ): Optional<T> => {
    let current = head;
    while (current) {
      if (predicate(current.value, current)) {
        return Optional(current.value);
      }
      current = current.next;
    }
    return Optional<T>();
  };

  const findIndex = (
    predicate: (value: T, node: LinkedListNode<T>) => boolean,
  ): number => {
    let current = head;
    let index = 0;
    while (current) {
      if (predicate(current.value, current)) {
        return index;
      }
      current = current.next;
      index++;
    }
    return -1;
  };

  const foreach = (
    callback: (value: T, node: LinkedListNode<T>) => void,
  ): void => {
    let current = head;
    while (current) {
      callback(current.value, current);
      current = current.next;
    }
  };

  const reduce = <R>(
    callback: (accumulator: R, value: T, node: LinkedListNode<T>) => R,
    initial: R,
  ): R => {
    let accumulator = initial;
    let current = head;
    while (current) {
      accumulator = callback(accumulator, current.value, current);
      current = current.next;
    }
    return accumulator;
  };

  const toArray = (): T[] => {
    const result: T[] = [];
    let current = head;
    while (current) {
      result.push(current.value);
      current = current.next;
    }
    return result;
  };

  const toList = (): ImmutableList<T> => {
    const converters = (globalThis as Record<string, unknown>)
      .__conversionHelpers as ReturnType<typeof createConverters>;

    return converters.listFactory(toArray());
  };

  const toSet = (hasher: Hasher): ImmutableSet<T> => {
    const converters = (globalThis as Record<string, unknown>)
      .__conversionHelpers as ReturnType<typeof createConverters>;

    return converters.setFactory(hasher)(toArray());
  };

  const reverse = (): LinkedList<T> => {
    return LinkedListImpl(toArray().reverse());
  };

  const equals = (
    other: LinkedList<T>,
    equalityFn: (a: T, b: T) => boolean = (a, b) => a === b,
  ): boolean => {
    if (size() !== other.size()) {
      return false;
    }

    const thisArray = toArray();
    const otherArray = other.toArray();

    for (let i = 0; i < thisArray.length; i++) {
      if (!equalityFn(thisArray[i], otherArray[i])) {
        return false;
      }
    }

    return true;
  };

  return {
    head,
    tail,
    size,
    isEmpty,
    isNotEmpty,
    addFirst,
    addLast,
    addAfter,
    addBefore,
    addAt,
    removeFirst,
    removeLast,
    remove,
    removeNode,
    removeAt,
    first,
    last,
    get,
    getNode,
    findNode,
    updateNode,
    updateAt,
    map,
    filter,
    find,
    findIndex,
    foreach,
    reduce,
    toArray,
    toList,
    toSet,
    reverse,
    equals,
  };
};

export interface LinkedListConstructor {
  <T>(values?: T[]): LinkedList<T>;
  fromArray<T>(values: T[]): LinkedList<T>;
  of<T>(...values: T[]): LinkedList<T>;
  empty<T>(): LinkedList<T>;
}

export const LinkedList: LinkedListConstructor = Object.assign(LinkedListImpl, {
  fromArray: <T>(values: T[]): LinkedList<T> => LinkedListImpl(values),
  of: <T>(...values: T[]): LinkedList<T> => LinkedListImpl(values),
  empty: <T>(): LinkedList<T> => LinkedListImpl<T>(),
}) as LinkedListConstructor;

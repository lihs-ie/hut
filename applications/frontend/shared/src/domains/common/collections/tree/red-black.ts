// Red-Black Tree implementation for SortedImmutableMap and SortedImmutableSet

// Comparator type definition
export type Comparator<T> = (left: T, right: T) => number;

// Red-Black Tree node colors
export enum Color {
  RED = 'RED',
  BLACK = 'BLACK',
}

// Red-Black Tree node interface
export interface RBTreeNode<K, V> {
  key: K;
  value: V;
  color: Color;
  left: RBTreeNode<K, V> | null;
  right: RBTreeNode<K, V> | null;
  parent: RBTreeNode<K, V> | null;
}

// Helper function to create RBTree node
export const createNode = <K, V>(
  key: K,
  value: V,
  color: Color = Color.RED,
  left: RBTreeNode<K, V> | null = null,
  right: RBTreeNode<K, V> | null = null,
  parent: RBTreeNode<K, V> | null = null
): RBTreeNode<K, V> => ({
  key,
  value,
  color,
  left,
  right,
  parent,
});

// Helper function to calculate tree size
export const calculateSize = <K, V>(node: RBTreeNode<K, V> | null): number => {
  if (node === null) return 0;
  return 1 + calculateSize(node.left) + calculateSize(node.right);
};

// Helper function to insert into tree (simplified, without full Red-Black balancing)
export const insertNode = <K, V>(
  root: RBTreeNode<K, V> | null,
  key: K,
  value: V,
  comparator: Comparator<K>
): RBTreeNode<K, V> => {
  if (root === null) {
    return createNode(key, value, Color.BLACK); // Root is always black
  }

  const compareResult = comparator(key, root.key);
  if (compareResult < 0) {
    const newLeft = insertNode(root.left, key, value, comparator);
    return createNode(root.key, root.value, root.color, newLeft, root.right, root.parent);
  } else if (compareResult > 0) {
    const newRight = insertNode(root.right, key, value, comparator);
    return createNode(root.key, root.value, root.color, root.left, newRight, root.parent);
  } else {
    // Key already exists, update value
    return createNode(key, value, root.color, root.left, root.right, root.parent);
  }
};

// Helper function to build tree from array
export const buildTreeFromArray = <K, V>(
  items: [K, V][],
  comparator: Comparator<K>
): RBTreeNode<K, V> | null => {
  let root: RBTreeNode<K, V> | null = null;
  for (const [key, value] of items) {
    root = insertNode(root, key, value, comparator);
  }
  return root;
};

// Helper function to build tree from object
export const buildTreeFromObject = <K extends string | number | symbol, V>(
  items: Record<K, V>,
  comparator: Comparator<K>
): RBTreeNode<K, V> | null => {
  let root: RBTreeNode<K, V> | null = null;
  for (const [key, value] of Object.entries(items) as [K, V][]) {
    root = insertNode(root, key, value, comparator);
  }
  return root;
};

// Helper function to collect all key-value pairs in sorted order (in-order traversal)
export const collectInOrder = <K, V>(node: RBTreeNode<K, V> | null): [K, V][] => {
  if (node === null) return [];
  return [...collectInOrder(node.left), [node.key, node.value], ...collectInOrder(node.right)];
};

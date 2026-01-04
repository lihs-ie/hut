import { ImmutableList } from '../list/common';
import { Optional } from '../optional/common';

export interface ImmutableStack<T> {
  push(item: T): ImmutableStack<T>;
  pop(): Optional<[T, ImmutableStack<T>]>;
  peek(): Optional<T>;
  isEmpty(): boolean;
  size(): number;
  toArray(): T[];
  foreach(fn: (item: T, index: number) => void): void;
}

export interface ImmutableStackConstructor {
  <T>(items?: T[]): ImmutableStack<T>;
  fromArray<T>(items: T[]): ImmutableStack<T>;
  of<T>(...items: T[]): ImmutableStack<T>;
  empty<T>(): ImmutableStack<T>;
}

const ImmutableStackImpl = <T>(items: T[] = []): ImmutableStack<T> => {
  const list = ImmutableList<T>(items);

  return {
    push(item: T): ImmutableStack<T> {
      return ImmutableStackImpl(list.addFirst(item).toArray());
    },

    pop(): Optional<[T, ImmutableStack<T>]> {
      if (list.isEmpty()) {
        return Optional<[T, ImmutableStack<T>]>();
      }
      const first = list.first();
      const rest = list.drop(1);
      return first.map(value => [value, ImmutableStackImpl(rest.toArray())]);
    },

    peek(): Optional<T> {
      return list.first();
    },

    isEmpty(): boolean {
      return list.isEmpty();
    },

    size(): number {
      return list.size();
    },

    toArray(): T[] {
      return list.toArray();
    },

    foreach(fn: (item: T, index: number) => void): void {
      list.foreach((item, index) => fn(item, index));
    },
  };
};

export const ImmutableStack: ImmutableStackConstructor = Object.assign(ImmutableStackImpl, {
  fromArray: <T>(items: T[]): ImmutableStack<T> => ImmutableStackImpl(items),
  of: <T>(...items: T[]): ImmutableStack<T> => ImmutableStackImpl(items),
  empty: <T>(): ImmutableStack<T> => ImmutableStackImpl<T>(),
}) as ImmutableStackConstructor;

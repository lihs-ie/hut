import { ImmutableList } from '../list/common';
import { Optional } from '../optional/common';

export interface ImmutableQueue<T> {
  enqueue(item: T): ImmutableQueue<T>;
  dequeue(): Optional<[T, ImmutableQueue<T>]>;
  peek(): Optional<T>;
  isEmpty(): boolean;
  size(): number;
  toArray(): T[];
  foreach(fn: (item: T, index: number) => void): void;
}

export interface ImmutableQueueConstructor {
  <T>(items?: T[]): ImmutableQueue<T>;
  fromArray<T>(items: T[]): ImmutableQueue<T>;
  of<T>(...items: T[]): ImmutableQueue<T>;
  empty<T>(): ImmutableQueue<T>;
}

const ImmutableQueueImpl = <T>(items: T[] = []): ImmutableQueue<T> => {
  const list = ImmutableList<T>(items);

  return {
    enqueue(item: T): ImmutableQueue<T> {
      return ImmutableQueueImpl(list.addLast(item).toArray());
    },

    dequeue(): Optional<[T, ImmutableQueue<T>]> {
      if (list.isEmpty()) {
        return Optional<[T, ImmutableQueue<T>]>();
      }
      const first = list.first();
      const rest = list.drop(1);
      return first.map(value => [value, ImmutableQueueImpl(rest.toArray())]);
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

export const ImmutableQueue: ImmutableQueueConstructor = Object.assign(ImmutableQueueImpl, {
  fromArray: <T>(items: T[]): ImmutableQueue<T> => ImmutableQueueImpl(items),
  of: <T>(...items: T[]): ImmutableQueue<T> => ImmutableQueueImpl(items),
  empty: <T>(): ImmutableQueue<T> => ImmutableQueueImpl<T>(),
}) as ImmutableQueueConstructor;

import { hash as hashFunction } from "hash-it";

export const hash = <T>(value: T): string => {
  return hashFunction(value).toString();
};

export const equals = <T>(left: T, right: T): boolean => {
  return hash(left) === hash(right);
};

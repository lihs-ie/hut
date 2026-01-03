export type Command<T> = {
  identifier: string;
  sentAt: Date;
  payload: T;
};

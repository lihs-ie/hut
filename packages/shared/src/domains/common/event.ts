export type Event<P> = {
  identifier: string;
  occurredAt: Date;
  payload: P;
};

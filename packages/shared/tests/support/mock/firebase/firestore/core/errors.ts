
export type FirestoreErrorCode = 
  | 'cancelled'
  | 'unknown'
  | 'invalid-argument'
  | 'deadline-exceeded'
  | 'not-found'
  | 'already-exists'
  | 'permission-denied'
  | 'resource-exhausted'
  | 'failed-precondition'
  | 'aborted'
  | 'out-of-range'
  | 'unimplemented'
  | 'internal'
  | 'unavailable'
  | 'data-loss'
  | 'unauthenticated';

export class FirestoreError extends Error {
  constructor(
    public readonly code: FirestoreErrorCode,
    message: string
  ) {
    super(message);
    this.name = 'FirestoreError';
  }
}

export function createNotFoundError(path: string): FirestoreError {
  return new FirestoreError('not-found', `No document to update: ${path}`);
}

export function createInvalidArgumentError(message: string): FirestoreError {
  return new FirestoreError('invalid-argument', message);
}

export function createFailedPreconditionError(message: string): FirestoreError {
  return new FirestoreError('failed-precondition', message);
}
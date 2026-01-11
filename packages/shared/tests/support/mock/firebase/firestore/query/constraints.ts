
import {
  DocumentSnapshot,
  FieldPath,
  OrderByDirection,
  QueryCompositeFilterConstraint,
  QueryConstraint,
  QueryEndAtConstraint,
  QueryFieldFilterConstraint,
  QueryLimitConstraint,
  QueryOrderByConstraint,
  QueryStartAtConstraint,
  WhereFilterOp
} from "../../types/firestore"

export class QueryFieldFilterConstraintImpl extends QueryFieldFilterConstraint {
  constructor(
    public readonly field: string | FieldPath,
    public readonly op: WhereFilterOp,
    public readonly value: unknown
  ) {
    super()
  }
}

export class QueryCompositeFilterConstraintImpl extends QueryCompositeFilterConstraint {
  constructor(
    type: "or" | "and",
    public readonly filters: QueryFieldFilterConstraintImpl[]
  ) {
    super(type)
  }
}

export class QueryOrderByConstraintImpl extends QueryOrderByConstraint {
  constructor(
    public readonly field: string | FieldPath,
    public readonly direction: OrderByDirection = "asc"
  ) {
    super()
  }
}

export class QueryLimitConstraintImpl extends QueryLimitConstraint {
  constructor(
    type: "limit" | "limitToLast",
    public readonly count: number
  ) {
    super(type)
  }
}

export class QueryStartAtConstraintImpl extends QueryStartAtConstraint {
  constructor(
    type: "startAt" | "startAfter",
    public readonly values: unknown[],
    public readonly snapshot?: DocumentSnapshot
  ) {
    super(type)
  }
}

export class QueryEndAtConstraintImpl extends QueryEndAtConstraint {
  constructor(
    type: "endBefore" | "endAt",
    public readonly values: unknown[],
    public readonly snapshot?: DocumentSnapshot
  ) {
    super(type)
  }
}

export function where(
  fieldPath: string | FieldPath,
  opStr: WhereFilterOp,
  value: unknown
): QueryFieldFilterConstraintImpl {
  return new QueryFieldFilterConstraintImpl(fieldPath, opStr, value)
}

export function orderBy(
  fieldPath: string | FieldPath,
  directionStr?: OrderByDirection
): QueryOrderByConstraintImpl {
  return new QueryOrderByConstraintImpl(fieldPath, directionStr)
}

export function limit(limit: number): QueryLimitConstraintImpl {
  return new QueryLimitConstraintImpl("limit", limit)
}

export function limitToLast(limit: number): QueryLimitConstraintImpl {
  return new QueryLimitConstraintImpl("limitToLast", limit)
}

export function startAt(...fieldValues: unknown[]): QueryStartAtConstraintImpl
export function startAt<T>(
  snapshot: DocumentSnapshot<T>
): QueryStartAtConstraintImpl
export function startAt<T>(...args: unknown[]): QueryStartAtConstraintImpl {
  if (
    args.length === 1 &&
    args[0] &&
    typeof args[0] === "object" &&
    "exists" in args[0]
  ) {
    return new QueryStartAtConstraintImpl(
      "startAt",
      [],
      args[0] as DocumentSnapshot
    )
  }
  return new QueryStartAtConstraintImpl("startAt", args)
}

export function startAfter(
  ...fieldValues: unknown[]
): QueryStartAtConstraintImpl
export function startAfter<T>(
  snapshot: DocumentSnapshot<T>
): QueryStartAtConstraintImpl
export function startAfter<T>(...args: unknown[]): QueryStartAtConstraintImpl {
  if (
    args.length === 1 &&
    args[0] &&
    typeof args[0] === "object" &&
    "exists" in args[0]
  ) {
    return new QueryStartAtConstraintImpl(
      "startAfter",
      [],
      args[0] as DocumentSnapshot
    )
  }
  return new QueryStartAtConstraintImpl("startAfter", args)
}

export function endAt(...fieldValues: unknown[]): QueryEndAtConstraintImpl
export function endAt<T>(
  snapshot: DocumentSnapshot<T>
): QueryEndAtConstraintImpl
export function endAt<T>(...args: unknown[]): QueryEndAtConstraintImpl {
  if (
    args.length === 1 &&
    args[0] &&
    typeof args[0] === "object" &&
    "exists" in args[0]
  ) {
    return new QueryEndAtConstraintImpl(
      "endAt",
      [],
      args[0] as DocumentSnapshot
    )
  }
  return new QueryEndAtConstraintImpl("endAt", args)
}

export function endBefore(...fieldValues: unknown[]): QueryEndAtConstraintImpl
export function endBefore<T>(
  snapshot: DocumentSnapshot<T>
): QueryEndAtConstraintImpl
export function endBefore<T>(...args: unknown[]): QueryEndAtConstraintImpl {
  if (
    args.length === 1 &&
    args[0] &&
    typeof args[0] === "object" &&
    "exists" in args[0]
  ) {
    return new QueryEndAtConstraintImpl(
      "endBefore",
      [],
      args[0] as DocumentSnapshot
    )
  }
  return new QueryEndAtConstraintImpl("endBefore", args)
}

export function and(
  ...queryConstraints: QueryFieldFilterConstraintImpl[]
): QueryCompositeFilterConstraintImpl {
  return new QueryCompositeFilterConstraintImpl("and", queryConstraints)
}

export function or(
  ...queryConstraints: QueryFieldFilterConstraintImpl[]
): QueryCompositeFilterConstraintImpl {
  return new QueryCompositeFilterConstraintImpl("or", queryConstraints)
}

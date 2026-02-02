
import { DocumentData, FieldPath, WhereFilterOp } from "../../types/firestore"
import {
  QueryCompositeFilterConstraintImpl,
  QueryEndAtConstraintImpl,
  QueryFieldFilterConstraintImpl,
  QueryLimitConstraintImpl,
  QueryOrderByConstraintImpl,
  QueryStartAtConstraintImpl
} from "./constraints"

export interface DocumentEntry {
  id: string
  path: string
  data: DocumentData
}

export class QueryEngine {
  static applyFilters(
    documents: DocumentEntry[],
    constraints: Array<
      QueryFieldFilterConstraintImpl | QueryCompositeFilterConstraintImpl
    >
  ): DocumentEntry[] {
    let result = documents

    for (const constraint of constraints) {
      if (constraint instanceof QueryFieldFilterConstraintImpl) {
        result = this.applyFieldFilter(result, constraint)
      } else if (constraint instanceof QueryCompositeFilterConstraintImpl) {
        result = this.applyCompositeFilter(result, constraint)
      }
    }

    return result
  }

  static applyOrderBy(
    documents: DocumentEntry[],
    constraints: QueryOrderByConstraintImpl[]
  ): DocumentEntry[] {
    if (constraints.length === 0) return documents

    return documents.slice().sort((a, b) => {
      for (const orderBy of constraints) {
        const fieldPath = this.getFieldPath(orderBy.field)
        const valueA = this.getNestedValue(a.data, fieldPath)
        const valueB = this.getNestedValue(b.data, fieldPath)

        const comparison = this.compareValues(valueA, valueB)

        if (comparison !== 0) {
          return orderBy.direction === "desc" ? -comparison : comparison
        }
      }
      return 0
    })
  }

  static applyLimit(
    documents: DocumentEntry[],
    constraint: QueryLimitConstraintImpl
  ): DocumentEntry[] {
    if (constraint.type === "limitToLast") {
      return documents.slice(-constraint.count)
    }
    return documents.slice(0, constraint.count)
  }

  static applyStartAt(
    documents: DocumentEntry[],
    constraint: QueryStartAtConstraintImpl,
    orderByConstraints: QueryOrderByConstraintImpl[]
  ): DocumentEntry[] {
    if (constraint.values.length === 0) return documents

    const startIndex = this.findStartIndex(
      documents,
      constraint,
      orderByConstraints
    )
    const inclusive = constraint.type === "startAt"

    return documents.slice(inclusive ? startIndex : startIndex + 1)
  }

  static applyEndAt(
    documents: DocumentEntry[],
    constraint: QueryEndAtConstraintImpl,
    orderByConstraints: QueryOrderByConstraintImpl[]
  ): DocumentEntry[] {
    if (constraint.values.length === 0) return documents

    const endIndex = this.findEndIndex(
      documents,
      constraint,
      orderByConstraints
    )
    const inclusive = constraint.type === "endAt"

    return documents.slice(0, inclusive ? endIndex + 1 : endIndex)
  }

  private static applyFieldFilter(
    documents: DocumentEntry[],
    constraint: QueryFieldFilterConstraintImpl
  ): DocumentEntry[] {
    const fieldPath = this.getFieldPath(constraint.field)

    return documents.filter((doc) => {
      const fieldValue = this.getNestedValue(doc.data, fieldPath)
      return this.evaluateFilter(fieldValue, constraint.op, constraint.value)
    })
  }

  private static applyCompositeFilter(
    documents: DocumentEntry[],
    constraint: QueryCompositeFilterConstraintImpl
  ): DocumentEntry[] {
    return documents.filter((doc) => {
      if (constraint.type === "and") {
        return constraint.filters.every((filter) => {
          const fieldPath = this.getFieldPath(filter.field)
          const fieldValue = this.getNestedValue(doc.data, fieldPath)
          return this.evaluateFilter(fieldValue, filter.op, filter.value)
        })
      } else {
        return constraint.filters.some((filter) => {
          const fieldPath = this.getFieldPath(filter.field)
          const fieldValue = this.getNestedValue(doc.data, fieldPath)
          return this.evaluateFilter(fieldValue, filter.op, filter.value)
        })
      }
    })
  }

  private static evaluateFilter(
    fieldValue: unknown,
    op: WhereFilterOp,
    filterValue: unknown
  ): boolean {
    switch (op) {
      case "==":
        return this.isEqual(fieldValue, filterValue)
      case "!=":
        return !this.isEqual(fieldValue, filterValue)
      case ">":
        return this.compareValues(fieldValue, filterValue) > 0
      case ">=":
        return this.compareValues(fieldValue, filterValue) >= 0
      case "<":
        return this.compareValues(fieldValue, filterValue) < 0
      case "<=":
        return this.compareValues(fieldValue, filterValue) <= 0
      case "in":
        return (
          Array.isArray(filterValue) &&
          filterValue.some((v) => this.isEqual(fieldValue, v))
        )
      case "not-in":
        return (
          !Array.isArray(filterValue) ||
          !filterValue.some((v) => this.isEqual(fieldValue, v))
        )
      case "array-contains":
        return (
          Array.isArray(fieldValue) &&
          fieldValue.some((v) => this.isEqual(v, filterValue))
        )
      case "array-contains-any":
        return (
          Array.isArray(fieldValue) &&
          Array.isArray(filterValue) &&
          fieldValue.some((v) => filterValue.some((fv) => this.isEqual(v, fv)))
        )
      default:
        return false
    }
  }

  private static getFieldPath(field: string | FieldPath): string {
    return field instanceof FieldPath ? field.toString() : field
  }

  private static getNestedValue(obj: Record<string, unknown>, path: string): unknown {
    const keys = path.split(".")
    let current: unknown = obj

    for (const key of keys) {
      if (current === null || current === undefined) return undefined
      if (typeof current !== "object") return undefined
      current = (current as Record<string, unknown>)[key]
    }

    return current
  }

  private static isEqual(a: unknown, b: unknown): boolean {
    if (a === b) return true
    if (a === null || b === null) return false
    if (a === undefined || b === undefined) return false

    if (Array.isArray(a) && Array.isArray(b)) {
      if (a.length !== b.length) return false
      return a.every((item, index) => this.isEqual(item, b[index]))
    }

    if (typeof a === "object" && typeof b === "object") {
      const keysA = Object.keys(a as object)
      const keysB = Object.keys(b as object)
      if (keysA.length !== keysB.length) return false
      return keysA.every((key) =>
        this.isEqual((a as Record<string, unknown>)[key], (b as Record<string, unknown>)[key])
      )
    }

    return false
  }

  private static compareValues(a: unknown, b: unknown): number {
    if (a == null && b == null) return 0
    if (a == null) return -1
    if (b == null) return 1

    if (typeof a !== typeof b) {
      return typeof a < typeof b ? -1 : 1
    }

    if (typeof a === "number" && typeof b === "number") {
      return a - b
    }

    if (typeof a === "string" && typeof b === "string") {
      return a.localeCompare(b)
    }

    if (typeof a === "boolean" && typeof b === "boolean") {
      return a === b ? 0 : a ? 1 : -1
    }

    return String(a).localeCompare(String(b))
  }

  private static findStartIndex(
    documents: DocumentEntry[],
    constraint: QueryStartAtConstraintImpl,
    orderByConstraints: QueryOrderByConstraintImpl[]
  ): number {
    for (let i = 0; i < documents.length; i++) {
      const doc = documents[i]
      if (
        doc &&
        this.matchesStartCondition(doc, constraint, orderByConstraints)
      ) {
        return i
      }
    }
    return documents.length
  }

  private static findEndIndex(
    documents: DocumentEntry[],
    constraint: QueryEndAtConstraintImpl,
    orderByConstraints: QueryOrderByConstraintImpl[]
  ): number {
    for (let i = documents.length - 1; i >= 0; i--) {
      const doc = documents[i]
      if (
        doc &&
        this.matchesEndCondition(doc, constraint, orderByConstraints)
      ) {
        return i
      }
    }
    return -1
  }

  private static matchesStartCondition(
    document: DocumentEntry,
    constraint: QueryStartAtConstraintImpl,
    orderByConstraints: QueryOrderByConstraintImpl[]
  ): boolean {
    if (constraint.values.length === 0) return true
    if (orderByConstraints.length === 0) return true

    const orderByField = orderByConstraints[0]
    if (!orderByField) return true
    const fieldPath = this.getFieldPath(orderByField.field)
    const documentValue = this.getNestedValue(document.data, fieldPath)
    const comparison = this.compareValues(documentValue, constraint.values[0])

    return comparison >= 0
  }

  private static matchesEndCondition(
    document: DocumentEntry,
    constraint: QueryEndAtConstraintImpl,
    orderByConstraints: QueryOrderByConstraintImpl[]
  ): boolean {
    if (constraint.values.length === 0) return true
    if (orderByConstraints.length === 0) return true

    const orderByField = orderByConstraints[0]
    if (!orderByField) return true
    const fieldPath = this.getFieldPath(orderByField.field)
    const documentValue = this.getNestedValue(document.data, fieldPath)
    const comparison = this.compareValues(documentValue, constraint.values[0])

    return comparison <= 0
  }
}

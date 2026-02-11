
import {
  CollectionReference,
  DocumentData,
  DocumentReference,
  Firestore,
  FirestoreDataConverter,
  Query
} from "../../types/firestore"
import { FirestoreImpl } from "../database/firestore-impl"

export class DocumentReferenceImpl<T = DocumentData>
  implements DocumentReference<T>
{
  readonly type = "document" as const

  constructor(
    readonly firestore: FirestoreImpl,
    readonly path: string,
    readonly converter: FirestoreDataConverter<T> | null = null
  ) {}

  get id(): string {
    const segments = this.path.split("/")
    return segments[segments.length - 1] || ""
  }

  get parent(): CollectionReference<T> {
    const segments = this.path.split("/")
    const parentPath = segments.slice(0, -1).join("/")
    return new CollectionReferenceImpl(
      this.firestore,
      parentPath,
      this.converter
    )
  }

  withConverter<
    NewAppModelType,
    NewDbModelType extends DocumentData = DocumentData
  >(
    converter: FirestoreDataConverter<NewAppModelType, NewDbModelType>
  ): DocumentReference<NewAppModelType>
  withConverter(converter: null): DocumentReference<DocumentData>
  withConverter<
    NewAppModelType,
    NewDbModelType extends DocumentData = DocumentData
  >(
    converter: FirestoreDataConverter<NewAppModelType, NewDbModelType> | null
  ): DocumentReference<NewAppModelType> {
    return new DocumentReferenceImpl<NewAppModelType>(
      this.firestore,
      this.path,
      converter
    )
  }

  toJSON(): object {
    return {
      type: this.type,
      path: this.path,
      id: this.id
    }
  }
}

export class CollectionReferenceImpl<T = DocumentData>
  implements CollectionReference<T>, Query<T>
{
  readonly type = "collection" as const

  constructor(
    readonly firestore: FirestoreImpl,
    readonly path: string,
    readonly converter: FirestoreDataConverter<T> | null = null
  ) {}

  get id(): string {
    const segments = this.path.split("/")
    return segments[segments.length - 1] || ""
  }

  get parent(): DocumentReference | null {
    const segments = this.path.split("/")
    if (segments.length <= 1) return null

    const parentPath = segments.slice(0, -1).join("/")
    return new DocumentReferenceImpl(this.firestore, parentPath)
  }

  withConverter<
    NewAppModelType,
    NewDbModelType extends DocumentData = DocumentData
  >(
    converter: FirestoreDataConverter<NewAppModelType, NewDbModelType>
  ): CollectionReference<NewAppModelType>
  withConverter(converter: null): CollectionReference<DocumentData>
  withConverter<
    NewAppModelType,
    NewDbModelType extends DocumentData = DocumentData
  >(
    converter: FirestoreDataConverter<NewAppModelType, NewDbModelType> | null
  ): CollectionReference<NewAppModelType> {
    return new CollectionReferenceImpl<NewAppModelType>(
      this.firestore,
      this.path,
      converter
    )
  }
}

export function collection(
  firestore: Firestore,
  path: string,
  ...pathSegments: string[]
): CollectionReferenceImpl
export function collection(
  reference: DocumentReference | CollectionReference,
  path: string,
  ...pathSegments: string[]
): CollectionReferenceImpl
export function collection(
  firestoreOrReference: Firestore | DocumentReference | CollectionReference,
  path: string,
  ...pathSegments: string[]
): CollectionReferenceImpl {
  let firestore: FirestoreImpl
  let fullPath: string

  if ("firestore" in firestoreOrReference) {
    firestore = firestoreOrReference.firestore as FirestoreImpl
    fullPath = `${firestoreOrReference.path}/${path}`
  } else {
    firestore = firestoreOrReference as FirestoreImpl
    fullPath = path
  }

  if (pathSegments.length > 0) {
    fullPath = `${fullPath}/${pathSegments.join("/")}`
  }

  return new CollectionReferenceImpl(firestore, fullPath)
}

export function doc(
  firestore: Firestore,
  path: string,
  ...pathSegments: string[]
): DocumentReferenceImpl
export function doc(
  reference: CollectionReference,
  path?: string,
  ...pathSegments: string[]
): DocumentReferenceImpl
export function doc(
  reference: DocumentReference,
  path: string,
  ...pathSegments: string[]
): DocumentReferenceImpl
export function doc(
  firestoreOrReference: Firestore | DocumentReference | CollectionReference,
  path?: string,
  ...pathSegments: string[]
): DocumentReferenceImpl {
  let firestore: FirestoreImpl
  let fullPath: string
  let converter: FirestoreDataConverter<unknown> | null = null

  if ("firestore" in firestoreOrReference) {
    firestore = firestoreOrReference.firestore as FirestoreImpl

    if ("converter" in firestoreOrReference) {
      converter = firestoreOrReference.converter
    }

    if (path) {
      fullPath = `${firestoreOrReference.path}/${path}`
    } else {
      const autoId = generateAutoId()
      fullPath = `${firestoreOrReference.path}/${autoId}`
    }
  } else {
    firestore = firestoreOrReference as FirestoreImpl
    fullPath = path || ""
  }

  if (pathSegments.length > 0) {
    fullPath = `${fullPath}/${pathSegments.join("/")}`
  }

  return new DocumentReferenceImpl(firestore, fullPath, converter)
}

function generateAutoId(): string {
  const chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  let result = ""
  for (let i = 0; i < 20; i++) {
    result += chars.charAt(Math.floor(Math.random() * chars.length))
  }
  return result
}

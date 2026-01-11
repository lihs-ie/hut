
import { TransactionFunction } from "../../types/firestore"
import { FirestoreImpl } from "../database/firestore-impl"
import { WriteBatchImpl } from "./batch-impl"
import { TransactionImpl } from "./transaction-impl"

export async function runTransaction<T>(
  firestore: FirestoreImpl,
  updateFunction: TransactionFunction<T>
): Promise<T> {
  const maxRetries = 5
  let retryCount = 0

  while (retryCount < maxRetries) {
    try {
      const transaction = new TransactionImpl(firestore)

      const result = await updateFunction(transaction)

      await transaction._commit()

      return result
    } catch (error) {
      retryCount++

      if (retryCount >= maxRetries) {
        throw error
      }

      await new Promise((resolve) => setTimeout(resolve, 50 * retryCount))
    }
  }

  throw new Error("Transaction failed after maximum retries")
}

export function writeBatch(firestore: FirestoreImpl): WriteBatchImpl {
  return new WriteBatchImpl(firestore)
}

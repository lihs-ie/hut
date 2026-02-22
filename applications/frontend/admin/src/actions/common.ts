"use server";

import { unwrapForNextJs } from "@shared/components/global/next-error";
import { AdminImageUploaderProvider } from "../providers/infrastructure/storage";

export async function uploadImage(
  file: File | Blob,
  path: string,
): Promise<string> {
  return unwrapForNextJs(
    AdminImageUploaderProvider.firebaseAdmin.upload(file, path),
  );
}

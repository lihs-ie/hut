"use server";

import { unwrapForNextJs } from "@shared/components/global/next-error";
import { ImageUploaderProvider } from "@shared/providers/infrastructure/common";

export async function uploadImage(
  file: File | Blob,
  path: string,
): Promise<string> {
  return unwrapForNextJs(ImageUploaderProvider.firebase.upload(file, path));
}

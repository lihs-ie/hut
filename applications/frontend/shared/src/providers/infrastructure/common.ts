import { FirebaseProvider } from "./firebase";
import type { ImageUploader } from "@shared/domains/common/image";

export const ImageUploaderProvider = {
  get firebase(): ImageUploader {
    return FirebaseProvider.storage.imageUploader;
  },
};

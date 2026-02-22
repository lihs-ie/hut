import { ImageUploader } from "@shared/domains/common/image";
import { FirebaseAdminStorageImageUploader } from "../../infrastructure/storage";
import { FirebaseAdminProvider } from "../auth/admin";

let cachedUploader: ImageUploader | null = null;

const getUploader = (): ImageUploader => {
  if (cachedUploader === null) {
    cachedUploader = FirebaseAdminStorageImageUploader(
      FirebaseAdminProvider.storage.instance,
    );
  }
  return cachedUploader;
};

export const AdminImageUploaderProvider = {
  get firebaseAdmin(): ImageUploader {
    return getUploader();
  },
};

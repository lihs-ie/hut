import { FirebaseStorageImageUploader } from "@shared/infrastructures/common";
import { FirebaseProvider } from "./firebase";

export const ImageUploaderProvider = {
  firebase: FirebaseStorageImageUploader(FirebaseProvider.storage.instance),
};

import { FirebaseAdminSdkAdminRepository } from "../../infrastructure/admin";
import { FirebaseAdminProvider } from "../auth/admin";
import type { AdminRepository } from "@shared/domains/user";

let cachedRepository: AdminRepository | null = null;

export const AdminRepositoryProvider = {
  get firebase(): AdminRepository {
    if (cachedRepository === null) {
      cachedRepository = FirebaseAdminSdkAdminRepository(
        FirebaseAdminProvider.firestore.instance,
      );
    }
    return cachedRepository;
  },
};

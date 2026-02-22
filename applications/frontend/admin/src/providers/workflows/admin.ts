import {
  createAdminFindWorkflow,
  createPersistProfileWorkflow,
} from "@shared/workflows/admin";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";
import { AdminRepositoryProvider } from "@shared/providers/infrastructure/admin";
import { validateProfile } from "@shared/domains/user";
import { imageSchema } from "@shared/domains/common/image";
import { ok } from "@shared/aspects/result";
import { AdminImageUploaderProvider } from "../infrastructure/storage";

const findWorkflow = createAdminFindWorkflow(LoggerProvider.console)(
  AdminRepositoryProvider.firebase.find,
);

export const AdminWorkflowProvider = {
  find: findWorkflow,

  persistProfile: createPersistProfileWorkflow(validateProfile)((avatar) => {
    if (avatar.startsWith("data:")) {
      return AdminImageUploaderProvider.firebaseAdmin.upload(
        avatar,
        "admin/avatar",
      );
    }
    return ok(imageSchema.parse(avatar)).toAsync();
  })(LoggerProvider.console)(findWorkflow)(
    AdminRepositoryProvider.firebase.persist,
  ),
} as const;

import {
  createAdminFindWorkflow,
  createPersistProfileWorkflow,
} from "@shared/workflows/admin";
import { LoggerProvider } from "../infrastructure/logger";
import { AdminRepositoryProvider } from "../infrastructure/admin";
import { validateProfile } from "@shared/domains/user";
import { ImageUploaderProvider } from "../infrastructure/common";

export const AdminWorkflowProvider = {
  find: createAdminFindWorkflow(LoggerProvider.console)(
    AdminRepositoryProvider.firebase.find,
  ),

  persistProfile: createPersistProfileWorkflow(validateProfile)((avatar) =>
    ImageUploaderProvider.firebase.upload(avatar, "admin/avatar"),
  )(LoggerProvider.console)(
    createAdminFindWorkflow(LoggerProvider.console)(
      AdminRepositoryProvider.firebase.find,
    ),
  )(AdminRepositoryProvider.firebase.persist),
} as const;

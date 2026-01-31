import { AsyncResult, combineAsync, Result } from "@shared/aspects/result";
import { Command } from "./common";
import {
  Admin,
  Profile,
  UnvalidatedProfile,
  validateAdmin,
} from "@shared/domains/user";
import {
  AggregateNotFoundError,
  UnexpectedError,
  ValidationError,
} from "@shared/aspects/error";
import { Logger } from "@shared/aspects/logger";
import { Image } from "@shared/domains/common/image";

type AdminFindWorkflow = (
  command: Command<null>,
) => AsyncResult<Admin, AggregateNotFoundError<"Admin"> | UnexpectedError>;

type Find = () => AsyncResult<
  Admin,
  AggregateNotFoundError<"Admin"> | UnexpectedError
>;

export const createAdminFindWorkflow =
  (logger: Logger) =>
  (find: Find): AdminFindWorkflow => {
    return (command: Command<null>) => {
      logger.info("AdminFindWorkflow started", { command });

      return find()
        .tap((admin) => {
          logger.info("AdminFindWorkflow completed", {
            identifier: admin.identifier,
          });
        })
        .tapError((error) => {
          logger.error("AdminFindWorkflow failed", { error, command });
        });
    };
  };

export type PersistProfileCommand = Command<UnvalidatedProfile>;

export type PersistProfileWorkflow = (
  command: PersistProfileCommand,
) => AsyncResult<
  void,
  ValidationError[] | UnexpectedError | AggregateNotFoundError<"Admin">
>;

type ValidateProfile = (
  unvalidated: UnvalidatedProfile,
) => Result<Profile, ValidationError[]>;

type PersistAdmin = (
  admin: Admin,
) => AsyncResult<void, UnexpectedError | AggregateNotFoundError<"Admin">>;

type UploadAvatar = (avatar: string) => AsyncResult<Image, UnexpectedError>;

export const createPersistProfileWorkflow =
  (validate: ValidateProfile) =>
  (uploadAvatar: UploadAvatar) =>
  (logger: Logger) =>
  (find: AdminFindWorkflow) =>
  (persist: PersistAdmin): PersistProfileWorkflow =>
  (command: PersistProfileCommand) => {
    logger.info("PersistProfileWorkflow started", { command });

    return validate(command.payload)
      .toAsync()
      .andThen((profile) =>
        combineAsync([
          uploadAvatar(profile.avatar),
          find({ payload: null, now: command.now }),
        ] as const),
      )
      .andThen(([avatar, admin]) => {
        const updatedProfile: UnvalidatedProfile = {
          ...command.payload,
          avatar,
        };

        return validateAdmin({
          identifier: admin.identifier,
          profile: updatedProfile,
        }).toAsync();
      })
      .andThen((updatedAdmin) => persist(updatedAdmin))
      .tap(() => {
        logger.info("PersistProfileWorkflow completed", { command });
      })
      .tapError((error) => {
        logger.error("PersistProfileWorkflow failed", { error, command });
      });
  };

import {
  AggregateNotFoundError,
  UnexpectedError,
  ValidationError,
} from "@shared/aspects/error";
import { AsyncResult, ok } from "@shared/aspects/result";
import {
  SiteDocument,
  SiteDocumentRepository,
  updatePrivacy,
} from "@shared/domains/document";
import {
  PrivacyPolicy,
  UnvalidatedPrivacyPolicySection,
} from "@shared/domains/document/privacy";
import { Logger } from "@shared/aspects/logger";
import { Command } from "./common";

export type GetDocumentWorkflow = () => AsyncResult<SiteDocument, Error>;

type FindSiteDocument = () => AsyncResult<
  SiteDocument,
  AggregateNotFoundError<"SiteDocument"> | UnexpectedError
>;

type GetPrivacyPolicyCommand = Command<null>;

export type PrivacyPolicyFindWorkflow = (
  command: GetPrivacyPolicyCommand,
) => AsyncResult<
  PrivacyPolicy,
  AggregateNotFoundError<"SiteDocument"> | UnexpectedError
>;

export const createGetPrivacyPolicyWorkflow =
  (find: FindSiteDocument) =>
  (logger: Logger): PrivacyPolicyFindWorkflow =>
  (command: GetPrivacyPolicyCommand) => {
    logger.info("PrivacyPolicyFindWorkflow started", { command });

    return find()
      .map((document) => document.privacy)
      .tap((privacy) => {
        logger.info("PrivacyPolicyFindWorkflow completed", {
          sectionCount: privacy.sections.length,
        });
      })
      .tapError((error) => {
        logger.error("PrivacyPolicyFindWorkflow failed", { error });
      });
  };

type PersistSiteDocument = SiteDocumentRepository["persist"];

export type PrivacyPolicyPersistPayload = {
  sections: UnvalidatedPrivacyPolicySection[];
};

export type PrivacyPolicyPersistWorkflow = (
  command: Command<PrivacyPolicyPersistPayload>,
) => AsyncResult<
  void,
  AggregateNotFoundError<"SiteDocument"> | UnexpectedError | ValidationError[]
>;

export const createPrivacyPolicyPersistWorkflow =
  (find: FindSiteDocument) =>
  (persist: PersistSiteDocument) =>
  (logger: Logger): PrivacyPolicyPersistWorkflow =>
  (command: Command<PrivacyPolicyPersistPayload>) => {
    logger.info("PrivacyPolicyPersistWorkflow started", {
      sectionCount: command.payload.sections.length,
    });

    return find()
      .tap((document) => {
        logger.debug("SiteDocument found", {
          currentSectionCount: document.privacy.sections.length,
        });
      })
      .andThen((currentDocument) => {
        const now = new Date();
        const updateResult = updatePrivacy({
          sections: command.payload.sections,
          timeline: {
            createdAt: currentDocument.privacy.timeline.createdAt,
            updatedAt: now,
          },
        });

        return updateResult.toAsync();
      })
      .tap((document) => {
        logger.debug("PrivacyPolicy validated", {
          sectionCount: document.privacy.sections.length,
        });
      })
      .tapError((errors) => {
        logger.warn("PrivacyPolicy validation failed", { errors });
      })
      .andThen((document) =>
        persist(document).tap(() => {
          logger.debug("SiteDocument persisted");
        }),
      )
      .andThen(() => ok(undefined))
      .tap(() => {
        logger.info("PrivacyPolicyPersistWorkflow completed");
      })
      .tapError((error) => {
        logger.error("PrivacyPolicyPersistWorkflow failed", { error });
      });
  };

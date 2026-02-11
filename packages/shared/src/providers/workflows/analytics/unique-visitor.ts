import { validateUniqueVisitor } from "@shared/domains/analytics/unique-visitor";
import { validatePeriod } from "@shared/domains/analytics/common";
import {
  createRecordUniqueVisitorWorkflow,
  createGetUniqueVisitorsWorkflow,
} from "@shared/workflows/analytics/unique-visitor";
import { AnalyticsRepositoryProvider } from "../../infrastructure/analytics";
import { LoggerProvider } from "../../infrastructure/logger";

export const UniqueVisitorWorkflowProvider = {
  record: createRecordUniqueVisitorWorkflow(validateUniqueVisitor)(
    AnalyticsRepositoryProvider.uniqueVisitor.persist,
  )(LoggerProvider.console),

  getUniqueVisitors: createGetUniqueVisitorsWorkflow(validatePeriod)(
    AnalyticsRepositoryProvider.uniqueVisitor.search,
  )(LoggerProvider.console),
} as const;

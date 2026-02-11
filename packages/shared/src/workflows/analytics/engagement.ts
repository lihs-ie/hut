import { UnexpectedError, ValidationError } from "@shared/aspects/error";
import {
  AsyncResult,
  Result,
  ok,
  combineAsync,
} from "@shared/aspects/result";
import { Logger } from "@shared/aspects/logger";
import { Command } from "@shared/workflows/common";
import {
  EngagementRecord,
  UnvalidatedEngagementRecord,
  Criteria as EngagementCriteria,
  validateCriteria as validateEngagementCriteria,
} from "@shared/domains/analytics/engagement";
import {
  Period,
  PeriodComparison,
  RankedItem,
  Distribution,
  toJstDateKey,
  resolveDateRange,
  resolvePreviousDateRange,
  validatePeriodComparison,
  validateRankedItem,
  validateDistribution,
} from "@shared/domains/analytics/common";
import {
  EngagementRecordedEvent,
  createEngagementRecordedEvent,
} from "@shared/domains/analytics/event";
import {
  type SearchArticles,
  type SearchMemos,
  buildEmptyArticleCriteria,
  buildEmptyMemoCriteria,
  buildContentTitleMap,
  resolveRankedItemTitles,
} from "./title-resolution";

type RecordEngagementPayload = {
  reference: { type: string; content: string };
  sessionKey: string;
  dwellTime: number;
  scrollDepth: number;
};

type ValidateEngagementRecord = (
  unvalidated: UnvalidatedEngagementRecord
) => Result<EngagementRecord, ValidationError[]>;

type PersistEngagementRecord = (
  record: EngagementRecord
) => AsyncResult<void, UnexpectedError>;

export type RecordEngagementWorkflow = (
  command: Command<RecordEngagementPayload>
) => AsyncResult<EngagementRecordedEvent, ValidationError[] | UnexpectedError>;

export const createRecordEngagementWorkflow =
  (validate: ValidateEngagementRecord) =>
  (persist: PersistEngagementRecord) =>
  (logger: Logger): RecordEngagementWorkflow =>
  (command: Command<RecordEngagementPayload>) => {
    logger.info("RecordEngagementWorkflow started", {
      reference: command.payload.reference,
    });

    const dateKey = toJstDateKey(command.now);

    return validate({
      identifier: {
        reference: command.payload.reference,
        dateKey,
        sessionKey: command.payload.sessionKey,
      },
      dwellTime: command.payload.dwellTime,
      scrollDepth: command.payload.scrollDepth,
      createdAt: command.now,
      updatedAt: command.now,
    })
      .toAsync()
      .tap((record) => {
        logger.debug("EngagementRecord validation passed", {
          identifier: record.identifier,
        });
      })
      .tapError((errors) => {
        logger.warn("EngagementRecord validation failed", { errors });
      })
      .andThen((record) =>
        persist(record)
          .tap(() => {
            logger.debug("EngagementRecord persisted", {
              identifier: record.identifier,
            });
          })
          .andThen(() => ok(record))
      )
      .map((record) => createEngagementRecordedEvent(record, command.now))
      .tap((event) => {
        logger.info("RecordEngagementWorkflow completed", { event });
      })
      .tapError((error) => {
        logger.error("RecordEngagementWorkflow failed", { error });
      });
  };

type ValidatePeriod = (
  candidate: string
) => Result<Period, ValidationError>;

type SearchEngagementRecords = (
  criteria: EngagementCriteria
) => AsyncResult<EngagementRecord[], UnexpectedError>;

export const calculateAverageDwellTime = (records: EngagementRecord[]): number => {
  if (records.length === 0) return 0;
  const total = records.reduce(
    (accumulator, record) => accumulator + record.dwellTime,
    0
  );
  return Math.round(total / records.length);
};

export type GetAverageDwellTimeWorkflow = (
  command: Command<{ period: string }>
) => AsyncResult<PeriodComparison, ValidationError | ValidationError[] | UnexpectedError>;

export const createGetAverageDwellTimeWorkflow =
  (validatePeriod: ValidatePeriod) =>
  (search: SearchEngagementRecords) =>
  (logger: Logger): GetAverageDwellTimeWorkflow =>
  (command: Command<{ period: string }>) => {
    logger.info("GetAverageDwellTimeWorkflow started", {
      period: command.payload.period,
    });

    return validatePeriod(command.payload.period)
      .toAsync()
      .tap((period) => {
        logger.debug("Period validation passed", { period });
      })
      .tapError((error) => {
        logger.warn("Period validation failed", { error });
      })
      .andThen((period) => {
        const currentRange = resolveDateRange(period);
        const previousRange = resolvePreviousDateRange(period);
        const currentCriteria = validateEngagementCriteria({ dateRange: currentRange }).unwrap();
        const previousCriteria = validateEngagementCriteria({ dateRange: previousRange }).unwrap();

        return combineAsync([
          search(currentCriteria),
          search(previousCriteria),
        ] as const).andThen(([currentRecords, previousRecords]) => {
          const currentAverage = calculateAverageDwellTime(currentRecords);
          const previousAverage = calculateAverageDwellTime(previousRecords);

          return validatePeriodComparison({
            current: currentAverage,
            previous: previousAverage,
          }).toAsync();
        });
      })
      .tap((comparison) => {
        logger.info("GetAverageDwellTimeWorkflow completed", { comparison });
      })
      .tapError((error) => {
        logger.error("GetAverageDwellTimeWorkflow failed", { error });
      });
  };

export const aggregateDwellTimeByContent = (
  records: EngagementRecord[]
): RankedItem[] => {
  const contentStats = new Map<
    string,
    { total: number; count: number; type: string }
  >();

  for (const record of records) {
    const key = record.identifier.reference.content;
    const existing = contentStats.get(key);
    if (existing) {
      existing.total += record.dwellTime;
      existing.count += 1;
    } else {
      contentStats.set(key, {
        total: record.dwellTime,
        count: 1,
        type: record.identifier.reference.type,
      });
    }
  }

  const items: RankedItem[] = [];

  for (const [contentIdentifier, stats] of contentStats) {
    items.push(
      validateRankedItem({
        label: contentIdentifier,
        value: Math.round(stats.total / stats.count),
        subLabel: stats.type,
      }).unwrap()
    );
  }

  return items.toSorted(
    (first, second) => second.value - first.value
  );
};

export type GetDwellTimeRankingWorkflow = (
  command: Command<{ period: string }>
) => AsyncResult<RankedItem[], ValidationError | UnexpectedError>;

export const createGetDwellTimeRankingWorkflow =
  (validatePeriod: ValidatePeriod) =>
  (search: SearchEngagementRecords) =>
  (searchArticles: SearchArticles) =>
  (searchMemos: SearchMemos) =>
  (logger: Logger): GetDwellTimeRankingWorkflow =>
  (command: Command<{ period: string }>) => {
    logger.info("GetDwellTimeRankingWorkflow started", {
      period: command.payload.period,
    });

    return validatePeriod(command.payload.period)
      .toAsync()
      .tap((period) => {
        logger.debug("Period validation passed", { period });
      })
      .tapError((error) => {
        logger.warn("Period validation failed", { error });
      })
      .andThen((period) => {
        const dateRange = resolveDateRange(period);
        const criteria = validateEngagementCriteria({ dateRange }).unwrap();
        return combineAsync([
          search(criteria),
          searchArticles(buildEmptyArticleCriteria()),
          searchMemos(buildEmptyMemoCriteria()),
        ] as const);
      })
      .map(([records, articles, memos]) => {
        const ranking = aggregateDwellTimeByContent(records);
        const titleMap = buildContentTitleMap(articles, memos);
        return resolveRankedItemTitles(ranking, titleMap);
      })
      .tap((ranking) => {
        logger.info("GetDwellTimeRankingWorkflow completed", {
          count: ranking.length,
        });
      })
      .tapError((error) => {
        logger.error("GetDwellTimeRankingWorkflow failed", { error });
      });
  };

const SCROLL_DEPTH_BUCKETS = [
  "0-25%",
  "26-50%",
  "51-75%",
  "76-100%",
] as const;

export const aggregateScrollDepth = (records: EngagementRecord[]): Distribution[] => {
  const buckets = new Map<string, number>(
    SCROLL_DEPTH_BUCKETS.map((bucket) => [bucket, 0])
  );

  for (const record of records) {
    const depth = record.scrollDepth;
    if (depth <= 25) {
      buckets.set("0-25%", (buckets.get("0-25%") ?? 0) + 1);
    } else if (depth <= 50) {
      buckets.set("26-50%", (buckets.get("26-50%") ?? 0) + 1);
    } else if (depth <= 75) {
      buckets.set("51-75%", (buckets.get("51-75%") ?? 0) + 1);
    } else {
      buckets.set("76-100%", (buckets.get("76-100%") ?? 0) + 1);
    }
  }

  return SCROLL_DEPTH_BUCKETS.map((label) =>
    validateDistribution({ label, value: buckets.get(label) ?? 0 }).unwrap()
  );
};

export type GetScrollDepthDistributionWorkflow = (
  command: Command<{ period: string }>
) => AsyncResult<Distribution[], ValidationError | UnexpectedError>;

export const createGetScrollDepthDistributionWorkflow =
  (validatePeriod: ValidatePeriod) =>
  (search: SearchEngagementRecords) =>
  (logger: Logger): GetScrollDepthDistributionWorkflow =>
  (command: Command<{ period: string }>) => {
    logger.info("GetScrollDepthDistributionWorkflow started", {
      period: command.payload.period,
    });

    return validatePeriod(command.payload.period)
      .toAsync()
      .tap((period) => {
        logger.debug("Period validation passed", { period });
      })
      .tapError((error) => {
        logger.warn("Period validation failed", { error });
      })
      .andThen((period) => {
        const dateRange = resolveDateRange(period);
        const criteria = validateEngagementCriteria({ dateRange }).unwrap();
        return search(criteria);
      })
      .map((records) => aggregateScrollDepth(records))
      .tap((distribution) => {
        logger.info("GetScrollDepthDistributionWorkflow completed", {
          distribution,
        });
      })
      .tapError((error) => {
        logger.error("GetScrollDepthDistributionWorkflow failed", { error });
      });
  };

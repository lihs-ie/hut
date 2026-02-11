import { ulid } from "ulid";
import { UnexpectedError, ValidationError } from "@shared/aspects/error";
import { AsyncResult, Result, ok, combineAsync } from "@shared/aspects/result";
import { Logger } from "@shared/aspects/logger";
import { Command } from "@shared/workflows/common";
import {
  SearchRecord,
  UnvalidatedSearchRecord,
  Criteria as SearchRecordCriteria,
  validateCriteria as validateSearchRecordCriteria,
} from "@shared/domains/analytics/search-record";
import {
  Period,
  DateKey,
  TrendPoint,
  RankedItem,
  PeriodComparison,
  toJstDateKey,
  resolveDateRange,
  resolvePreviousDateRange,
  validatePeriodComparison,
  validateTrendPoint,
} from "@shared/domains/analytics/common";
import {
  SearchRecordedEvent,
  createSearchRecordedEvent,
} from "@shared/domains/analytics/event";
import { toSortedRankedItems } from "./page-view";

export const aggregateByKeyword = (records: SearchRecord[]): RankedItem[] => {
  const keywordTotals = new Map<string, number>();
  for (const record of records) {
    keywordTotals.set(
      record.keyword,
      (keywordTotals.get(record.keyword) ?? 0) + 1,
    );
  }
  return toSortedRankedItems(keywordTotals);
};

export const aggregateSearchByDate = (records: SearchRecord[]): TrendPoint[] => {
  const dailyTotals = new Map<DateKey, number>();
  for (const record of records) {
    dailyTotals.set(
      record.dateKey,
      (dailyTotals.get(record.dateKey) ?? 0) + 1,
    );
  }
  return Array.from(dailyTotals.entries())
    .map(([dateKey, value]) => validateTrendPoint({ dateKey, value }).unwrap())
    .toSorted((first, second) => first.dateKey.localeCompare(second.dateKey));
};

type RecordSearchPayload = {
  keyword: string;
  resultCount: number;
  tags: string[] | null;
  contentType: string | null;
};

type ValidateSearchRecord = (
  unvalidated: UnvalidatedSearchRecord,
) => Result<SearchRecord, ValidationError[]>;

type PersistSearchRecord = (
  record: SearchRecord,
) => AsyncResult<void, UnexpectedError>;

export type RecordSearchWorkflow = (
  command: Command<RecordSearchPayload>,
) => AsyncResult<SearchRecordedEvent, ValidationError[] | UnexpectedError>;

export const createRecordSearchWorkflow =
  (validate: ValidateSearchRecord) =>
  (persist: PersistSearchRecord) =>
  (logger: Logger): RecordSearchWorkflow =>
  (command: Command<RecordSearchPayload>) => {
    logger.info("RecordSearchWorkflow started", {
      keyword: command.payload.keyword,
    });

    const dateKey = toJstDateKey(command.now);
    const identifier = ulid();

    return validate({
      identifier,
      dateKey,
      keyword: command.payload.keyword,
      resultCount: command.payload.resultCount,
      tags: command.payload.tags,
      contentType: command.payload.contentType,
      createdAt: command.now,
    })
      .toAsync()
      .tap((record) => {
        logger.debug("SearchRecord validation passed", {
          identifier: record.identifier,
        });
      })
      .tapError((errors) => {
        logger.warn("SearchRecord validation failed", { errors });
      })
      .andThen((record) =>
        persist(record)
          .tap(() => {
            logger.debug("SearchRecord persisted", {
              identifier: record.identifier,
            });
          })
          .andThen(() => ok(record)),
      )
      .map((record) => createSearchRecordedEvent(record, command.now))
      .tap((event) => {
        logger.info("RecordSearchWorkflow completed", { event });
      })
      .tapError((error) => {
        logger.error("RecordSearchWorkflow failed", { error });
      });
  };

type ValidatePeriod = (
  candidate: string,
) => Result<Period, ValidationError>;

type SearchSearchRecords = (
  criteria: SearchRecordCriteria,
) => AsyncResult<SearchRecord[], UnexpectedError>;

export type GetSearchCountWorkflow = (
  command: Command<{ period: string }>,
) => AsyncResult<PeriodComparison, ValidationError | ValidationError[] | UnexpectedError>;

export const createGetSearchCountWorkflow =
  (validatePeriod: ValidatePeriod) =>
  (search: SearchSearchRecords) =>
  (logger: Logger): GetSearchCountWorkflow =>
  (command: Command<{ period: string }>) => {
    logger.info("GetSearchCountWorkflow started", {
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
        const currentCriteria = validateSearchRecordCriteria({ dateRange: currentRange }).unwrap();
        const previousCriteria = validateSearchRecordCriteria({ dateRange: previousRange }).unwrap();

        return combineAsync([
          search(currentCriteria),
          search(previousCriteria),
        ] as const).andThen(([currentRecords, previousRecords]) =>
          validatePeriodComparison({
            current: currentRecords.length,
            previous: previousRecords.length,
          }).toAsync(),
        );
      })
      .tap((comparison) => {
        logger.info("GetSearchCountWorkflow completed", { comparison });
      })
      .tapError((error) => {
        logger.error("GetSearchCountWorkflow failed", { error });
      });
  };

export type GetSearchKeywordRankingWorkflow = (
  command: Command<{ period: string }>,
) => AsyncResult<RankedItem[], ValidationError | UnexpectedError>;

export const createGetSearchKeywordRankingWorkflow =
  (validatePeriod: ValidatePeriod) =>
  (search: SearchSearchRecords) =>
  (logger: Logger): GetSearchKeywordRankingWorkflow =>
  (command: Command<{ period: string }>) => {
    logger.info("GetSearchKeywordRankingWorkflow started", {
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
        const criteria = validateSearchRecordCriteria({ dateRange }).unwrap();
        return search(criteria);
      })
      .map(aggregateByKeyword)
      .tap((ranking) => {
        logger.info("GetSearchKeywordRankingWorkflow completed", { ranking });
      })
      .tapError((error) => {
        logger.error("GetSearchKeywordRankingWorkflow failed", { error });
      });
  };

export type GetSearchCountTrendWorkflow = (
  command: Command<{ period: string }>,
) => AsyncResult<TrendPoint[], ValidationError | UnexpectedError>;

export const createGetSearchCountTrendWorkflow =
  (validatePeriod: ValidatePeriod) =>
  (search: SearchSearchRecords) =>
  (logger: Logger): GetSearchCountTrendWorkflow =>
  (command: Command<{ period: string }>) => {
    logger.info("GetSearchCountTrendWorkflow started", {
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
        const criteria = validateSearchRecordCriteria({ dateRange }).unwrap();
        return search(criteria);
      })
      .map(aggregateSearchByDate)
      .tap((trend) => {
        logger.info("GetSearchCountTrendWorkflow completed", { trend });
      })
      .tapError((error) => {
        logger.error("GetSearchCountTrendWorkflow failed", { error });
      });
  };

export type GetZeroHitKeywordsWorkflow = (
  command: Command<{ period: string }>,
) => AsyncResult<RankedItem[], ValidationError | UnexpectedError>;

export const createGetZeroHitKeywordsWorkflow =
  (validatePeriod: ValidatePeriod) =>
  (search: SearchSearchRecords) =>
  (logger: Logger): GetZeroHitKeywordsWorkflow =>
  (command: Command<{ period: string }>) => {
    logger.info("GetZeroHitKeywordsWorkflow started", {
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
        const criteria = validateSearchRecordCriteria({ dateRange, hasResults: false }).unwrap();
        return search(criteria);
      })
      .map(aggregateByKeyword)
      .tap((keywords) => {
        logger.info("GetZeroHitKeywordsWorkflow completed", { keywords });
      })
      .tapError((error) => {
        logger.error("GetZeroHitKeywordsWorkflow failed", { error });
      });
  };


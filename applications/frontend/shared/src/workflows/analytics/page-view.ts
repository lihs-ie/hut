import {
  DuplicationError,
  UnexpectedError,
  ValidationError,
} from "@shared/aspects/error";
import { AsyncResult, Result, ok, combineAsync } from "@shared/aspects/result";
import { Logger } from "@shared/aspects/logger";
import { Command } from "@shared/workflows/common";
import {
  PageView,
  UnvalidatedPageView,
  Criteria as PageViewCriteria,
  criteriaSchema,
  detectDeviceType,
  extractReferrerDomain,
} from "@shared/domains/analytics/page-view";
import {
  DateRange,
  Period,
  TrendPoint,
  RankedItem,
  Distribution,
  PeriodComparison,
  toJstDateKey,
  resolveDateRange,
  resolvePreviousDateRange,
  validatePeriodComparison,
  validateTrendPoint,
  validateRankedItem,
  validateDistribution,
} from "@shared/domains/analytics/common";
import {
  PageViewRecordedEvent,
  createPageViewRecordedEvent,
} from "@shared/domains/analytics/event";

const buildSearchCriteria = (dateRange: DateRange): PageViewCriteria =>
  criteriaSchema.parse({ dateRange });

type RecordPageViewPayload = {
  reference: { type: string; content: string };
  sessionKey: string;
  referrer: string | null;
  userAgent: string | null;
};

type ValidatePageView = (
  unvalidated: UnvalidatedPageView
) => Result<PageView, ValidationError[]>;

type PersistPageView = (
  pageView: PageView
) => AsyncResult<void, DuplicationError<"PageView"> | UnexpectedError>;

type ValidatePeriod = (
  period: string
) => Result<Period, ValidationError>;

type SearchPageViews = (
  criteria: PageViewCriteria
) => AsyncResult<PageView[], UnexpectedError>;

export type RecordPageViewWorkflow = (
  command: Command<RecordPageViewPayload>
) => AsyncResult<
  PageViewRecordedEvent,
  DuplicationError<"PageView"> | ValidationError[] | UnexpectedError
>;

export type GetTotalPageViewsWorkflow = (
  command: Command<{ period: string }>
) => AsyncResult<PeriodComparison, ValidationError | UnexpectedError>;

export type GetPageViewTrendWorkflow = (
  command: Command<{ period: string }>
) => AsyncResult<TrendPoint[], ValidationError | UnexpectedError>;

export type GetReferrerRankingWorkflow = (
  command: Command<{ period: string }>
) => AsyncResult<RankedItem[], ValidationError | UnexpectedError>;

export type GetDeviceDistributionWorkflow = (
  command: Command<{ period: string }>
) => AsyncResult<Distribution[], ValidationError | UnexpectedError>;

export const toSortedRankedItems = <K extends string>(
  totals: Map<K, number>
): RankedItem[] => {
  const items: RankedItem[] = [];
  for (const [label, value] of totals) {
    items.push(validateRankedItem({ label, value }).unwrap());
  }
  return items.toSorted((first, second) => second.value - first.value);
};

export const toSortedDistributions = <K extends string>(
  totals: Map<K, number>
): Distribution[] => {
  const items: Distribution[] = [];
  for (const [label, value] of totals) {
    items.push(validateDistribution({ label, value }).unwrap());
  }
  return items.toSorted((first, second) => second.value - first.value);
};

export const aggregateByDate = (pageViews: PageView[]): TrendPoint[] => {
  const totals = new Map<string, number>();
  for (const pageView of pageViews) {
    const key = pageView.identifier.dateKey;
    totals.set(key, (totals.get(key) ?? 0) + 1);
  }
  const points: TrendPoint[] = [];
  for (const [dateKey, value] of totals) {
    points.push(validateTrendPoint({ dateKey, value }).unwrap());
  }
  return points.toSorted((first, second) =>
    first.dateKey.localeCompare(second.dateKey)
  );
};

export const aggregateByReferrer = (pageViews: PageView[]): RankedItem[] => {
  const totals = new Map<string, number>();
  for (const pageView of pageViews) {
    const domain = extractReferrerDomain(pageView.referrer);
    totals.set(domain, (totals.get(domain) ?? 0) + 1);
  }
  return toSortedRankedItems(totals);
};

export const aggregateByDevice = (pageViews: PageView[]): Distribution[] => {
  const totals = new Map<string, number>();
  for (const pageView of pageViews) {
    const device = pageView.deviceType;
    totals.set(device, (totals.get(device) ?? 0) + 1);
  }
  return toSortedDistributions(totals);
};

export const createRecordPageViewWorkflow =
  (validate: ValidatePageView) =>
  (persist: PersistPageView) =>
  (logger: Logger): RecordPageViewWorkflow =>
  (command: Command<RecordPageViewPayload>) => {
    logger.info("RecordPageViewWorkflow started", {
      reference: command.payload.reference,
    });

    const dateKey = toJstDateKey(command.now);
    const deviceType = detectDeviceType(command.payload.userAgent);

    return validate({
      identifier: {
        reference: command.payload.reference,
        dateKey,
        sessionKey: command.payload.sessionKey,
      },
      referrer: { raw: command.payload.referrer },
      deviceType,
      createdAt: command.now,
    })
      .toAsync()
      .tap((pageView) => {
        logger.debug("PageView validation passed", {
          identifier: pageView.identifier,
        });
      })
      .tapError((errors) => {
        logger.warn("PageView validation failed", { errors });
      })
      .andThen((pageView) =>
        persist(pageView)
          .tap(() => {
            logger.debug("PageView persisted", {
              identifier: pageView.identifier,
            });
          })
          .andThen(() => ok(pageView))
      )
      .map((pageView) => createPageViewRecordedEvent(pageView, command.now))
      .tap((event) => {
        logger.info("RecordPageViewWorkflow completed", { event });
      })
      .tapError((error) => {
        logger.error("RecordPageViewWorkflow failed", { error });
      });
  };

export const createGetTotalPageViewsWorkflow =
  (validatePeriod: ValidatePeriod) =>
  (search: SearchPageViews) =>
  (logger: Logger): GetTotalPageViewsWorkflow =>
  (command: Command<{ period: string }>) => {
    logger.info("GetTotalPageViewsWorkflow started", {
      period: command.payload.period,
    });

    return validatePeriod(command.payload.period)
      .toAsync()
      .andThen((period) => {
        const currentRange = resolveDateRange(period);
        const previousRange = resolvePreviousDateRange(period);
        return combineAsync([
          search(buildSearchCriteria(currentRange)),
          search(buildSearchCriteria(previousRange)),
        ]);
      })
      .map(([currentResults, previousResults]) =>
        validatePeriodComparison({
          current: currentResults.length,
          previous: previousResults.length,
        }).unwrap()
      )
      .tap((result) => {
        logger.info("GetTotalPageViewsWorkflow completed", {
          current: result.current,
          previous: result.previous,
        });
      })
      .tapError((error) => {
        logger.error("GetTotalPageViewsWorkflow failed", { error });
      });
  };

export const createGetPageViewTrendWorkflow =
  (validatePeriod: ValidatePeriod) =>
  (search: SearchPageViews) =>
  (logger: Logger): GetPageViewTrendWorkflow =>
  (command: Command<{ period: string }>) => {
    logger.info("GetPageViewTrendWorkflow started", {
      period: command.payload.period,
    });

    return validatePeriod(command.payload.period)
      .toAsync()
      .andThen((period) => {
        const range = resolveDateRange(period);
        return search(buildSearchCriteria(range));
      })
      .map((pageViews) => aggregateByDate(pageViews))
      .tap((trend) => {
        logger.info("GetPageViewTrendWorkflow completed", {
          pointCount: trend.length,
        });
      })
      .tapError((error) => {
        logger.error("GetPageViewTrendWorkflow failed", { error });
      });
  };

export const createGetReferrerRankingWorkflow =
  (validatePeriod: ValidatePeriod) =>
  (search: SearchPageViews) =>
  (logger: Logger): GetReferrerRankingWorkflow =>
  (command: Command<{ period: string }>) => {
    logger.info("GetReferrerRankingWorkflow started", {
      period: command.payload.period,
    });

    return validatePeriod(command.payload.period)
      .toAsync()
      .andThen((period) => {
        const range = resolveDateRange(period);
        return search(buildSearchCriteria(range));
      })
      .map((pageViews) => aggregateByReferrer(pageViews))
      .tap((ranking) => {
        logger.info("GetReferrerRankingWorkflow completed", {
          rankingCount: ranking.length,
        });
      })
      .tapError((error) => {
        logger.error("GetReferrerRankingWorkflow failed", { error });
      });
  };

export const createGetDeviceDistributionWorkflow =
  (validatePeriod: ValidatePeriod) =>
  (search: SearchPageViews) =>
  (logger: Logger): GetDeviceDistributionWorkflow =>
  (command: Command<{ period: string }>) => {
    logger.info("GetDeviceDistributionWorkflow started", {
      period: command.payload.period,
    });

    return validatePeriod(command.payload.period)
      .toAsync()
      .andThen((period) => {
        const range = resolveDateRange(period);
        return search(buildSearchCriteria(range));
      })
      .map((pageViews) => aggregateByDevice(pageViews))
      .tap((distribution) => {
        logger.info("GetDeviceDistributionWorkflow completed", {
          distributionCount: distribution.length,
        });
      })
      .tapError((error) => {
        logger.error("GetDeviceDistributionWorkflow failed", { error });
      });
  };

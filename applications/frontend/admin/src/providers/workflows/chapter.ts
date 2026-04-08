import { validateChapter, validateChapterIdentifier } from "@shared/domains/series/chapter";
import { validateSlug } from "@shared/domains/common";
import { validateSeries } from "@shared/domains/series";
import {
  createChapterPersistWorkflow,
  createChapterFindBySlugWorkflow,
  createChapterTerminateWorkflow,
  createChapterPersistWithSeriesWorkflow,
  createChapterTerminateWithSeriesWorkflow,
} from "@shared/workflows/chapter";
import { createPassthroughFilter } from "@shared/workflows/common";
import {
  createSeriesPersistWorkflow,
  createSeriesFindBySlugWorkflow,
} from "@shared/workflows/series";
import { ChapterRepositoryProvider } from "@shared/providers/infrastructure/chapter";
import { SeriesRepositoryProvider } from "@shared/providers/infrastructure/series";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";

const chapterPersistWorkflow = createChapterPersistWorkflow(validateChapter)(
  ChapterRepositoryProvider.firebase.persist,
)(LoggerProvider.console);

const chapterTerminateWorkflow = createChapterTerminateWorkflow(validateChapterIdentifier)(
  ChapterRepositoryProvider.firebase.terminate,
)(LoggerProvider.console);

const seriesFindBySlugWorkflow = createSeriesFindBySlugWorkflow(validateSlug)(
  LoggerProvider.console,
)(SeriesRepositoryProvider.firebase.findBySlug);

const seriesPersistWorkflow = createSeriesPersistWorkflow(validateSeries)(
  SeriesRepositoryProvider.firebase.persist,
)(LoggerProvider.console);

export const AdminChapterWorkflowProvider = {
  persist: chapterPersistWorkflow,

  terminate: chapterTerminateWorkflow,

  findBySlug: createChapterFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console,
  )(ChapterRepositoryProvider.firebase.findBySlug)(createPassthroughFilter()),

  persistWithSeries: createChapterPersistWithSeriesWorkflow(chapterPersistWorkflow)(
    validateSlug,
  )(
    (slug) => seriesFindBySlugWorkflow({ payload: { slug }, now: new Date() }),
  )(seriesPersistWorkflow)(LoggerProvider.console),

  terminateWithSeries: createChapterTerminateWithSeriesWorkflow(chapterTerminateWorkflow)(
    validateSlug,
  )(
    (slug) => seriesFindBySlugWorkflow({ payload: { slug }, now: new Date() }),
  )(seriesPersistWorkflow)(LoggerProvider.console),
} as const;

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
import {
  createSeriesPersistWorkflow,
  createSeriesFindBySlugWorkflow,
} from "@shared/workflows/series";
import { AdminChapterRepositoryProvider } from "../infrastructure/chapter";
import { AdminSeriesRepositoryProvider } from "../infrastructure/series";
import { LoggerProvider } from "@shared/providers/infrastructure/logger";

const chapterPersistWorkflow = createChapterPersistWorkflow(validateChapter)(
  AdminChapterRepositoryProvider.firebase.persist,
)(LoggerProvider.console);

const chapterTerminateWorkflow = createChapterTerminateWorkflow(validateChapterIdentifier)(
  AdminChapterRepositoryProvider.firebase.terminate,
)(LoggerProvider.console);

const seriesFindBySlugWorkflow = createSeriesFindBySlugWorkflow(validateSlug)(
  LoggerProvider.console,
)(AdminSeriesRepositoryProvider.firebase.findBySlug);

const seriesPersistWorkflow = createSeriesPersistWorkflow(validateSeries)(
  AdminSeriesRepositoryProvider.firebase.persist,
)(LoggerProvider.console);

export const AdminChapterWorkflowProvider = {
  persist: chapterPersistWorkflow,

  terminate: chapterTerminateWorkflow,

  findBySlug: createChapterFindBySlugWorkflow(validateSlug)(
    LoggerProvider.console,
  )(AdminChapterRepositoryProvider.firebase.findBySlug),

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

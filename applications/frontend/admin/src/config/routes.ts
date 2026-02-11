import { Routes as BaseRoutes } from "@shared/config/presentation/route";

export const Routes = {
  ...BaseRoutes,
  admin: {
    dashboard: "/admin",
    articles: {
      list: "/admin/articles",
    },
    memos: {
      list: "/admin/memos",
    },
    // [初期リリース対象外] series: {
    //   list: "/admin/series",
    // },
    tag: {
      list: "/admin/tags",
      new: "/admin/tags/new",
      edit: (identifier: string) => `/admin/tags/${identifier}/edit`,
    },
    profile: {
      edit: "/admin/profile/edit",
    },
    privacy: {
      edit: "/admin/privacy/edit",
    },
    analytics: {
      dashboard: "/admin/analytics",
    },
  },
} as const;

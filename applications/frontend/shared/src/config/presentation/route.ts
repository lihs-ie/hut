export const Routes = {
  page: {
    top: "/",
    articles: {
      index: "/articles",
      new: "/articles/new",
      show: (slug: string) => `/articles/${slug}`,
      edit: (slug: string) => `/articles/${slug}/edit`,
    },
    series: {
      index: "/series",
      new: "/series/new",
      show: (slug: string) => `/series/${slug}`,
      edit: (slug: string) => `/series/${slug}/edit`,
      chapter: {
        new: (slug: string) => `/series/${slug}/chapters/new`,
        edit: (slug: string, chapter: string) =>
          `/series/${slug}/chapters/${chapter}/edit`,
        show: (slug: string, chapter: string) =>
          `/series/${slug}/chapters/${chapter}`,
      },
    },
    memos: {
      index: "/memos",
      new: "/memos/new",
      show: (slug: string) => `/memos/${slug}`,
      edit: (slug: string) => `/memos/${slug}/edit`,
    },
    search: "/search",
    about: "/about",
  },
} as const;

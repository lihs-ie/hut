import { TechnologyKind } from "@shared/domains/common/tech";

export const profile = {
  techStack: {
    logoSources: {
      [TechnologyKind.NEXTJS]: "/logo/nextjs.svg",
      [TechnologyKind.REACT]: "/logo/react.svg",
      [TechnologyKind.TYPESCRIPT]: "/logo/typescript.svg",
      [TechnologyKind.PHP]: "/logo/php.svg",
      [TechnologyKind.LARAVEL]: "/logo/laravel.svg",
      [TechnologyKind.JAVA]: "/logo/java.svg",
      [TechnologyKind.GO]: "/logo/go.svg",
      [TechnologyKind.GIN]: "/logo/gin.svg",
      [TechnologyKind.RUST]: "/logo/rust.png",
    },
  },
} as const;

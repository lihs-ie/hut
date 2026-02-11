import { Environment } from "@shared/aspects/logger";

export const common = {
  ENVIRONMENT: (process.env.NODE_ENV || "development") as Environment,
} as const;

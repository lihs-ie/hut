import { Logger } from "@shared/aspects/logger";
import { common } from "@shared/config/common";

export const LoggerProvider = {
  console: Logger(common.ENVIRONMENT),
} as const;

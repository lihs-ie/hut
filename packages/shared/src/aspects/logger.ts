export const Level = {
  DEBUG: "debug",
  INFO: "info",
  WARN: "warn",
  ERROR: "error",
} as const;

export type Level = (typeof Level)[keyof typeof Level];

export interface Logger {
  debug(message: string, meta?: Record<string, unknown>): void;
  info(message: string, meta?: Record<string, unknown>): void;
  warn(message: string, meta?: Record<string, unknown>): void;
  error(message: string, meta?: Record<string, unknown>): void;
}

export const Environment = {
  PRODUCTION: "production",
  STAGING: "staging",
  DEVELOPMENT: "development",
} as const;

export type Environment = (typeof Environment)[keyof typeof Environment];

type Entry = {
  level: Level;
  message: string;
  meta?: Record<string, unknown>;
  timestamp: string;
};

const Entry = (
  level: Level,
  message: string,
  meta?: Record<string, unknown>
): Entry => ({
  level,
  message,
  meta,
  timestamp: new Date().toISOString(),
});

export const Logger = (
  environment: Environment,
  entryFormat: (
    level: Level,
    message: string,
    meta?: Record<string, unknown>
  ) => Entry = Entry
): Logger => {
  const log = (
    level: Level,
    message: string,
    meta?: Record<string, unknown>
  ) => {
    const entry = entryFormat(level, message, meta);

    if (environment === Environment.PRODUCTION) {
      console.log(JSON.stringify(entry));
    } else {
      const logMessage = `[${entry.timestamp}] [${entry.level.toUpperCase()}] ${
        entry.message
      }`;
      if (entry.meta) {
        console.log(logMessage, entry.meta);
      } else {
        console.log(logMessage);
      }
    }
  };

  return {
    debug: (message: string, meta?: Record<string, unknown>) => {
      log(Level.DEBUG, message, meta);
    },
    info: (message: string, meta?: Record<string, unknown>) => {
      log(Level.INFO, message, meta);
    },
    warn: (message: string, meta?: Record<string, unknown>) => {
      log(Level.WARN, message, meta);
    },
    error: (message: string, meta?: Record<string, unknown>) => {
      log(Level.ERROR, message, meta);
    },
  };
};

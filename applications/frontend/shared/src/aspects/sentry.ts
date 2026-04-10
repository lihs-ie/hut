type SentryConfiguration = {
  dsn: string | undefined;
  enabled: boolean;
  tracesSampleRate: number;
};

export const createSentryConfiguration = (
  dsn: string | undefined,
  useFirebaseEmulator: string | undefined,
): SentryConfiguration => ({
  dsn,
  enabled: dsn !== undefined && dsn !== "" && useFirebaseEmulator !== "true",
  tracesSampleRate: 1.0,
});

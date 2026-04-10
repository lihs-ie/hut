type SentryConfiguration = {
  dsn: string | undefined;
  enabled: boolean;
  tracesSampleRate: number;
};

const DEFAULT_TRACES_SAMPLE_RATE = 0.1;

export const createSentryConfiguration = (
  dsn: string | undefined,
  useFirebaseEmulator: string | undefined,
): SentryConfiguration => {
  const normalizedDsn = dsn?.trim() || undefined;

  return {
    dsn: normalizedDsn,
    enabled: normalizedDsn !== undefined && useFirebaseEmulator !== "true",
    tracesSampleRate: DEFAULT_TRACES_SAMPLE_RATE,
  };
};

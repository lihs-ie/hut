export type E2EEnvironment = {
  readonly e2eAuthEnabled?: string;
  readonly useFirebaseEmulator?: string;
};

export type E2EAvailability =
  | { readonly available: true }
  | { readonly available: false; readonly reason: string };

export const isE2EAuthAvailable = (
  environment: E2EEnvironment,
): E2EAvailability => {
  if (environment.useFirebaseEmulator !== "true") {
    return { available: false, reason: "Firebase emulator is not enabled" };
  }

  if (environment.e2eAuthEnabled !== "true") {
    return { available: false, reason: "E2E auth is not enabled" };
  }

  return { available: true };
};

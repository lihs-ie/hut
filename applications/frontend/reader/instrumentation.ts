export const register = async (): Promise<void> => {
  if (process.env.NEXT_RUNTIME === "nodejs") {
    await import("./sentry.server.config");
    return;
  }

  if (process.env.NEXT_RUNTIME === "edge") {
    await import("./sentry.edge.config");
  }
};

export { captureRequestError as onRequestError } from "@sentry/nextjs";

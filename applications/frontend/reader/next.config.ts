import MDX from "@next/mdx";
import { createBaseNextConfig } from "../next.config.shared";

const withMDX = MDX({ extension: /\.mdx?$/ });
const isProduction = process.env.NODE_ENV === "production";
const readerContentSecurityPolicy = [
  "default-src 'self'",
  isProduction
    ? "script-src 'self' 'unsafe-inline'"
    : "script-src 'self' 'unsafe-inline' 'unsafe-eval'",
  "style-src 'self' 'unsafe-inline' https://fonts.googleapis.com",
  "font-src 'self' https://fonts.gstatic.com",
  "img-src 'self' data: blob: https:",
  "connect-src 'self' https://*.googleapis.com https://*.firebaseio.com wss://*.firebaseio.com https://identitytoolkit.googleapis.com",
  "frame-src 'self' https://accounts.google.com https://*.firebaseapp.com",
  "frame-ancestors 'none'",
  "object-src 'none'",
  "base-uri 'self'",
  "form-action 'self'",
].join("; ");

export default withMDX(
  createBaseNextConfig({
    useFirebaseEmulator:
      process.env.NEXT_PUBLIC_USE_FIREBASE_EMULATOR === "true",
    contentSecurityPolicy: readerContentSecurityPolicy,
  }),
);

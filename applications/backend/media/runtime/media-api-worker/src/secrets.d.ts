import type { R2PresignerSecrets } from "./r2-presigner.js";

declare global {
  /**
   * Secret bindings are supplied with Wrangler and are intentionally absent
   * from wrangler.jsonc.
   */
  interface MediaApiWorkerEnv extends R2PresignerSecrets {}
}

export {};

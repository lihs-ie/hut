import { DurableObject } from 'cloudflare:workers';
import { createReactor, bindExport } from '@cloudflare-workers-hs/runtime';
import makeImports from './probe-jsffi.mjs';
import wasm from './probe.wasm';

const identity = value => value;

export class ArticleProbe extends DurableObject {
  constructor(ctx, env) {
    super(ctx, env);
    ctx.blockConcurrencyWhile(async () => {
      this.reactor = await createReactor(wasm, makeImports, exports => ({
        initialize: bindExport(exports, 'initialize', identity),
        workflow: bindExport(exports, 'workflow', identity),
        snapshot: bindExport(exports, 'snapshot', identity),
      }));
      await this.reactor.initialize(ctx.storage);
    });
  }

  async fetch(request) {
    const mode = Number(new URL(request.url).searchParams.get('mode') ?? 0);
    if (mode === 4) return Response.json(await this.reactor.snapshot(this.ctx.storage));
    try {
      const result = await this.ctx.storage.transaction(async () => {
        const outcome = await this.reactor.workflow(this.ctx.storage, mode);
        // A Haskell Left/status is a normal return, so explicitly abort in JS.
        if (outcome < 0) throw new Error(`workflow:${outcome}`);
        return outcome;
      });
      return Response.json({ result });
    } catch (error) {
      return Response.json({ error: String(error) }, { status: 409 });
    }
  }
}

export default {
  fetch(request, env) {
    const name = new URL(request.url).searchParams.get('object') ?? 'article';
    return env.ARTICLES.get(env.ARTICLES.idFromName(name)).fetch(request);
  },
};

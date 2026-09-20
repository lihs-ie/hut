const { resolve } = require('node:path');
const esbuild = require(process.argv[2]);
const output = resolve(process.argv[4]);
esbuild.build({
  entryPoints: [resolve(__dirname, 'worker.mjs')],
  outfile: resolve(output, 'worker.mjs'),
  bundle: true,
  format: 'esm',
  target: 'es2022',
  external: ['cloudflare:workers'],
  alias: { '@cloudflare-workers-hs/runtime': resolve(process.argv[3]) },
  plugins: [{
    name: 'probe-artifacts',
    setup(build) {
      build.onResolve({ filter: /probe-jsffi\.mjs$/ }, () => ({ path: resolve(output, 'probe-jsffi.mjs') }));
      build.onResolve({ filter: /probe\.wasm$/ }, () => ({ path: './probe.wasm', external: true }));
    },
  }],
}).catch(error => { console.error(error); process.exitCode = 1; });

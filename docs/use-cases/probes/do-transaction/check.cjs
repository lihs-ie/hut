const assert = require('node:assert/strict');
const { mkdtemp, rm } = require('node:fs/promises');
const { tmpdir } = require('node:os');
const { join, resolve } = require('node:path');
const { Miniflare } = require(process.argv[2]);

async function main() {
  const directory = await mkdtemp(join(tmpdir(), 'hut-do-probe-'));
  const options = {
    modules: true,
    scriptPath: resolve(process.argv[3]),
    compatibilityDate: '2026-06-17',
    durableObjects: { ARTICLES: { className: 'ArticleProbe', useSQLite: true } },
    durableObjectsPersist: directory,
    modulesRules: [{ type: 'CompiledWasm', include: ['**/*.wasm'] }],
  };
  let mf;
  try {
    mf = new Miniflare(options);
    const call = (mode, object = 'article') => mf.dispatchFetch(`http://probe/?mode=${mode}&object=${object}`);
    const snapshot = async () => (await call(4)).json();
    assert.deepEqual(await snapshot(), { revision: 0, events: 0 });
    assert.equal((await call(0)).status, 200);
    assert.deepEqual(await snapshot(), { revision: 1, events: 1 });
    console.log('PASS: Haskell reads, decides, writes, reads its own write, and appends outbox');
    for (const mode of [1, 2, 3]) {
      const response = await call(mode);
      assert.equal(response.status, 409);
      console.log('abort response', mode, await response.json());
      assert.deepEqual(await snapshot(), { revision: 1, events: 1 });
    }
    console.log('PASS: domain rejection, SQL constraint failure and Haskell exception roll back');
    const concurrent = await Promise.all(Array.from({ length: 8 }, () => call(7)));
    for (const response of concurrent) assert.equal(response.status, 200);
    const revisions = await Promise.all(concurrent.map(async response => (await response.json()).result));
    assert.deepEqual(revisions.sort((a, b) => a - b), [2, 3, 4, 5, 6, 7, 8, 9]);
    assert.deepEqual(await snapshot(), { revision: 9, events: 9 });
    console.log('PASS: eight concurrent requests across async Haskell suspension lose no updates');
    const mixed = await Promise.all(Array.from({ length: 8 }, (_, index) => call(index % 2 === 0 ? 7 : 3)));
    mixed.forEach((response, index) => assert.equal(response.status, index % 2 === 0 ? 200 : 409));
    assert.deepEqual(await snapshot(), { revision: 13, events: 13 });
    console.log('PASS: concurrent failed transactions do not erase successful transactions');
    assert.deepEqual(await (await call(4, 'other')).json(), { revision: 0, events: 0 });
    console.log('PASS: separate DO instance has independent storage');
    await mf.dispose();
    mf = new Miniflare(options);
    assert.deepEqual(await snapshot(), { revision: 13, events: 13 });
    console.log('PASS: article and outbox survive runtime restart');
  } finally {
    if (mf) await mf.dispose();
    await rm(directory, { recursive: true, force: true });
  }
}
main().catch(error => { console.error(error); process.exitCode = 1; });

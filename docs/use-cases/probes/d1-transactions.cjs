// Local feasibility probe only; not an Article feature test or production code.
const assert = require('node:assert/strict');
const { Miniflare } = require(process.argv[2]);

async function main() {
  const mf = new Miniflare({
    modules: true,
    script: 'export default { fetch() { return new Response("probe"); } };',
    compatibilityDate: '2026-06-17',
    d1Databases: { DB: 'transaction-probe' },
  });
  try {
    const db = await mf.getD1Database('DB');
    await db.exec('CREATE TABLE article (identifier TEXT PRIMARY KEY, revision INTEGER NOT NULL);');
    await db.exec('CREATE TABLE outbox (identifier TEXT PRIMARY KEY);');
    await db.prepare("INSERT INTO article VALUES ('a', 1)").run();
    for (const sql of ['BEGIN TRANSACTION', 'SAVEPOINT probe']) {
      await assert.rejects(db.prepare(sql).run(), error => {
        console.log(sql + ': ' + error.message);
        return /transaction|savepoint/i.test(error.message);
      });
    }
    await assert.rejects(db.batch([
      db.prepare("UPDATE article SET revision = 2 WHERE identifier = 'a'"),
      db.prepare("INSERT INTO outbox VALUES ('event')"),
      db.prepare("INSERT INTO outbox VALUES ('event')"),
    ]));
    assert.equal(await db.prepare("SELECT revision FROM article WHERE identifier = 'a'").first('revision'), 1);
    assert.equal(await db.prepare('SELECT count(*) AS n FROM outbox').first('n'), 0);
    console.log('PASS: SQL failure rolls back both article and outbox');

    const result = await db.batch([
      db.prepare("UPDATE article SET revision = 2 WHERE identifier = 'a' AND revision = 99"),
      db.prepare("INSERT INTO outbox VALUES ('zero-update')"),
    ]);
    assert.equal(result[0].meta.changes, 0);
    assert.equal(await db.prepare('SELECT count(*) AS n FROM outbox').first('n'), 1);
    console.log('PASS: zero-row update does NOT roll back the outbox insert');

    const session = db.withSession('first-primary');
    try {
      await session.prepare("UPDATE article SET revision = 3 WHERE identifier = 'a'").run();
      throw new Error('application failure');
    } catch (error) {
      assert.equal(error.message, 'application failure');
    }
    assert.equal(await db.prepare("SELECT revision FROM article WHERE identifier = 'a'").first('revision'), 3);
    console.log('PASS: session does not roll back a completed write on application failure');
  } finally {
    await mf.dispose();
  }
}
main().catch(error => { console.error(error); process.exitCode = 1; });

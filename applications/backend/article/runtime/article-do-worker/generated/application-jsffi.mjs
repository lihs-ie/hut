// This file implements the JavaScript runtime logic for Haskell
// modules that use JSFFI. It is not an ESM module, but the template
// of one; the post-linker script will copy all contents into a new
// ESM module.

// Manage a mapping from 32-bit ids to actual JavaScript values.
class JSValManager {
  #lastk = 0;
  #kv = new Map();

  newJSVal(v) {
    const k = ++this.#lastk;
    this.#kv.set(k, v);
    return k;
  }

  // A separate has() call to ensure we can store undefined as a value
  // too. Also, unconditionally check this since the check is cheap
  // anyway, if the check fails then there's a use-after-free to be
  // fixed.
  getJSVal(k) {
    if (!this.#kv.has(k)) {
      throw new WebAssembly.RuntimeError(`getJSVal(${k})`);
    }
    return this.#kv.get(k);
  }

  // Check for double free as well.
  freeJSVal(k) {
    if (!this.#kv.delete(k)) {
      throw new WebAssembly.RuntimeError(`freeJSVal(${k})`);
    }
  }
}

// The actual setImmediate() to be used. This is a ESM module top
// level binding and doesn't pollute the globalThis namespace.
//
// To benchmark different setImmediate() implementations in the
// browser, use https://github.com/jphpsf/setImmediate-shim-demo as a
// starting point.
const setImmediate = (() => {
  // node, deno, bun, or other scripts might have set this up in the
  // browser
  if (globalThis.setImmediate) {
    return globalThis.setImmediate;
  }

  // https://developer.mozilla.org/en-US/docs/Web/API/Scheduler/postTask
  if (globalThis.scheduler) {
    return (cb, ...args) => scheduler.postTask(() => cb(...args));
  }

  // Cloudflare workers doesn't support MessageChannel
  if (globalThis.MessageChannel) {
    // A simple & fast setImmediate() implementation for browsers. It's
    // not a drop-in replacement for node.js setImmediate() because:
    // 1. There's no clearImmediate(), and setImmediate() doesn't return
    //    anything
    // 2. There's no guarantee that callbacks scheduled by setImmediate()
    //    are executed in the same order (in fact it's the opposite lol),
    //    but you are never supposed to rely on this assumption anyway
    class SetImmediate {
      #fs = [];
      #mc = new MessageChannel();

      constructor() {
        this.#mc.port1.addEventListener("message", () => {
          this.#fs.pop()();
        });
        this.#mc.port1.start();
      }

      setImmediate(cb, ...args) {
        this.#fs.push(() => cb(...args));
        this.#mc.port2.postMessage(undefined);
      }
    }

    const sm = new SetImmediate();
    return (cb, ...args) => sm.setImmediate(cb, ...args);
  }

  return (cb, ...args) => setTimeout(cb, 0, ...args);
})();

export default (__exports) => {
const __ghc_wasm_jsffi_jsval_manager = new JSValManager();
const __ghc_wasm_jsffi_finalization_registry = globalThis.FinalizationRegistry ? new FinalizationRegistry(sp => __exports.rts_freeStablePtr(sp)) : { register: () => {}, unregister: () => true };
return {
newJSVal: (v) => __ghc_wasm_jsffi_jsval_manager.newJSVal(v),
getJSVal: (k) => __ghc_wasm_jsffi_jsval_manager.getJSVal(k),
freeJSVal: (k) => __ghc_wasm_jsffi_jsval_manager.freeJSVal(k),
scheduleWork: () => setImmediate(__exports.rts_schedulerLoop),
ZC0ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziBindingziDurableObjectziSQLZC: ($1,$2,$3,$4,$5) => ((() => {
  try {
    const plan = JSON.parse($2);
    let rowCount = 0, byteCount = 0;
    const count = n => {
      byteCount += n;
      if (byteCount > $4) {
        throw new Error('SQL output exceeds byte limit');
      }
    };
    const encode = value => {
      if (value === null) {
        count(1);
        return {tag:'null'};
      }
      if (typeof value === 'string') {
        count(value.length * 2);
        return {tag:'text',value};
      }
      if (typeof value === 'number') {
        if (!Number.isFinite(value) || (Number.isInteger(value) && !Number.isSafeInteger(value))) {
          throw new Error('SQL number cannot be represented safely');
        }
        count(8);
        return {tag:'number',value};
      }
      const bytes = value instanceof ArrayBuffer ? new Uint8Array(value) : new Uint8Array(value.buffer,value.byteOffset,value.byteLength);
      count(bytes.byteLength); return {tag:'blob',value:Array.from(bytes)};
    };
    const decode = item => item.tag === 'null' ? null : item.tag === 'blob' ? new Uint8Array(item.value).buffer : item.value;
    const execute = () => plan.map(statement => {
      const cursor = $1.sql.exec(statement.sql, ...statement.parameters.map(decode));
      const columns = cursor.columnNames;
      columns.forEach(name => count(name.length * 2));
      const rows = [];
      for (const row of cursor.raw()) {
        if (++rowCount > $3) {
          throw new Error('SQL output exceeds row limit');
        }
        rows.push(row.map(encode));
      }
      return {columns,rows,rowsRead:cursor.rowsRead,rowsWritten:cursor.rowsWritten};
    });
    const results = $5 ? $1.transactionSync(execute) : execute();
    return {ok:true,value:JSON.stringify(results)};
  } catch (error) {
    let message = 'SQL operation failed with an unprintable error';
    try { message = String(error); } catch (_) {}
    return {ok:false,message};
  }
})()),
ZC0ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziBytesZC: ($1,$2,$3) => (new Uint8Array(__exports.memory.buffer, $2, $3).set($1.bytes)),
ZC1ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziBytesZC: ($1) => ($1.length),
ZC2ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziBytesZC: ($1) => ((() => {
  const source = $1;
  if (!ArrayBuffer.isView(source)) {
    return { length: -1 };
  }
  const prototype = Object.getPrototypeOf(Uint8Array.prototype);
  const tag = Object.getOwnPropertyDescriptor(prototype, Symbol.toStringTag).get.call(source);
  if (!['Uint8Array', 'Int8Array', 'Uint8ClampedArray'].includes(tag)) {
    return { length: -2 };
  }
  try {
    const byteCount = source.length;
    const actualLength = Object.getOwnPropertyDescriptor(prototype, 'length').get.call(source);
    const buffer = Object.getOwnPropertyDescriptor(prototype, 'buffer').get.call(source);
    new Uint8Array(buffer, 0, 0);
    if (!Number.isSafeInteger(byteCount) || byteCount < 0 || byteCount > 2147483647
      || byteCount !== actualLength) {
      return { length: -3 };
    }
    const offset = Object.getOwnPropertyDescriptor(prototype, 'byteOffset').get.call(source);
    // Snapshot before Haskell allocation: create() may grow WASM memory
    // and detach a caller view of that same memory buffer.
    const bytes = new Uint8Array(buffer, offset, byteCount).slice();
    return { length: byteCount, bytes };
  } catch (_) {
    return { length: -3 };
  }
})()),
ZC3ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziBytesZC: ($1,$2) => (new Uint8Array(__exports.memory.buffer, $1, $2).slice()),
ZC12ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziDurableObjectZC: async ($1,$2) => ((async () => {
  try {
    await $1.setAlarm($2);
    return {
      ok: true,
      value: undefined
    };
  } catch (error) {
    return {
      ok: false,
      message: `${error}`
    };
  }
})()),
ZC36ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziDurableObjectZC: async ($1) => ((async () => {
  try {
    return {
      ok: true,
      value: new Uint8Array(await $1.arrayBuffer())
    };
  } catch (error) {
    return {
      ok: false,
      message: String(error)
    };
  }
})()),
ZC37ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziDurableObjectZC: ($1) => ($1.headers),
ZC38ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziDurableObjectZC: ($1) => ((() => {
  const statusCode = $1.status;

  if (!Number.isSafeInteger(statusCode) || statusCode < 0 || statusCode > 2147483647) {
    throw new TypeError('the Durable Object Response status is not a non-negative 32-bit integer: ' + typeof statusCode);
  }

  return statusCode;
})()),
ZC39ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziDurableObjectZC: () => (undefined),
ZC40ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziDurableObjectZC: ($1,$2,$3,$4) => (new Request($1, { method: $2, headers: $3, body: $4 })),
ZC42ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziDurableObjectZC: async ($1,$2) => ((async() => {
 try {
  return {
    ok: true,
    value: await $1.fetch($2)
  };
} catch (error) {
  return {
    ok: false,
    message: String(error)
  };
}
})()),
ZC43ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziDurableObjectZC: ($1,$2) => ($1.getByName($2)),
ZC0ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziDurableObjectziTransactionZC: ($1) => ((() => {
  try {
    return String($1.error);
  } catch (_) {
    return 'Unprintable transaction error';
  }
})()),
ZC1ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziDurableObjectziTransactionZC: ($1) => ($1.error),
ZC2ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziDurableObjectziTransactionZC: ($1,$2) => ((() => {
  $1.requested = $2;
  if ($1.finish !== null) {
    $1.finish($2);
  }
})()),
ZC3ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziDurableObjectziTransactionZC: async ($1) => ($1.done),
ZC4ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziDurableObjectziTransactionZC: async ($1) => ($1.ready),
ZC5ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziDurableObjectziTransactionZC: ($1) => ((() => {
  const aborted = {};
  const control = {finish: null, requested: null, error: null};
  let entered;
  control.ready = new Promise(resolve => { entered = resolve; });
  control.done = Promise.resolve().then(() => {
    if (!$1.sql) {
      throw new Error('Callback transactions require SQLite-backed Durable Object storage');
    }
    return $1.transaction(() => {
      const body = new Promise((resolve, reject) => {
        control.finish = success => success ? resolve() : reject(aborted);
        if (control.requested !== null) {
          control.finish(control.requested);
        }
      });
      entered(true);
      return body;
    });
  }).then(() => 1, error => {
    control.error = error;
    entered(false);
    return error === aborted ? 2 : 3;
  });
  return control;
})()),
ZC0ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziEnvZC: ($1,$2) => ($1[$2]),
ZC1ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziEnvZC: ($1) => ($1.length),
ZC2ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziEnvZC: ($1) => ((() => {
  try {
    return { ok: true, value: Object.entries($1) };
  } catch (_) {
    return { ok: false, message: 'Could not read Worker environment' };
  }
})()),
ZC1ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziEnvelopeZC: ($1) => ($1.message),
ZC2ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziEnvelopeZC: ($1) => ($1.value),
ZC3ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziEnvelopeZC: ($1) => (($1 === null || $1 === undefined) ? undefined : $1.ok),
ZC4ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziEnvelopeZC: ($1) => (($1 !== null && typeof $1 === 'object' && typeof $1.ok === 'boolean') ? ($1.ok ? 1 : 2) : 0),
ZC0ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziHeadersZC: ($1) => ((() => {
  try {
  const result = [];
  for (const [name, value] of $1.entries()) {
    if (typeof name !== "string" || typeof value !== "string") {
      throw new TypeError("Header entries must contain strings");
    }
    if (name.toLowerCase() !== 'set-cookie') {
      result.push(name);
      result.push(value);
    }
  }

  for (const cookie of $1.getSetCookie()) {
    if (typeof cookie !== "string") {
      throw new TypeError("Set-Cookie values must be strings");
    }
    result.push('set-cookie');
    result.push(cookie);
  }

  return { ok: true, value: result };
  } catch (_) {
    return { ok: false, message: "Failed to decode native Headers entries" };
  }
})()),
ZC1ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziHeadersZC: ($1,$2) => ($1[$2]),
ZC2ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziHeadersZC: ($1) => ((() => {
  const sourceArray = $1;
  if (!Array.isArray(sourceArray)) {
    throw new TypeError('the flattened Headers entry array is not a JS Array: ' + typeof sourceArray);
  }
  const elementCount = sourceArray.length;
  if (!Number.isSafeInteger(elementCount) || elementCount < 0 || elementCount > 2147483647) {
    throw new RangeError('the flattened Headers entry array has a length no 32-bit Haskell Int can carry');
  }
  return elementCount;
})()),
ZC3ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziHeadersZC: ($1) => ($1 === true),
ZC4ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziHeadersZC: ($1,$2,$3) => ((() => {
  try {
    $1.append($2, $3);
    return { ok: true, value: true };
  } catch (_) {
    return { ok: false, message: "Failed to append a native Header" };
  }
})()),
ZC5ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziHeadersZC: () => (new Headers()),
ZC10ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1,$2) => ($1.delaySeconds = $2),
ZC11ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1,$2) => ($1.contentType = $2),
ZC17ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: () => (({})),
ZC18ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: async ($1) => ((() => {
  try {
    return {
      ok: true,
      value: JSON.parse($1)
    };
  } catch (error) {
    return {
      ok: false,
      message: String(error)
    };
  }
})()),
ZC19ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1) => ($1.timestampMillis),
ZC20ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1) => ($1.backlogBytes),
ZC21ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1) => ($1.backlogCount),
ZC22ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: async ($1,$2) => ((() => {
  try {
    const mode = $2;
    const input = $1;
    let metrics;
    if (mode === 0 && input === undefined) {
      return { ok: true, value: undefined };
    }
    if (mode === 2) {
      metrics = input;
    } else {
      const metadata = input.metadata;
      if (mode === 1 && metadata === undefined) {
        return { ok: true, value: undefined };
      }
      if (metadata === null || typeof metadata !== 'object') {
        throw new TypeError('invalid queue metadata');
      }
      metrics = metadata.metrics;
      if (mode === 1 && metrics === undefined) {
        return { ok: true, value: undefined };
      }
    }
    if (metrics === null || typeof metrics !== 'object') {
      throw new TypeError('invalid queue metrics');
    }
    const count = metrics.backlogCount;
    const bytes = metrics.backlogBytes;
    if (!Number.isSafeInteger(count) || count < 0) {
      throw new TypeError('invalid queue backlogCount');
    }
    if (!Number.isSafeInteger(bytes) || bytes < 0) {
      throw new TypeError('invalid queue backlogBytes');
    }
    const timestamp = metrics.oldestMessageTimestamp;
    const millis = timestamp == null ? -1 : Date.prototype.getTime.call(timestamp);
    if (timestamp != null && (!Number.isSafeInteger(millis) || millis < 0)) {
      throw new TypeError('invalid queue oldestMessageTimestamp');
    }
    return { ok: true, value: { backlogCount: count, backlogBytes: bytes, timestampMillis: millis } };
  } catch (_) {
    return { ok: false, value: null, message: 'invalid queue metrics response' };
  }
})()),
ZC23ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1) => ($1 === undefined),
ZC26ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: async ($1,$2,$3) => ((async () => {
  try {
    const value = await $1.send($2, $3);
    return {
      ok: true,
      value,
      message: ''
    };
  } catch (error) {
    return {
      ok: false,
      value: null,
      message: `${error}`
    };
  }
})()),
ZC4ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziReactorZC: ($1) => ((() => {try {console.log($1); return {ok:true,value:null};} catch (_) {return {ok:false,message:'Worker console.log failed'};}})()),
ZC0ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziRequestZC: ($1,$2,$3,$4) => ((() => {
  try {
  const init = { method: $2, headers: $3 };
  if ($4 !== undefined) {
    init.body = $4;
    if ($4 instanceof ReadableStream) {
      init.duplex = "half";
    }
  }
  return { ok: true, value: new Request($1, init) };
  } catch (_) {
    return { ok: false, message: "Failed to construct the native Request" };
  }
})()),
ZC1ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziRequestZC: () => (undefined),
ZC2ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziRequestZC: ($1) => ($1.cf.colo),
ZC3ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziRequestZC: ($1) => (typeof $1.cf?.colo === 'string'),
ZC4ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziRequestZC: ($1) => ($1.body),
ZC5ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziRequestZC: ($1) => ($1.body === null),
ZC6ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziRequestZC: ($1) => ($1.headers),
ZC7ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziRequestZC: async ($1) => ($1.method),
ZC0ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziResponseZC: ($1,$2) => (new Response(null, {status: 101, webSocket: $1.webSocket, headers: $2})),
ZC1ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziResponseZC: ($1,$2,$3) => (new Response(([204, 205, 304].includes($2) && $1.byteLength === 0) ? null : $1, { status: $2, headers: $3 })),
ZC1ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziServiceBindingZC: ($1) => ($1),
ZC4ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziServiceBindingZC: ($1) => ($1 === null || $1 === undefined),
ZC5ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziServiceBindingZC: ($1) => ((() => {
  try {
    return { ok: true, value: $1.body };
  } catch (_) {
    return { ok: false, message: "Could not decode native Service value (jsResponseBody)" };
  }
})()),
ZC6ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziServiceBindingZC: ($1) => ((() => {
  try {
    return { ok: true, value: $1.headers };
  } catch (_) {
    return { ok: false, message: "Could not decode native Service value (jsResponseHeaders)" };
  }
})()),
ZC7ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziServiceBindingZC: ($1) => ((() => {
  try {
    const statusCode = $1.status;
    if (!Number.isSafeInteger(statusCode) || statusCode < 0 || statusCode > 2147483647) {
    throw new TypeError('the service-binding Response status is not a non-negative 32-bit integer: ' + typeof statusCode);
    }

    return { ok: true, value: statusCode };
  } catch (_) {
    return { ok: false, message: "Could not decode native Service value (jsResponseStatus)" };
  }
})()),
ZC11ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziServiceBindingZC: async ($1,$2) => ((async () => {
  try {
    return {
      ok: true,
      value: await $1.fetch($2)
    };
  } catch (error) {
    return {
      ok: false,
      message: String(error)
    };
  }
})()),
ZC2ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziStreamZC: async ($1) => ((async () => {
  try {
    const result = await $1.read();
    if (result === null || typeof result !== 'object') {
      throw new TypeError('the stream read() result is not an object');
    }
    const done = result.done;
    if (typeof done !== 'boolean') {
      throw new TypeError('the stream read() result done field is not a boolean: ' + typeof done);
    }
    // Read caller-owned getters inside the envelope boundary, before any
    // unsafe decoder runs. A completed read need not expose a value field.
    const value = done ? undefined : result.value;
    return { ok: true, value: { done, value } };
  } catch (error) {
    return {
      ok: false,
      message: String((error && error.message) || error)
    }
  }
})()),
ZC3ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziStreamZC: ($1) => ($1.done === true),
ZC4ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziStreamZC: ($1) => ($1.value ?? new Uint8Array(0)),
ZC5ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziStreamZC: ($1) => ((() => {
  try {
    return { ok: true, value: $1.getReader() };
  } catch (error) {
    return { ok: false, message: String(error) };
  }
})()),
ZC16ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziStreamZC: ($1) => ((() => { try { $1.releaseLock(); } catch (_) {} })()),
ZC17ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziStreamZC: ($1) => ($1.completed === true),
ZC18ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziStreamZC: async ($1) => ((async () => {
  try {
    await $1.cancel();
  } catch (_) {}
  return { completed: true };
})()),
ZC19ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziStreamZC: ($1,$2) => ((() => {
  const chunk = $1;
  if (!ArrayBuffer.isView(chunk)) {
    return false;
  }
  const prototype = Object.getPrototypeOf(Uint8Array.prototype);
  const tag = Object.getOwnPropertyDescriptor(prototype, Symbol.toStringTag).get.call(chunk);
  if (tag === undefined) {
    // DataView has no typed-array shape; the Bytes decoder rejects it.
    return false;
  }
  const byteLength = Object.getOwnPropertyDescriptor(prototype, 'byteLength').get.call(chunk);
  return byteLength > $2;
})()),
ZC0ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziTextZC: ($1) => (new TextDecoder().decode($1)),
ZC1ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziTextZC: ($1) => (typeof $1),
ZC2ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziTextZC: ($1) => ($1 === undefined ? 0 : ($1 === null ? 1 : (typeof $1 === 'string' ? 2 : 3))),
ZC3ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziTextZC: ($1) => (new TextEncoder().encode($1)),
ZC0ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziURLZC: ($1) => ($1.url),
ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1,$2) => ($1.reject(new WebAssembly.RuntimeError($2))),
ZC18ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1,$2) => ($1.resolve($2)),
ZC19ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1) => ($1.resolve()),
ZC20ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1) => {$1.throwTo = () => {};},
ZC21ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1,$2) => {$1.throwTo = (err) => __exports.rts_promiseThrowTo($2, err);},
ZC22ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: () => {let res, rej; const p = new Promise((resolve, reject) => { res = resolve; rej = reject; }); p.resolve = res; p.reject = rej; return p;},
ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziImportsZC: ($1,$2) => ($1.then(res => __exports.rts_promiseResolveBool($2, res), err => __exports.rts_promiseReject($2, err))),
ZC15ZCghczminternalZCGHCziInternalziWasmziPrimziImportsZC: ($1,$2) => ($1.then(res => __exports.rts_promiseResolveInt($2, res), err => __exports.rts_promiseReject($2, err))),
ZC17ZCghczminternalZCGHCziInternalziWasmziPrimziImportsZC: ($1,$2) => ($1.then(res => __exports.rts_promiseResolveJSVal($2, res), err => __exports.rts_promiseReject($2, err))),
ZC18ZCghczminternalZCGHCziInternalziWasmziPrimziImportsZC: ($1,$2) => ($1.then(() => __exports.rts_promiseResolveUnit($2), err => __exports.rts_promiseReject($2, err))),
ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1) => (`${$1.stack ? $1.stack : $1}`),
ZC1ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1,$2) => ((new TextDecoder('utf-8', {fatal: true})).decode(new Uint8Array(__exports.memory.buffer, $1, $2))),
ZC2ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1,$2,$3) => ((new TextEncoder()).encodeInto($1, new Uint8Array(__exports.memory.buffer, $2, $3)).written),
ZC3ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1) => ($1.length),
ZC4ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC: ($1) => {try { __ghc_wasm_jsffi_finalization_registry.unregister($1); } catch {}},
ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziConcziInternalZC: async ($1) => (new Promise(res => setTimeout(res, $1 / 1000))),
};
};

export const generatedArtifactKind = "generated";

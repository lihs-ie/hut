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
ZC1ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziBindingEnvZC: ($1) => (typeof $1 === 'string'),
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
ZC0ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziEnvZC: ($1,$2) => ($1[$2]),
ZC1ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziEnvZC: ($1) => ($1.length),
ZC2ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziEnvZC: ($1) => ((() => {
  try {
    return { ok: true, value: Object.entries($1) };
  } catch (_) {
    return { ok: false, message: 'Could not read Worker environment' };
  }
})()),
ZC0ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziEnvelopeZC: ($1) => ($1.error),
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
ZC0ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1,$2) => ($1.retry($2)),
ZC1ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1) => ($1.ack()),
ZC2ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1) => ((() => {
  const body = $1.body;

  if (typeof body === 'string') {
    return new TextEncoder().encode(body);
  }

  if (body instanceof ArrayBuffer) {
    return new Uint8Array(body);
  }

  if (ArrayBuffer.isView(body)) {
    return new Uint8Array(body.buffer, body.byteOffset, body.byteLength);
  }

  return new TextEncoder().encode(JSON.stringify(body))
})()),
ZC3ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1) => ((() => {
  const attemptCount = $1.attempts;

  if (!Number.isSafeInteger(attemptCount) || attemptCount < 0 || attemptCount > 2147483647) {
    throw new TypeError(
      'the Queue message attempts field is not a non-negative 32-bit integer: ' + typeof attemptCount
    );
  }

  return attemptCount;
})()),
ZC4ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1) => ((() => {
  const timestampMillis = $1.timestamp?.getTime?.();

  if (!Number.isFinite(timestampMillis)) {
    throw new TypeError(
      'the Queue message timestamp is not a finite number: ' + typeof timestampMillis
    );
  }

  return timestampMillis;
})()),
ZC5ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1) => ($1.id),
ZC6ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1,$2) => ($1.retryAll($2)),
ZC7ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1) => ($1.ackAll()),
ZC8ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1) => ($1.messages),
ZC9ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1) => ($1.queue),
ZC10ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1,$2) => ($1.delaySeconds = $2),
ZC11ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1,$2) => ($1.contentType = $2),
ZC13ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1,$2) => ($1[$2]),
ZC14ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziQueueZC: ($1) => ((() => {
  const sourceArray = $1;
  if (!Array.isArray(sourceArray)) {
    throw new TypeError('the Queue message batch array is not a JS Array: ' + typeof sourceArray);
  }

  const elementCount = sourceArray.length;
  if (!Number.isSafeInteger(elementCount) || elementCount < 0 || elementCount > 2147483647) {
    throw new RangeError(
      'the Queue message batch array has a length no 32-bit Haskell Int can carry'
    );
  }

  return elementCount;
})()),
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
ZC0ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziTextZC: ($1) => (new TextDecoder().decode($1)),
ZC1ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziTextZC: ($1) => (typeof $1),
ZC2ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziTextZC: ($1) => ($1 === undefined ? 0 : ($1 === null ? 1 : (typeof $1 === 'string' ? 2 : 3))),
ZC3ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziTextZC: ($1) => (new TextEncoder().encode($1)),
ZC4ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziWorkersAIZC: ($1) => ($1.name),
ZC5ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziWorkersAIZC: ($1) => (typeof $1.name === 'string'),
ZC6ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziWorkersAIZC: ($1) => ($1.invalidInput === true),
ZC7ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziWorkersAIZC: ($1) => ($1.invalid === true),
ZC8ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziWorkersAIZC: ($1) => ($1.message),
ZC9ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziWorkersAIZC: () => (undefined),
ZC10ZCcldflrzmwrkrszm0zi1zi0zi0zm2c908212ZCCloudflareziWorkersziInternalziFFIziWorkersAIZC: async ($1,$2,$3,$4,$5,$6) => ((async () => {
  let phase = 'input';

  try {
    const finiteNumber = (_key, value) => {
      if (typeof value === 'number' && !Number.isFinite(value)) {
        throw new RangeError('Workers AI input number is outside the JavaScript finite number range');
      }

      return value;
    };

    const input = JSON.parse($3, finiteNumber);
    const options = JSON.parse($4, finiteNumber);

    if (options.gateway?.metadata) {
      for (const key of Object.keys(options.gateway.metadata)) {
        const value = options.gateway.metadata[key];

        if (value !== null && typeof value === 'object' && value.kind === 'bigint') {
          options.gateway.metadata[key] = BigInt(value.value);
        }
      }
    }

    if ($5 !== undefined) {
      options.signal = $5;
    }

    phase = 'call';

    const result = await $1.run($2, input, options);
    phase = 'result';

    if ($6) {
      if (!(result instanceof Response)) {
        throw new TypeError('Workers AI returned a non-Response in response mode');
      }

      const status = result.status;
      const headers = new Headers(result.headers);

      if (!Number.isInteger(status) || status < 0 || status > 599) {
        throw new TypeError('Workers AI returned an invalid response status');
      }

      const body = result.body;
      const statusText = result.statusText;
      const webSocket = result.webSocket;
      const cf = result.cf;

      if (body !== null && !(body instanceof ReadableStream)) {
        throw new TypeError('Workers AI returned an invalid response body');
      }

      if (typeof statusText !== 'string') {
        throw new TypeError('Workers AI returned an invalid response status text');
      }

      if (webSocket !== undefined && webSocket !== null && !(webSocket instanceof WebSocket)) {
        throw new TypeError('Workers AI returned an invalid websocket');
      }

      return {
        ok: true,
        value: {
          original: result,
          status,
          headers,
          body,
          statusText,
          webSocket,
          cf
        }
      };
    }

    const encoded = JSON.stringify(result, (_key, value) => {
      if (typeof value === 'number' && !Number.isFinite(value)) {
        throw new TypeError('Workers AI returned a non-finite JSON number');
      }

      if (['undefined', 'function', 'symbol', 'bigint'].includes(typeof value)) {
        throw new TypeError('Workers AI returned a non-JSON value');
      }

      return value;
    });

    if (typeof encoded !== 'string') {
      throw new TypeError('Workers AI returned a non-JSON value');
    }

    return { ok: true, value: encoded };
  } catch (error) {
    let message = 'Workers AI threw an unprintable exception';
    let name = null;
    let hasMessage = false;

    try {
      const candidate = error?.message;
      if (typeof candidate === 'string') {
        message = candidate; hasMessage = true; }
    } catch (_) {}

    try {
      const candidate = error?.name;
      if (typeof candidate === 'string') {
        name = candidate;
      }
    } catch (_) {}
    if (!hasMessage) {
      try {
        message = String(error);
      } catch (_) {} }

    return {
      ok: false,
      error: {
        message,
        name,
        invalid: phase === 'result',
        invalidInput: phase === 'input'
      }
    };
  }
})()),
ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1,$2) => ($1.reject(new WebAssembly.RuntimeError($2))),
ZC19ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1) => ($1.resolve()),
ZC20ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1) => {$1.throwTo = () => {};},
ZC21ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: ($1,$2) => {$1.throwTo = (err) => __exports.rts_promiseThrowTo($2, err);},
ZC22ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC: () => {let res, rej; const p = new Promise((resolve, reject) => { res = resolve; rej = reject; }); p.resolve = res; p.reject = rej; return p;},
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

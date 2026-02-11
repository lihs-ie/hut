import { describe, it, expect } from "vitest";
import {
  ok,
  err,
  isAsyncResult,
  fromPromise,
  fromThrowable,
  fromAsyncThrowable,
  combine,
  combineAsync,
  isOk,
  isErr,
  type Result,
  type AsyncResult,
} from "@shared/aspects/result";

describe("aspects/result", () => {
  describe("ok", () => {
    it("Ok Result を作成できる", () => {
      const result = ok(42);

      expect(result._tag).toBe("Ok");
      expect(result.isOk).toBe(true);
      expect(result.isErr).toBe(false);
    });

    it("様々な型の値を保持できる", () => {
      expect(ok("hello").unwrap()).toBe("hello");
      expect(ok({ name: "test" }).unwrap()).toEqual({ name: "test" });
      expect(ok([1, 2, 3]).unwrap()).toEqual([1, 2, 3]);
      expect(ok(null).unwrap()).toBe(null);
      expect(ok(undefined).unwrap()).toBe(undefined);
    });
  });

  describe("err", () => {
    it("Err Result を作成できる", () => {
      const result = err("error message");

      expect(result._tag).toBe("Err");
      expect(result.isOk).toBe(false);
      expect(result.isErr).toBe(true);
    });

    it("様々な型のエラーを保持できる", () => {
      expect(err("error").unwrapError()).toBe("error");
      expect(err({ code: 404 }).unwrapError()).toEqual({ code: 404 });
      expect(err(new Error("test")).unwrapError()).toBeInstanceOf(Error);
    });
  });

  describe("Result.map", () => {
    it("Ok の値を変換できる", () => {
      const result = ok(10).map((x) => x * 2);

      expect(result.isOk).toBe(true);
      expect(result.unwrap()).toBe(20);
    });

    it("Err の場合は変換されない", () => {
      const result = err<number, string>("error").map((x) => x * 2);

      expect(result.isErr).toBe(true);
      expect(result.unwrapError()).toBe("error");
    });

    it("型を変換できる", () => {
      const result = ok(42).map((x) => x.toString());

      expect(result.unwrap()).toBe("42");
    });
  });

  describe("Result.mapError", () => {
    it("Err のエラーを変換できる", () => {
      const result = err<number, string>("error").mapError(
        (e) => `mapped: ${e}`
      );

      expect(result.isErr).toBe(true);
      expect(result.unwrapError()).toBe("mapped: error");
    });

    it("Ok の場合は変換されない", () => {
      const result = ok<number, string>(42).mapError((e) => `mapped: ${e}`);

      expect(result.isOk).toBe(true);
      expect(result.unwrap()).toBe(42);
    });
  });

  describe("Result.unwrap", () => {
    it("Ok の場合は値を返す", () => {
      expect(ok(42).unwrap()).toBe(42);
    });

    it("Err の場合は例外を投げる", () => {
      expect(() => err("error").unwrap()).toThrow(
        'Called unwrap on an Err value: "error"'
      );
    });
  });

  describe("Result.unwrapOr", () => {
    it("Ok の場合は値を返す", () => {
      expect(ok(42).unwrapOr(0)).toBe(42);
    });

    it("Err の場合はデフォルト値を返す", () => {
      expect(err<number, string>("error").unwrapOr(0)).toBe(0);
    });
  });

  describe("Result.unwrapError", () => {
    it("Err の場合はエラーを返す", () => {
      expect(err("error").unwrapError()).toBe("error");
    });

    it("Ok の場合は例外を投げる", () => {
      expect(() => ok(42).unwrapError()).toThrow(
        "Called unwrapError on an Ok value"
      );
    });
  });

  describe("Result.andThen", () => {
    it("Ok の場合は次の操作を実行する", () => {
      const result = ok(10).andThen((x) => ok(x * 2));

      expect(result.isOk).toBe(true);
      expect(result.unwrap()).toBe(20);
    });

    it("Ok から Err に変化できる", () => {
      const result = ok(10).andThen((_x) => err<number, string>("failed"));

      expect(result.isErr).toBe(true);
      expect(result.unwrapError()).toBe("failed");
    });

    it("Err の場合は次の操作を実行しない", () => {
      let called = false;
      const result = err<number, string>("error").andThen((x) => {
        called = true;
        return ok(x * 2);
      });

      expect(called).toBe(false);
      expect(result.isErr).toBe(true);
      expect(result.unwrapError()).toBe("error");
    });

    it("チェーンできる", () => {
      const result = ok(10)
        .andThen((x) => ok(x + 5))
        .andThen((x) => ok(x * 2));

      expect(result.unwrap()).toBe(30);
    });
  });

  describe("Result.orElse", () => {
    it("Err の場合はリカバリー操作を実行する", () => {
      const result = err<number, string>("error").orElse((_e) => ok(0));

      expect(result.isOk).toBe(true);
      expect(result.unwrap()).toBe(0);
    });

    it("Err から別の Err に変化できる", () => {
      const result = err<number, string>("error1").orElse((_e) =>
        err<number, string>("error2")
      );

      expect(result.isErr).toBe(true);
      expect(result.unwrapError()).toBe("error2");
    });

    it("Ok の場合はリカバリー操作を実行しない", () => {
      let called = false;
      const result = ok<number, string>(42).orElse((_e) => {
        called = true;
        return ok(0);
      });

      expect(called).toBe(false);
      expect(result.isOk).toBe(true);
      expect(result.unwrap()).toBe(42);
    });
  });

  describe("Result.match", () => {
    it("Ok の場合は ok ハンドラーを実行する", () => {
      const result = ok(42).match({
        ok: (x) => `success: ${x}`,
        err: (e) => `error: ${e}`,
      });

      expect(result).toBe("success: 42");
    });

    it("Err の場合は err ハンドラーを実行する", () => {
      const result = err<number, string>("failed").match({
        ok: (x) => `success: ${x}`,
        err: (e) => `error: ${e}`,
      });

      expect(result).toBe("error: failed");
    });
  });

  describe("Result.tap", () => {
    it("Ok の場合は副作用を実行し、元の Result を返す", () => {
      let sideEffect = 0;
      const result = ok(42).tap((x) => {
        sideEffect = x;
      });

      expect(sideEffect).toBe(42);
      expect(result.isOk).toBe(true);
      expect(result.unwrap()).toBe(42);
    });

    it("Err の場合は副作用を実行しない", () => {
      let called = false;
      const result = err<number, string>("error").tap((_x) => {
        called = true;
      });

      expect(called).toBe(false);
      expect(result.isErr).toBe(true);
    });
  });

  describe("Result.tapError", () => {
    it("Err の場合は副作用を実行し、元の Result を返す", () => {
      let sideEffect = "";
      const result = err<number, string>("error").tapError((e) => {
        sideEffect = e;
      });

      expect(sideEffect).toBe("error");
      expect(result.isErr).toBe(true);
      expect(result.unwrapError()).toBe("error");
    });

    it("Ok の場合は副作用を実行しない", () => {
      let called = false;
      const result = ok<number, string>(42).tapError((_e) => {
        called = true;
      });

      expect(called).toBe(false);
      expect(result.isOk).toBe(true);
    });
  });

  describe("Result.toAsync", () => {
    it("Ok を AsyncResult に変換できる", async () => {
      const asyncResult = ok(42).toAsync();

      expect(isAsyncResult(asyncResult)).toBe(true);
      expect(await asyncResult.unwrap()).toBe(42);
    });

    it("Err を AsyncResult に変換できる", async () => {
      const asyncResult = err<number, string>("error").toAsync();

      expect(isAsyncResult(asyncResult)).toBe(true);
      expect(await asyncResult.unwrapError()).toBe("error");
    });
  });

  describe("AsyncResult.map", () => {
    it("Ok の値を同期的に変換できる", async () => {
      const result = ok(10).toAsync().map((x) => x * 2);

      expect(await result.unwrap()).toBe(20);
    });

    it("Ok の値を非同期的に変換できる", async () => {
      const result = ok(10)
        .toAsync()
        .map(async (x) => x * 2);

      expect(await result.unwrap()).toBe(20);
    });

    it("Err の場合は変換されない", async () => {
      const result = err<number, string>("error")
        .toAsync()
        .map((x) => x * 2);

      expect(await result.unwrapError()).toBe("error");
    });
  });

  describe("AsyncResult.mapError", () => {
    it("Err のエラーを同期的に変換できる", async () => {
      const result = err<number, string>("error")
        .toAsync()
        .mapError((e) => `mapped: ${e}`);

      expect(await result.unwrapError()).toBe("mapped: error");
    });

    it("Err のエラーを非同期的に変換できる", async () => {
      const result = err<number, string>("error")
        .toAsync()
        .mapError(async (e) => `mapped: ${e}`);

      expect(await result.unwrapError()).toBe("mapped: error");
    });

    it("Ok の場合は変換されない", async () => {
      const result = ok<number, string>(42)
        .toAsync()
        .mapError((e) => `mapped: ${e}`);

      expect(await result.unwrap()).toBe(42);
    });
  });

  describe("AsyncResult.unwrap", () => {
    it("Ok の場合は値を返す", async () => {
      expect(await ok(42).toAsync().unwrap()).toBe(42);
    });

    it("Err の場合は例外を投げる", async () => {
      await expect(err("error").toAsync().unwrap()).rejects.toThrow(
        'Called unwrap on an Err value: "error"'
      );
    });
  });

  describe("AsyncResult.unwrapOr", () => {
    it("Ok の場合は値を返す", async () => {
      expect(await ok(42).toAsync().unwrapOr(0)).toBe(42);
    });

    it("Err の場合はデフォルト値を返す", async () => {
      expect(await err<number, string>("error").toAsync().unwrapOr(0)).toBe(0);
    });
  });

  describe("AsyncResult.unwrapError", () => {
    it("Err の場合はエラーを返す", async () => {
      expect(await err("error").toAsync().unwrapError()).toBe("error");
    });

    it("Ok の場合は例外を投げる", async () => {
      await expect(ok(42).toAsync().unwrapError()).rejects.toThrow(
        "Called unwrapError on an Ok value"
      );
    });
  });

  describe("AsyncResult.andThen", () => {
    it("Ok の場合に同期 Result を返す関数を実行する", async () => {
      const result = ok(10)
        .toAsync()
        .andThen((x) => ok(x * 2));

      expect(await result.unwrap()).toBe(20);
    });

    it("Ok の場合に AsyncResult を返す関数を実行する", async () => {
      const result = ok(10)
        .toAsync()
        .andThen((x) => ok(x * 2).toAsync());

      expect(await result.unwrap()).toBe(20);
    });

    it("Ok から Err に変化できる", async () => {
      const result = ok(10)
        .toAsync()
        .andThen((_x) => err<number, string>("failed"));

      expect(await result.unwrapError()).toBe("failed");
    });

    it("Err の場合は次の操作を実行しない", async () => {
      let called = false;
      const result = err<number, string>("error")
        .toAsync()
        .andThen((x) => {
          called = true;
          return ok(x * 2);
        });

      expect(called).toBe(false);
      expect(await result.unwrapError()).toBe("error");
    });

    it("チェーンできる", async () => {
      const result = ok(10)
        .toAsync()
        .andThen((x) => ok(x + 5))
        .andThen((x) => ok(x * 2));

      expect(await result.unwrap()).toBe(30);
    });
  });

  describe("AsyncResult.orElse", () => {
    it("Err の場合に同期 Result でリカバリーできる", async () => {
      const result = err<number, string>("error")
        .toAsync()
        .orElse((_e) => ok(0));

      expect(await result.unwrap()).toBe(0);
    });

    it("Err の場合に AsyncResult でリカバリーできる", async () => {
      const result = err<number, string>("error")
        .toAsync()
        .orElse((_e) => ok<number, string>(0).toAsync());

      expect(await result.unwrap()).toBe(0);
    });

    it("Ok の場合はリカバリー操作を実行しない", async () => {
      let called = false;
      const result = ok<number, string>(42)
        .toAsync()
        .orElse((_e) => {
          called = true;
          return ok(0);
        });

      expect(called).toBe(false);
      expect(await result.unwrap()).toBe(42);
    });
  });

  describe("AsyncResult.match", () => {
    it("Ok の場合は ok ハンドラーを実行する", async () => {
      const result = await ok(42)
        .toAsync()
        .match({
          ok: (x) => `success: ${x}`,
          err: (e) => `error: ${e}`,
        });

      expect(result).toBe("success: 42");
    });

    it("Err の場合は err ハンドラーを実行する", async () => {
      const result = await err<number, string>("failed")
        .toAsync()
        .match({
          ok: (x) => `success: ${x}`,
          err: (e) => `error: ${e}`,
        });

      expect(result).toBe("error: failed");
    });

    it("非同期ハンドラーを使用できる", async () => {
      const result = await ok(42)
        .toAsync()
        .match({
          ok: async (x) => `success: ${x}`,
          err: async (e) => `error: ${e}`,
        });

      expect(result).toBe("success: 42");
    });
  });

  describe("AsyncResult.tap", () => {
    it("Ok の場合は副作用を実行し、元の AsyncResult を返す", async () => {
      let sideEffect = 0;
      const result = ok(42)
        .toAsync()
        .tap((x) => {
          sideEffect = x;
        });

      expect(await result.unwrap()).toBe(42);
      expect(sideEffect).toBe(42);
    });

    it("非同期の副作用を実行できる", async () => {
      let sideEffect = 0;
      const result = ok(42)
        .toAsync()
        .tap(async (x) => {
          sideEffect = x;
        });

      expect(await result.unwrap()).toBe(42);
      expect(sideEffect).toBe(42);
    });

    it("Err の場合は副作用を実行しない", async () => {
      let called = false;
      const result = err<number, string>("error")
        .toAsync()
        .tap((_x) => {
          called = true;
        });

      expect(await result.unwrapError()).toBe("error");
      expect(called).toBe(false);
    });
  });

  describe("AsyncResult.tapError", () => {
    it("Err の場合は副作用を実行し、元の AsyncResult を返す", async () => {
      let sideEffect = "";
      const result = err<number, string>("error")
        .toAsync()
        .tapError((e) => {
          sideEffect = e;
        });

      expect(await result.unwrapError()).toBe("error");
      expect(sideEffect).toBe("error");
    });

    it("非同期の副作用を実行できる", async () => {
      let sideEffect = "";
      const result = err<number, string>("error")
        .toAsync()
        .tapError(async (e) => {
          sideEffect = e;
        });

      expect(await result.unwrapError()).toBe("error");
      expect(sideEffect).toBe("error");
    });

    it("Ok の場合は副作用を実行しない", async () => {
      let called = false;
      const result = ok<number, string>(42)
        .toAsync()
        .tapError((_e) => {
          called = true;
        });

      expect(await result.unwrap()).toBe(42);
      expect(called).toBe(false);
    });
  });

  describe("isAsyncResult", () => {
    it("AsyncResult を正しく判定する", () => {
      expect(isAsyncResult(ok(42).toAsync())).toBe(true);
      expect(isAsyncResult(err("error").toAsync())).toBe(true);
    });

    it("Result は false を返す", () => {
      expect(isAsyncResult(ok(42))).toBe(false);
      expect(isAsyncResult(err("error"))).toBe(false);
    });

    it("その他のオブジェクトは false を返す", () => {
      expect(isAsyncResult(null as unknown as Result<unknown, unknown>)).toBe(
        false
      );
      expect(
        isAsyncResult(undefined as unknown as Result<unknown, unknown>)
      ).toBe(false);
      expect(isAsyncResult({} as Result<unknown, unknown>)).toBe(false);
      expect(
        isAsyncResult({ _tag: "Other" } as unknown as Result<unknown, unknown>)
      ).toBe(false);
    });
  });

  describe("fromPromise", () => {
    it("成功した Promise を Ok に変換する", async () => {
      const asyncResult = fromPromise(Promise.resolve(42), (e) => String(e));

      expect(await asyncResult.unwrap()).toBe(42);
    });

    it("失敗した Promise を Err に変換する", async () => {
      const asyncResult = fromPromise(
        Promise.reject(new Error("failed")),
        (e) => (e as Error).message
      );

      expect(await asyncResult.unwrapError()).toBe("failed");
    });

    it("errorMapper でエラーを変換できる", async () => {
      const asyncResult = fromPromise(Promise.reject("raw error"), (e) => ({
        code: "ERROR",
        message: String(e),
      }));

      const error = await asyncResult.unwrapError();
      expect(error).toEqual({ code: "ERROR", message: "raw error" });
    });
  });

  describe("fromThrowable", () => {
    it("例外を投げない関数を Ok に変換する", () => {
      const safeDivide = fromThrowable(
        (a: number, b: number) => {
          if (b === 0) throw new Error("Division by zero");
          return a / b;
        },
        (e) => (e as Error).message
      );

      const result = safeDivide(10, 2);
      expect(result.isOk).toBe(true);
      expect(result.unwrap()).toBe(5);
    });

    it("例外を投げる関数を Err に変換する", () => {
      const safeDivide = fromThrowable(
        (a: number, b: number) => {
          if (b === 0) throw new Error("Division by zero");
          return a / b;
        },
        (e) => (e as Error).message
      );

      const result = safeDivide(10, 0);
      expect(result.isErr).toBe(true);
      expect(result.unwrapError()).toBe("Division by zero");
    });
  });

  describe("fromAsyncThrowable", () => {
    it("例外を投げない非同期関数を Ok に変換する", async () => {
      const safeAsyncDivide = fromAsyncThrowable(
        async (a: number, b: number) => {
          if (b === 0) throw new Error("Division by zero");
          return a / b;
        },
        (e) => (e as Error).message
      );

      const result = safeAsyncDivide(10, 2);
      expect(await result.unwrap()).toBe(5);
    });

    it("例外を投げる非同期関数を Err に変換する", async () => {
      const safeAsyncDivide = fromAsyncThrowable(
        async (a: number, b: number) => {
          if (b === 0) throw new Error("Division by zero");
          return a / b;
        },
        (e) => (e as Error).message
      );

      const result = safeAsyncDivide(10, 0);
      expect(await result.unwrapError()).toBe("Division by zero");
    });
  });

  describe("combine", () => {
    it("すべて Ok の場合は値の配列を返す", () => {
      const results = [ok(1), ok(2), ok(3)] as const;
      const combined = combine(results);

      expect(combined.isOk).toBe(true);
      expect(combined.unwrap()).toEqual([1, 2, 3]);
    });

    it("1つでも Err があれば最初の Err を返す", () => {
      const results = [ok(1), err("error"), ok(3)] as const;
      const combined = combine(results);

      expect(combined.isErr).toBe(true);
      expect(combined.unwrapError()).toBe("error");
    });

    it("空の配列は空の配列を返す", () => {
      const results: Result<number, string>[] = [];
      const combined = combine(results);

      expect(combined.isOk).toBe(true);
      expect(combined.unwrap()).toEqual([]);
    });

    it("異なる型の Result を結合できる", () => {
      const results = [ok("hello"), ok(42), ok(true)] as const;
      const combined = combine(results);

      expect(combined.isOk).toBe(true);
      expect(combined.unwrap()).toEqual(["hello", 42, true]);
    });
  });

  describe("combineAsync", () => {
    it("すべて Ok の場合は値の配列を返す", async () => {
      const asyncResults = [
        ok(1).toAsync(),
        ok(2).toAsync(),
        ok(3).toAsync(),
      ] as const;
      const combined = combineAsync(asyncResults);

      expect(await combined.unwrap()).toEqual([1, 2, 3]);
    });

    it("1つでも Err があれば最初の Err を返す", async () => {
      const asyncResults = [
        ok<number, string>(1).toAsync(),
        err<number, string>("error").toAsync(),
        ok<number, string>(3).toAsync(),
      ] as const;
      const combined = combineAsync(asyncResults);

      expect(await combined.unwrapError()).toBe("error");
    });

    it("空の配列は空の配列を返す", async () => {
      const asyncResults: AsyncResult<number, string>[] = [];
      const combined = combineAsync(asyncResults);

      expect(await combined.unwrap()).toEqual([]);
    });
  });

  describe("isOk", () => {
    it("Ok の場合は true を返す", () => {
      expect(isOk(ok(42))).toBe(true);
    });

    it("Err の場合は false を返す", () => {
      expect(isOk(err("error"))).toBe(false);
    });
  });

  describe("isErr", () => {
    it("Err の場合は true を返す", () => {
      expect(isErr(err("error"))).toBe(true);
    });

    it("Ok の場合は false を返す", () => {
      expect(isErr(ok(42))).toBe(false);
    });
  });

  describe("統合テスト", () => {
    it("複雑なチェーンを処理できる", async () => {
      const parseNumber = (s: string): Result<number, string> => {
        const n = parseInt(s, 10);
        return isNaN(n) ? err("Invalid number") : ok(n);
      };

      const doubleIfPositive = (n: number): Result<number, string> =>
        n > 0 ? ok(n * 2) : err("Number must be positive");

      const result = await parseNumber("10")
        .andThen(doubleIfPositive)
        .toAsync()
        .map(async (n) => n + 1)
        .match({
          ok: (n) => `Result: ${n}`,
          err: (e) => `Error: ${e}`,
        });

      expect(result).toBe("Result: 21");
    });

    it("エラーのチェーンを処理できる", async () => {
      const parseNumber = (s: string): Result<number, string> => {
        const n = parseInt(s, 10);
        return isNaN(n) ? err("Invalid number") : ok(n);
      };

      const result = await parseNumber("abc")
        .toAsync()
        .orElse((_e) => ok(0))
        .map((n) => n + 100)
        .match({
          ok: (n) => `Result: ${n}`,
          err: (e) => `Error: ${e}`,
        });

      expect(result).toBe("Result: 100");
    });
  });
});

import { describe, it, expect } from "vitest";
import { isE2EAuthAvailable, type E2EEnvironment } from "@/aspects/e2e";

describe("aspects/e2e", () => {
  describe("isE2EAuthAvailable", () => {
    it.each<{ label: string; environment: E2EEnvironment }>([
      {
        label: "エミュレータ無効 + E2E有効",
        environment: { e2eAuthEnabled: "true", useFirebaseEmulator: "false" },
      },
      {
        label: "両方未設定",
        environment: {
          e2eAuthEnabled: undefined,
          useFirebaseEmulator: undefined,
        },
      },
      {
        label: "エミュレータ無効 + E2E無効",
        environment: { e2eAuthEnabled: "false", useFirebaseEmulator: "false" },
      },
      {
        label: "エミュレータ未設定 + E2E有効",
        environment: {
          e2eAuthEnabled: "true",
          useFirebaseEmulator: undefined,
        },
      },
    ])(
      "$label の場合はエミュレータ未有効で利用不可を返す",
      ({ environment }) => {
        const result = isE2EAuthAvailable(environment);

        expect(result.available).toBe(false);
        expect(result).toStrictEqual({
          available: false,
          reason: "Firebase emulator is not enabled",
        });
      },
    );

    it.each<{ label: string; environment: E2EEnvironment }>([
      {
        label: "エミュレータ有効 + E2E無効",
        environment: { e2eAuthEnabled: "false", useFirebaseEmulator: "true" },
      },
      {
        label: "エミュレータ有効 + E2E未設定",
        environment: {
          e2eAuthEnabled: undefined,
          useFirebaseEmulator: "true",
        },
      },
    ])(
      "$label の場合はE2E認証未有効で利用不可を返す",
      ({ environment }) => {
        const result = isE2EAuthAvailable(environment);

        expect(result).toStrictEqual({
          available: false,
          reason: "E2E auth is not enabled",
        });
      },
    );

    it("エミュレータ有効 + E2E有効 の場合は利用可能を返す", () => {
      const environment: E2EEnvironment = {
        e2eAuthEnabled: "true",
        useFirebaseEmulator: "true",
      };

      const result = isE2EAuthAvailable(environment);

      expect(result).toStrictEqual({ available: true });
    });
  });
});

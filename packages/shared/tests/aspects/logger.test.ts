import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { Logger, Level, Environment } from "@shared/aspects/logger";

describe("aspects/logger", () => {
  let consoleLogSpy: ReturnType<typeof vi.spyOn>;

  beforeEach(() => {
    consoleLogSpy = vi.spyOn(console, "log").mockImplementation(() => {});
  });

  afterEach(() => {
    consoleLogSpy.mockRestore();
  });

  describe("Level", () => {
    it("正しいログレベルの値が定義されている", () => {
      expect(Level.DEBUG).toBe("debug");
      expect(Level.INFO).toBe("info");
      expect(Level.WARN).toBe("warn");
      expect(Level.ERROR).toBe("error");
    });
  });

  describe("Environment", () => {
    it("正しい環境の値が定義されている", () => {
      expect(Environment.PRODUCTION).toBe("production");
      expect(Environment.STAGING).toBe("staging");
      expect(Environment.DEVELOPMENT).toBe("development");
    });
  });

  describe("Logger", () => {
    describe("開発環境での動作", () => {
      it("debugメソッドがログを出力する", () => {
        const logger = Logger(Environment.DEVELOPMENT);

        logger.debug("デバッグメッセージ");

        expect(consoleLogSpy).toHaveBeenCalledTimes(1);
        expect(consoleLogSpy.mock.calls[0][0]).toContain("[DEBUG]");
        expect(consoleLogSpy.mock.calls[0][0]).toContain("デバッグメッセージ");
      });

      it("infoメソッドがログを出力する", () => {
        const logger = Logger(Environment.DEVELOPMENT);

        logger.info("情報メッセージ");

        expect(consoleLogSpy).toHaveBeenCalledTimes(1);
        expect(consoleLogSpy.mock.calls[0][0]).toContain("[INFO]");
        expect(consoleLogSpy.mock.calls[0][0]).toContain("情報メッセージ");
      });

      it("warnメソッドがログを出力する", () => {
        const logger = Logger(Environment.DEVELOPMENT);

        logger.warn("警告メッセージ");

        expect(consoleLogSpy).toHaveBeenCalledTimes(1);
        expect(consoleLogSpy.mock.calls[0][0]).toContain("[WARN]");
        expect(consoleLogSpy.mock.calls[0][0]).toContain("警告メッセージ");
      });

      it("errorメソッドがログを出力する", () => {
        const logger = Logger(Environment.DEVELOPMENT);

        logger.error("エラーメッセージ");

        expect(consoleLogSpy).toHaveBeenCalledTimes(1);
        expect(consoleLogSpy.mock.calls[0][0]).toContain("[ERROR]");
        expect(consoleLogSpy.mock.calls[0][0]).toContain("エラーメッセージ");
      });

      it("メタデータ付きでログを出力する", () => {
        const logger = Logger(Environment.DEVELOPMENT);

        logger.info("メッセージ", { userId: "123", action: "login" });

        expect(consoleLogSpy).toHaveBeenCalledTimes(1);
        expect(consoleLogSpy.mock.calls[0][0]).toContain("[INFO]");
        expect(consoleLogSpy.mock.calls[0][0]).toContain("メッセージ");
        // メタデータはJSON形式で第2引数として渡される
        expect(consoleLogSpy.mock.calls[0][1]).toContain('"userId": "123"');
        expect(consoleLogSpy.mock.calls[0][1]).toContain('"action": "login"');
      });

      it("タイムスタンプがログに含まれる", () => {
        const logger = Logger(Environment.DEVELOPMENT);

        logger.info("タイムスタンプテスト");

        expect(consoleLogSpy).toHaveBeenCalledTimes(1);
        // ISO形式のタイムスタンプが含まれることを確認
        expect(consoleLogSpy.mock.calls[0][0]).toMatch(/\[\d{4}-\d{2}-\d{2}T/);
      });
    });

    describe("ステージング環境での動作", () => {
      it("開発環境と同じ形式でログを出力する", () => {
        const logger = Logger(Environment.STAGING);

        logger.info("ステージングログ");

        expect(consoleLogSpy).toHaveBeenCalledTimes(1);
        expect(consoleLogSpy.mock.calls[0][0]).toContain("[INFO]");
        expect(consoleLogSpy.mock.calls[0][0]).toContain("ステージングログ");
      });
    });

    describe("本番環境での動作", () => {
      it("JSON形式でログを出力する", () => {
        const logger = Logger(Environment.PRODUCTION);

        logger.info("本番ログ");

        expect(consoleLogSpy).toHaveBeenCalledTimes(1);
        const logOutput = consoleLogSpy.mock.calls[0][0];
        const parsed = JSON.parse(logOutput);
        expect(parsed.level).toBe("info");
        expect(parsed.message).toBe("本番ログ");
        expect(parsed.timestamp).toBeDefined();
      });

      it("メタデータ付きでJSON形式のログを出力する", () => {
        const logger = Logger(Environment.PRODUCTION);

        logger.error("エラー発生", { errorCode: "E001", stack: "..." });

        expect(consoleLogSpy).toHaveBeenCalledTimes(1);
        const logOutput = consoleLogSpy.mock.calls[0][0];
        const parsed = JSON.parse(logOutput);
        expect(parsed.level).toBe("error");
        expect(parsed.message).toBe("エラー発生");
        expect(parsed.meta).toEqual({ errorCode: "E001", stack: "..." });
      });

      it("全てのログレベルでJSON形式を使用する", () => {
        const logger = Logger(Environment.PRODUCTION);

        logger.debug("debug");
        logger.info("info");
        logger.warn("warn");
        logger.error("error");

        expect(consoleLogSpy).toHaveBeenCalledTimes(4);

        // 各呼び出しがJSON形式であることを確認
        for (let i = 0; i < 4; i++) {
          const logOutput = consoleLogSpy.mock.calls[i][0];
          expect(() => JSON.parse(logOutput)).not.toThrow();
        }
      });
    });

    describe("カスタムエントリフォーマット", () => {
      it("カスタムフォーマットを使用できる", () => {
        const customFormat = (
          level: Level,
          message: string,
          meta?: Record<string, unknown>
        ) => ({
          level,
          message,
          meta,
          timestamp: "2024-01-01T00:00:00.000Z",
          custom: true,
        });

        const logger = Logger(Environment.PRODUCTION, customFormat);

        logger.info("カスタムフォーマットテスト");

        expect(consoleLogSpy).toHaveBeenCalledTimes(1);
        const logOutput = consoleLogSpy.mock.calls[0][0];
        const parsed = JSON.parse(logOutput);
        expect(parsed.custom).toBe(true);
        expect(parsed.timestamp).toBe("2024-01-01T00:00:00.000Z");
      });
    });
  });

  describe("Loggerインターフェース", () => {
    it("Loggerが全ての必要なメソッドを持つ", () => {
      const logger = Logger(Environment.DEVELOPMENT);

      expect(typeof logger.debug).toBe("function");
      expect(typeof logger.info).toBe("function");
      expect(typeof logger.warn).toBe("function");
      expect(typeof logger.error).toBe("function");
    });
  });
});

/**
 * @vitest-environment jsdom
 */
import { describe, it, expect, vi } from "vitest";
import { renderHook, act, waitFor } from "@testing-library/react";
import { useServerAction } from "@shared/hooks/useServerAction";

describe("hooks/useServerAction", () => {
  describe("初期状態", () => {
    it("初期状態が正しく設定される", () => {
      const mockAction = vi.fn();
      const { result } = renderHook(() => useServerAction(mockAction));

      expect(result.current.data).toBeNull();
      expect(result.current.error).toBeNull();
      expect(result.current.isLoading).toBe(false);
      expect(result.current.isError).toBe(false);
      expect(result.current.isSuccess).toBe(false);
    });
  });

  describe("execute", () => {
    it("成功時にdataとisSuccessが設定される", async () => {
      const mockAction = vi.fn().mockResolvedValue({ id: 1, name: "test" });
      const { result } = renderHook(() => useServerAction(mockAction));

      await act(async () => {
        await result.current.execute();
      });

      expect(result.current.data).toEqual({ id: 1, name: "test" });
      expect(result.current.isSuccess).toBe(true);
      expect(result.current.isError).toBe(false);
      expect(result.current.isLoading).toBe(false);
    });

    it("実行中はisLoadingがtrueになる", async () => {
      let resolvePromise: (value: string) => void;
      const mockAction = vi.fn(
        () =>
          new Promise<string>((resolve) => {
            resolvePromise = resolve;
          })
      );
      const { result } = renderHook(() => useServerAction(mockAction));

      act(() => {
        result.current.execute();
      });

      expect(result.current.isLoading).toBe(true);

      await act(async () => {
        resolvePromise!("done");
      });

      expect(result.current.isLoading).toBe(false);
    });

    it("引数を正しくアクションに渡す", async () => {
      const mockAction = vi.fn().mockResolvedValue("result");
      const { result } = renderHook(() =>
        useServerAction(mockAction)
      );

      await act(async () => {
        await result.current.execute("arg1", 123);
      });

      expect(mockAction).toHaveBeenCalledWith("arg1", 123);
    });

    it("成功時に結果を返す", async () => {
      const mockAction = vi.fn().mockResolvedValue("success");
      const { result } = renderHook(() => useServerAction(mockAction));

      let returnValue: string | null = null;
      await act(async () => {
        returnValue = await result.current.execute();
      });

      expect(returnValue).toBe("success");
    });
  });

  describe("エラーハンドリング", () => {
    it("Errorオブジェクトの場合はmessageを取得する", async () => {
      const mockAction = vi.fn().mockRejectedValue(new Error("Something went wrong"));
      const { result } = renderHook(() => useServerAction(mockAction));

      await act(async () => {
        await result.current.execute();
      });

      expect(result.current.isError).toBe(true);
      expect(result.current.error?.message).toBe("Something went wrong");
      expect(result.current.data).toBeNull();
    });

    it("errorsプロパティを持つErrorの場合はdetailsを取得する", async () => {
      const errorWithDetails = Object.assign(new Error("Validation failed"), {
        errors: [
          { field: "name", description: "Name is required" },
          { field: "email", description: "Invalid email format" },
        ],
      });
      const mockAction = vi.fn().mockRejectedValue(errorWithDetails);
      const { result } = renderHook(() => useServerAction(mockAction));

      await act(async () => {
        await result.current.execute();
      });

      expect(result.current.error?.message).toBe("Validation failed");
      expect(result.current.error?.details).toEqual([
        { field: "name", description: "Name is required" },
        { field: "email", description: "Invalid email format" },
      ]);
    });

    it("文字列エラーの場合はmessageとして設定される", async () => {
      const mockAction = vi.fn().mockRejectedValue("String error");
      const { result } = renderHook(() => useServerAction(mockAction));

      await act(async () => {
        await result.current.execute();
      });

      expect(result.current.error?.message).toBe("String error");
    });

    it("不明なエラーの場合はデフォルトメッセージが設定される", async () => {
      const mockAction = vi.fn().mockRejectedValue({ unexpected: true });
      const { result } = renderHook(() => useServerAction(mockAction));

      await act(async () => {
        await result.current.execute();
      });

      expect(result.current.error?.message).toBe("予期しないエラーが発生しました");
    });

    it("エラー時はnullを返す", async () => {
      const mockAction = vi.fn().mockRejectedValue(new Error("error"));
      const { result } = renderHook(() => useServerAction(mockAction));

      let returnValue: unknown = "initial";
      await act(async () => {
        returnValue = await result.current.execute();
      });

      expect(returnValue).toBeNull();
    });
  });

  describe("clearError", () => {
    it("エラー状態をクリアする", async () => {
      const mockAction = vi.fn().mockRejectedValue(new Error("error"));
      const { result } = renderHook(() => useServerAction(mockAction));

      await act(async () => {
        await result.current.execute();
      });

      expect(result.current.isError).toBe(true);

      act(() => {
        result.current.clearError();
      });

      expect(result.current.isError).toBe(false);
      expect(result.current.error).toBeNull();
    });

    it("他の状態は保持される", async () => {
      const mockAction = vi
        .fn()
        .mockResolvedValueOnce("success")
        .mockRejectedValueOnce(new Error("error"));
      const { result } = renderHook(() => useServerAction(mockAction));

      // まず成功させる
      await act(async () => {
        await result.current.execute();
      });

      expect(result.current.isSuccess).toBe(true);

      // 次にエラーを発生させる
      await act(async () => {
        await result.current.execute();
      });

      // clearErrorを呼ぶ
      act(() => {
        result.current.clearError();
      });

      expect(result.current.isError).toBe(false);
      expect(result.current.error).toBeNull();
    });
  });

  describe("reset", () => {
    it("すべての状態を初期値にリセットする", async () => {
      const mockAction = vi.fn().mockResolvedValue("success");
      const { result } = renderHook(() => useServerAction(mockAction));

      await act(async () => {
        await result.current.execute();
      });

      expect(result.current.data).toBe("success");
      expect(result.current.isSuccess).toBe(true);

      act(() => {
        result.current.reset();
      });

      expect(result.current.data).toBeNull();
      expect(result.current.error).toBeNull();
      expect(result.current.isLoading).toBe(false);
      expect(result.current.isError).toBe(false);
      expect(result.current.isSuccess).toBe(false);
    });
  });

  describe("連続実行", () => {
    it("新しい実行時に前の状態がリセットされる", async () => {
      const mockAction = vi
        .fn()
        .mockResolvedValueOnce("first")
        .mockResolvedValueOnce("second");
      const { result } = renderHook(() => useServerAction(mockAction));

      await act(async () => {
        await result.current.execute();
      });

      expect(result.current.data).toBe("first");

      await act(async () => {
        await result.current.execute();
      });

      expect(result.current.data).toBe("second");
    });

    it("成功後にエラーが発生した場合、エラー状態に遷移する", async () => {
      const mockAction = vi
        .fn()
        .mockResolvedValueOnce("success")
        .mockRejectedValueOnce(new Error("error"));
      const { result } = renderHook(() => useServerAction(mockAction));

      await act(async () => {
        await result.current.execute();
      });

      expect(result.current.isSuccess).toBe(true);

      await act(async () => {
        await result.current.execute();
      });

      expect(result.current.isSuccess).toBe(false);
      expect(result.current.isError).toBe(true);
      expect(result.current.data).toBeNull();
    });
  });
});

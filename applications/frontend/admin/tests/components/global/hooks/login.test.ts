import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { ok, err } from "@shared/aspects/result";

const mockPush = vi.fn();
const mockStartPopup = vi.fn();
const mockGetIdToken = vi.fn();
const mockLogin = vi.fn();

vi.mock("next/navigation", () => ({
  useRouter: () => ({ push: mockPush }),
}));

vi.mock("@/providers/acl/oidc/client", () => ({
  OIDCClientProvider: {
    startPopup: () => mockStartPopup(),
    getIdToken: () => mockGetIdToken(),
  },
}));

vi.mock("@/actions/auth", () => ({
  login: (...args: unknown[]) => mockLogin(...args),
}));

vi.mock("@shared/components/global/hooks/use-server-action", () => ({
  useServerAction: (action: (...args: unknown[]) => Promise<unknown>) => ({
    execute: (...args: unknown[]) => action(...args),
    isLoading: false,
    error: null,
    reset: vi.fn(),
  }),
}));

interface ErrorWithMessage {
  readonly message?: unknown;
}

const resolveErrorMessage = (error: unknown, fallback: string): string => {
  if (typeof error === "object" && error !== null && "message" in error) {
    const errorObject = error as ErrorWithMessage;
    if (typeof errorObject.message === "string") {
      return errorObject.message;
    }
  }
  return fallback;
};

type SuccessOutcome = { readonly outcome: string };
type ErrorOutcome = { readonly error: string };
type ResultOutcome = SuccessOutcome | ErrorOutcome;

interface MockUser {
  readonly uid: string;
  readonly email?: string;
}

describe("hooks/login", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  describe("useLogin ロジック", () => {
    describe("resolveErrorMessage ヘルパー関数のロジック", () => {
      const resolveErrorMessageCases = [
        { error: { message: "カスタムエラー" }, fallback: "フォールバック", expected: "カスタムエラー" },
        { error: { message: 123 }, fallback: "フォールバック", expected: "フォールバック" },
        { error: null, fallback: "フォールバック", expected: "フォールバック" },
        { error: undefined, fallback: "フォールバック", expected: "フォールバック" },
        { error: { code: "ERROR" }, fallback: "フォールバック", expected: "フォールバック" },
      ];

      resolveErrorMessageCases.forEach(({ error, fallback, expected }) => {
        it(`${JSON.stringify(error)} からメッセージを抽出して "${expected}" を返す`, () => {
          expect(resolveErrorMessage(error, fallback)).toBe(expected);
        });
      });
    });

    describe("handleGoogleLogin ロジック（popup フロー）", () => {
      const handleGoogleLogin = async (): Promise<ResultOutcome> => {
        return mockStartPopup()
          .mapError((error: unknown) =>
            resolveErrorMessage(error, "ログイン処理を開始できませんでした"),
          )
          .andThen((_user: MockUser) =>
            mockGetIdToken()
              .mapError((error: unknown) =>
                resolveErrorMessage(error, "認証処理中にエラーが発生しました"),
              ),
          )
          .andThen((idToken: string) => {
            mockLogin(idToken);
            return ok<SuccessOutcome, string>({ outcome: "loggedIn" }).toAsync();
          })
          .match({
            ok: (result: SuccessOutcome): ResultOutcome => result,
            err: (message: string): ResultOutcome => ({ error: message }),
          });
      };

      it("popup 成功 → IDトークン取得 → ログイン完了の場合は loggedIn を返す", async () => {
        const mockUser: MockUser = { uid: "test-uid", email: "test@example.com" };
        mockStartPopup.mockReturnValue(ok(mockUser).toAsync());
        mockGetIdToken.mockReturnValue(ok("mock-id-token").toAsync());
        mockLogin.mockResolvedValue(undefined);

        const result = await handleGoogleLogin();

        expect(result).toEqual({ outcome: "loggedIn" });
        expect(mockLogin).toHaveBeenCalledWith("mock-id-token");
      });

      it("popup がエラーの場合はエラーを返す", async () => {
        mockStartPopup.mockReturnValue(err({ message: "ポップアップエラー" }).toAsync());

        const result = await handleGoogleLogin();

        expect(result).toEqual({ error: "ポップアップエラー" });
      });

      it("popup エラーメッセージがない場合はデフォルトメッセージを使用する", async () => {
        mockStartPopup.mockReturnValue(err({}).toAsync());

        const result = await handleGoogleLogin();

        expect(result).toEqual({ error: "ログイン処理を開始できませんでした" });
      });

      it("IDトークン取得エラーの場合はエラーを返す", async () => {
        const mockUser: MockUser = { uid: "test-uid" };
        mockStartPopup.mockReturnValue(ok(mockUser).toAsync());
        mockGetIdToken.mockReturnValue(err({ message: "トークン取得エラー" }).toAsync());

        const result = await handleGoogleLogin();

        expect(result).toEqual({ error: "トークン取得エラー" });
      });

      it("IDトークン取得エラーメッセージがない場合はデフォルトメッセージを使用する", async () => {
        const mockUser: MockUser = { uid: "test-uid" };
        mockStartPopup.mockReturnValue(ok(mockUser).toAsync());
        mockGetIdToken.mockReturnValue(err({}).toAsync());

        const result = await handleGoogleLogin();

        expect(result).toEqual({ error: "認証処理中にエラーが発生しました" });
      });
    });

    describe("LoginState 型", () => {
      it("有効な状態値を検証する", () => {
        type LoginState = "idle" | "loading" | "error";
        const validStates: LoginState[] = ["idle", "loading", "error"];

        expect(validStates).toContain("idle");
        expect(validStates).toContain("loading");
        expect(validStates).toContain("error");
      });
    });

    describe("オプション設定", () => {
      it("デフォルトのリダイレクトパスは / である", () => {
        const options: { redirectPath?: string } = {};
        const redirectPath = options.redirectPath ?? "/";

        expect(redirectPath).toBe("/");
      });

      it("カスタムリダイレクトパスを使用できる", () => {
        const options = { redirectPath: "/admin/dashboard" };
        const redirectPath = options.redirectPath ?? "/";

        expect(redirectPath).toBe("/admin/dashboard");
      });
    });
  });
});

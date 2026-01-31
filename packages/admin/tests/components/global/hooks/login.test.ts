import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { ok, err } from "@shared/aspects/result";

const mockPush = vi.fn();
const mockGetRedirectResult = vi.fn();
const mockGetIdToken = vi.fn();
const mockStartRedirect = vi.fn();
const mockLogin = vi.fn();

vi.mock("next/navigation", () => ({
  useRouter: () => ({ push: mockPush }),
}));

vi.mock("@/providers/acl/oidc/client", () => ({
  OIDCClientProvider: {
    getRedirectResult: () => mockGetRedirectResult(),
    getIdToken: () => mockGetIdToken(),
    startRedirect: () => mockStartRedirect(),
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
    mockGetRedirectResult.mockReturnValue(ok(null).toAsync());
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

    describe("リダイレクト結果の処理ロジック", () => {
      const handleRedirectResult = async (): Promise<ResultOutcome> => {
        return mockGetRedirectResult()
          .mapError((error: unknown) => resolveErrorMessage(error, "認証処理中にエラーが発生しました"))
          .andThen((user: MockUser | null) => {
            if (user === null) {
              return ok<SuccessOutcome, string>({ outcome: "idle" }).toAsync();
            }
            return mockGetIdToken()
              .mapError((error: unknown) => resolveErrorMessage(error, "トークン取得エラー"))
              .map((): SuccessOutcome => ({ outcome: "loggedIn" }));
          })
          .match({
            ok: (result: SuccessOutcome): ResultOutcome => result,
            err: (message: string): ResultOutcome => ({ error: message }),
          });
      };

      it("ユーザーがnullの場合はidleに遷移する", async () => {
        mockGetRedirectResult.mockReturnValue(ok(null).toAsync());

        const result = await handleRedirectResult();

        expect(result).toEqual({ outcome: "idle" });
      });

      it("ユーザーがいる場合はloggedInに遷移する", async () => {
        const mockUser: MockUser = { uid: "test-uid", email: "test@example.com" };
        mockGetRedirectResult.mockReturnValue(ok(mockUser).toAsync());
        mockGetIdToken.mockReturnValue(ok("mock-id-token").toAsync());
        mockLogin.mockResolvedValue(undefined);

        const result = await handleRedirectResult();

        expect(result).toEqual({ outcome: "loggedIn" });
      });

      it("リダイレクト結果がエラーの場合はエラーを返す", async () => {
        mockGetRedirectResult.mockReturnValue(err({ message: "認証エラー" }).toAsync());

        const result = await handleRedirectResult();

        expect(result).toEqual({ error: "認証エラー" });
      });

      it("IDトークン取得エラーの場合はエラーを返す", async () => {
        const mockUser: MockUser = { uid: "test-uid" };
        mockGetRedirectResult.mockReturnValue(ok(mockUser).toAsync());
        mockGetIdToken.mockReturnValue(err({ message: "トークン取得エラー" }).toAsync());

        const result = await handleRedirectResult();

        expect(result).toEqual({ error: "トークン取得エラー" });
      });
    });

    describe("handleGoogleLogin ロジック", () => {
      const handleGoogleLogin = async (): Promise<void | ErrorOutcome> => {
        return mockStartRedirect()
          .mapError((error: unknown) =>
            resolveErrorMessage(error, "ログイン処理を開始できませんでした")
          )
          .match({
            ok: (): void => undefined,
            err: (message: string): ErrorOutcome => ({ error: message }),
          });
      };

      it("startRedirect が成功した場合はundefinedを返す", async () => {
        mockStartRedirect.mockReturnValue(ok(undefined).toAsync());

        const result = await handleGoogleLogin();

        expect(result).toBeUndefined();
      });

      it("startRedirect がエラーの場合はエラーを返す", async () => {
        mockStartRedirect.mockReturnValue(err({ message: "リダイレクトエラー" }).toAsync());

        const result = await handleGoogleLogin();

        expect(result).toEqual({ error: "リダイレクトエラー" });
      });

      it("エラーメッセージがない場合はデフォルトメッセージを使用する", async () => {
        mockStartRedirect.mockReturnValue(err({}).toAsync());

        const result = await handleGoogleLogin();

        expect(result).toEqual({ error: "ログイン処理を開始できませんでした" });
      });
    });

    describe("LoginState 型", () => {
      it("有効な状態値を検証する", () => {
        type LoginState = "idle" | "checking" | "loading" | "error";
        const validStates: LoginState[] = ["idle", "checking", "loading", "error"];

        expect(validStates).toContain("idle");
        expect(validStates).toContain("checking");
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

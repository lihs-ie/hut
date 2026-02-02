import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import type { Auth, User, UserInfo, Unsubscribe } from "firebase/auth";
import {
  createOIDCAuth,
  userCancelledError,
  networkError,
  invalidCredentialError,
  userDisabledError,
  configurationError,
  oidcPermissionDeniedError,
  oidcUnexpectedError,
  isUserCancelledError,
  isNetworkError,
  isInvalidCredentialError,
  isUserDisabledError,
  isConfigurationError,
  isOidcPermissionDeniedError,
  isOidcUnexpectedError,
  type OidcAuthConfig,
} from "@/aspects/auth/oidc";

vi.mock("firebase/auth", () => {
  const GoogleAuthProvider = vi.fn();
  Object.defineProperty(GoogleAuthProvider, "PROVIDER_ID", {
    value: "google.com",
  });

  return {
    GoogleAuthProvider,
    signInWithRedirect: vi.fn(),
    getRedirectResult: vi.fn(),
    signOut: vi.fn(),
    onAuthStateChanged: vi.fn(),
  };
});

const DEFAULT_PROVIDER_DATA: UserInfo[] = [
  {
    providerId: "google.com",
    uid: "test-uid",
    displayName: "Test User",
    email: "test@example.com",
    phoneNumber: null,
    photoURL: "https://example.com/photo.jpg",
  },
];

interface MockUserOverrides {
  readonly uid?: string;
  readonly email?: string | null;
  readonly displayName?: string;
  readonly photoURL?: string;
  readonly providerData?: UserInfo[];
}

const createMockUser = (overrides: MockUserOverrides = {}): User => {
  const hasEmailOverride = "email" in overrides;
  const user = {
    uid: overrides.uid ?? "test-uid",
    email: hasEmailOverride ? overrides.email : "test@example.com",
    displayName: overrides.displayName ?? "Test User",
    photoURL: overrides.photoURL ?? "https://example.com/photo.jpg",
    providerData: overrides.providerData ?? DEFAULT_PROVIDER_DATA,
    emailVerified: true,
    isAnonymous: false,
    metadata: {},
    phoneNumber: null,
    providerId: "google.com",
    refreshToken: "mock-refresh-token",
    tenantId: null,
    delete: vi.fn(),
    getIdToken: vi.fn().mockResolvedValue("mock-id-token"),
    getIdTokenResult: vi.fn(),
    reload: vi.fn(),
    toJSON: vi.fn(),
  };
  return user as User;
};

interface MockAuth {
  currentUser: User | null;
}

const createMockAuth = (currentUser: User | null = null): MockAuth => ({
  currentUser,
});

interface MockUserCredential {
  readonly user: User;
  readonly providerId: string;
  readonly operationType: string;
}

const createMockUserCredential = (
  user: User,
  providerId = "google.com"
): MockUserCredential => ({
  user,
  providerId,
  operationType: "signIn",
});

describe("aspects/auth/oidc", () => {
  describe("エラー生成関数", () => {
    const errorFactoryTests = [
      { factory: userCancelledError, tag: "UserCancelledError" },
      { factory: networkError, tag: "NetworkError" },
      { factory: invalidCredentialError, tag: "InvalidCredentialError" },
      { factory: userDisabledError, tag: "UserDisabledError" },
      { factory: configurationError, tag: "ConfigurationError" },
      { factory: oidcPermissionDeniedError, tag: "OidcPermissionDeniedError" },
    ];

    errorFactoryTests.forEach(({ factory, tag }) => {
      it(`${tag} を作成できる`, () => {
        const error = factory("テストメッセージ");

        expect(error._tag).toBe(tag);
        expect(error.message).toBe("テストメッセージ");
      });
    });

    describe("oidcUnexpectedError", () => {
      it("cause なしで作成できる", () => {
        const error = oidcUnexpectedError("予期しないエラー");

        expect(error._tag).toBe("OidcUnexpectedError");
        expect(error.message).toBe("予期しないエラー");
        expect(error.cause).toBeUndefined();
      });

      it("cause ありで作成できる", () => {
        const cause = new Error("Original error");
        const error = oidcUnexpectedError("予期しないエラー", cause);

        expect(error.cause).toBe(cause);
      });
    });
  });

  describe("エラー判定関数", () => {
    const errorTypeGuardTests = [
      { guard: isUserCancelledError, factory: userCancelledError },
      { guard: isNetworkError, factory: networkError },
      { guard: isInvalidCredentialError, factory: invalidCredentialError },
      { guard: isUserDisabledError, factory: userDisabledError },
      { guard: isConfigurationError, factory: configurationError },
      { guard: isOidcPermissionDeniedError, factory: oidcPermissionDeniedError },
      { guard: isOidcUnexpectedError, factory: oidcUnexpectedError },
    ];

    errorTypeGuardTests.forEach(({ guard, factory }) => {
      it(`${factory.name} のエラーを正しく判定する`, () => {
        const error = factory("message");
        expect(guard(error)).toBe(true);
      });

      it(`${guard.name} は無効な値に対して false を返す`, () => {
        expect(guard(null)).toBe(false);
        expect(guard(undefined)).toBe(false);
        expect(guard({})).toBe(false);
      });
    });

    it("各エラー型が相互に排他的である", () => {
      const errors = [
        userCancelledError("message"),
        networkError("message"),
        invalidCredentialError("message"),
        userDisabledError("message"),
        configurationError("message"),
        oidcPermissionDeniedError("message"),
        oidcUnexpectedError("message"),
      ];

      const guards = [
        isUserCancelledError,
        isNetworkError,
        isInvalidCredentialError,
        isUserDisabledError,
        isConfigurationError,
        isOidcPermissionDeniedError,
        isOidcUnexpectedError,
      ];

      errors.forEach((error, errorIndex) => {
        guards.forEach((guard, guardIndex) => {
          const expected = errorIndex === guardIndex;
          expect(guard(error)).toBe(expected);
        });
      });
    });
  });

  describe("createOIDCAuth", () => {
    let mockAuth: MockAuth;
    let mockFirebaseAuth: typeof import("firebase/auth");
    const defaultConfig: OidcAuthConfig = { allowedEmails: [] };

    beforeEach(async () => {
      vi.clearAllMocks();
      mockFirebaseAuth = await import("firebase/auth");
      mockAuth = createMockAuth();
    });

    afterEach(() => {
      vi.restoreAllMocks();
    });

    describe("startRedirect", () => {
      it("signInWithRedirect を呼び出す", async () => {
        vi.mocked(mockFirebaseAuth.signInWithRedirect).mockResolvedValue(undefined as never);

        const oidcAuth = createOIDCAuth(mockAuth as Auth, defaultConfig);
        const result = await oidcAuth.startRedirect().unwrap();

        expect(result).toBeUndefined();
        expect(mockFirebaseAuth.signInWithRedirect).toHaveBeenCalledWith(
          mockAuth,
          expect.any(Object)
        );
      });

      it("Firebase エラーを OidcAuthError にマッピングする", async () => {
        vi.mocked(mockFirebaseAuth.signInWithRedirect).mockRejectedValue({
          code: "auth/network-request-failed",
          message: "Network error",
        });

        const oidcAuth = createOIDCAuth(mockAuth as Auth, defaultConfig);
        const error = await oidcAuth.startRedirect().unwrapError();

        expect(isNetworkError(error)).toBe(true);
      });
    });

    describe("getRedirectResult", () => {
      it("リダイレクト結果がない場合は null を返す", async () => {
        vi.mocked(mockFirebaseAuth.getRedirectResult).mockResolvedValue(null);

        const oidcAuth = createOIDCAuth(mockAuth as Auth, defaultConfig);
        const result = await oidcAuth.getRedirectResult().unwrap();

        expect(result).toBeNull();
      });

      it("正常なリダイレクト結果から OidcUser を返す", async () => {
        const mockUser = createMockUser({
          uid: "user-123",
          email: "admin@example.com",
          displayName: "Admin User",
          photoURL: "https://example.com/admin.jpg",
        });
        vi.mocked(mockFirebaseAuth.getRedirectResult).mockResolvedValue(
          createMockUserCredential(mockUser) as ReturnType<typeof mockFirebaseAuth.getRedirectResult> extends Promise<infer T> ? T : never
        );

        const config: OidcAuthConfig = { allowedEmails: ["admin@example.com"] };
        const oidcAuth = createOIDCAuth(mockAuth as Auth, config);
        const result = await oidcAuth.getRedirectResult().unwrap();

        expect(result).toEqual({
          uid: "user-123",
          email: "admin@example.com",
          displayName: "Admin User",
          photoURL: "https://example.com/admin.jpg",
        });
      });

      it("許可されていないメールアドレスの場合はエラーを返す", async () => {
        const mockUser = createMockUser({ email: "notallowed@example.com" });
        vi.mocked(mockFirebaseAuth.getRedirectResult).mockResolvedValue(
          createMockUserCredential(mockUser) as ReturnType<typeof mockFirebaseAuth.getRedirectResult> extends Promise<infer T> ? T : never
        );
        vi.mocked(mockFirebaseAuth.signOut).mockResolvedValue(undefined);

        const config: OidcAuthConfig = { allowedEmails: ["admin@example.com"] };
        const oidcAuth = createOIDCAuth(mockAuth as Auth, config);
        const error = await oidcAuth.getRedirectResult().unwrapError();

        expect(isOidcPermissionDeniedError(error)).toBe(true);
        expect(mockFirebaseAuth.signOut).toHaveBeenCalled();
      });

      it("Google 以外のプロバイダーの場合はエラーを返す", async () => {
        const mockUser = createMockUser();
        vi.mocked(mockFirebaseAuth.getRedirectResult).mockResolvedValue(
          createMockUserCredential(mockUser, "facebook.com") as ReturnType<typeof mockFirebaseAuth.getRedirectResult> extends Promise<infer T> ? T : never
        );
        vi.mocked(mockFirebaseAuth.signOut).mockResolvedValue(undefined);

        const oidcAuth = createOIDCAuth(mockAuth as Auth, defaultConfig);
        const error = await oidcAuth.getRedirectResult().unwrapError();

        expect(isInvalidCredentialError(error)).toBe(true);
      });
    });

    describe("getCurrentUser", () => {
      it("現在のユーザーがいない場合は null を返す", async () => {
        const oidcAuth = createOIDCAuth(mockAuth as Auth, defaultConfig);
        const result = await oidcAuth.getCurrentUser().unwrap();

        expect(result).toBeNull();
      });

      it("現在のユーザーがいる場合は OidcUser を返す", async () => {
        const mockUser = createMockUser({
          uid: "current-user-123",
          email: "current@example.com",
        });
        mockAuth.currentUser = mockUser;

        const oidcAuth = createOIDCAuth(mockAuth as Auth, defaultConfig);
        const result = await oidcAuth.getCurrentUser().unwrap();

        expect(result?.uid).toBe("current-user-123");
        expect(result?.email).toBe("current@example.com");
      });

      it("許可されていないユーザーの場合はエラーを返す", async () => {
        const mockUser = createMockUser({ email: "notallowed@example.com" });
        mockAuth.currentUser = mockUser;
        vi.mocked(mockFirebaseAuth.signOut).mockResolvedValue(undefined);

        const config: OidcAuthConfig = { allowedEmails: ["admin@example.com"] };
        const oidcAuth = createOIDCAuth(mockAuth as Auth, config);
        const error = await oidcAuth.getCurrentUser().unwrapError();

        expect(isOidcPermissionDeniedError(error)).toBe(true);
      });
    });

    describe("signOut", () => {
      it("Firebase signOut を呼び出す", async () => {
        vi.mocked(mockFirebaseAuth.signOut).mockResolvedValue(undefined);

        const oidcAuth = createOIDCAuth(mockAuth as Auth, defaultConfig);
        const result = await oidcAuth.signOut().unwrap();

        expect(result).toBeUndefined();
        expect(mockFirebaseAuth.signOut).toHaveBeenCalledWith(mockAuth);
      });
    });

    describe("getIdToken", () => {
      it("現在のユーザーがいない場合はエラーを返す", async () => {
        const oidcAuth = createOIDCAuth(mockAuth as Auth, defaultConfig);
        const error = await oidcAuth.getIdToken().unwrapError();

        expect(isInvalidCredentialError(error)).toBe(true);
        expect(error.message).toBe("No authenticated user");
      });

      it("ID トークンを返す", async () => {
        const mockUser = createMockUser();
        mockUser.getIdToken = vi.fn().mockResolvedValue("mock-id-token-123");
        mockAuth.currentUser = mockUser;

        const oidcAuth = createOIDCAuth(mockAuth as Auth, defaultConfig);
        const result = await oidcAuth.getIdToken().unwrap();

        expect(result).toBe("mock-id-token-123");
      });
    });

    describe("onAuthStateChanged", () => {
      it("認証状態の変更をリッスンする", async () => {
        const mockUnsubscribe: Unsubscribe = vi.fn();
        vi.mocked(mockFirebaseAuth.onAuthStateChanged).mockReturnValue(mockUnsubscribe);

        const oidcAuth = createOIDCAuth(mockAuth as Auth, defaultConfig);
        const handler = vi.fn();
        const unsubscribe = await oidcAuth.onAuthStateChanged(handler).unwrap();

        expect(typeof unsubscribe).toBe("function");
        expect(mockFirebaseAuth.onAuthStateChanged).toHaveBeenCalledWith(
          mockAuth,
          expect.any(Function)
        );
      });

      it("ユーザーがログインした時にハンドラーを呼び出す", async () => {
        let capturedCallback: ((user: User | null) => void) | undefined;
        vi.mocked(mockFirebaseAuth.onAuthStateChanged).mockImplementation(
          ((_auth, nextOrObserver): Unsubscribe => {
            if (typeof nextOrObserver === "function") {
              capturedCallback = nextOrObserver;
            }
            return vi.fn();
          }) as typeof mockFirebaseAuth.onAuthStateChanged
        );

        const oidcAuth = createOIDCAuth(mockAuth as Auth, defaultConfig);
        const handler = vi.fn();
        await oidcAuth.onAuthStateChanged(handler).unwrap();

        const mockUser = createMockUser({ uid: "auth-state-user" });
        capturedCallback?.(mockUser);

        expect(handler).toHaveBeenCalledWith(
          expect.objectContaining({ uid: "auth-state-user" })
        );
      });

      it("ユーザーがログアウトした時に null でハンドラーを呼び出す", async () => {
        let capturedCallback: ((user: User | null) => void) | undefined;
        vi.mocked(mockFirebaseAuth.onAuthStateChanged).mockImplementation(
          ((_auth, nextOrObserver): Unsubscribe => {
            if (typeof nextOrObserver === "function") {
              capturedCallback = nextOrObserver;
            }
            return vi.fn();
          }) as typeof mockFirebaseAuth.onAuthStateChanged
        );

        const oidcAuth = createOIDCAuth(mockAuth as Auth, defaultConfig);
        const handler = vi.fn();
        await oidcAuth.onAuthStateChanged(handler).unwrap();

        capturedCallback?.(null);

        expect(handler).toHaveBeenCalledWith(null);
      });
    });

    describe("許可メール設定", () => {
      it("allowedEmails が空の場合は全てのメールを許可する", async () => {
        const mockUser = createMockUser({ email: "anyone@example.com" });
        vi.mocked(mockFirebaseAuth.getRedirectResult).mockResolvedValue(
          createMockUserCredential(mockUser) as ReturnType<typeof mockFirebaseAuth.getRedirectResult> extends Promise<infer T> ? T : never
        );

        const oidcAuth = createOIDCAuth(mockAuth as Auth, defaultConfig);
        const result = await oidcAuth.getRedirectResult().unwrap();

        expect(result?.email).toBe("anyone@example.com");
      });

      it("大文字小文字を無視してメールを比較する", async () => {
        const mockUser = createMockUser({ email: "ADMIN@EXAMPLE.COM" });
        vi.mocked(mockFirebaseAuth.getRedirectResult).mockResolvedValue(
          createMockUserCredential(mockUser) as ReturnType<typeof mockFirebaseAuth.getRedirectResult> extends Promise<infer T> ? T : never
        );

        const config: OidcAuthConfig = { allowedEmails: ["admin@example.com"] };
        const oidcAuth = createOIDCAuth(mockAuth as Auth, config);
        const result = await oidcAuth.getRedirectResult().unwrap();

        expect(result?.email).toBe("ADMIN@EXAMPLE.COM");
      });
    });

    describe("Firebase エラーマッピング", () => {
      const firebaseErrorTests = [
        { code: "auth/redirect-cancelled-by-user", guard: isUserCancelledError },
        { code: "auth/network-request-failed", guard: isNetworkError },
        { code: "auth/invalid-credential", guard: isInvalidCredentialError },
        { code: "auth/user-disabled", guard: isUserDisabledError },
        { code: "auth/operation-not-allowed", guard: isConfigurationError },
        { code: "auth/unknown-error", guard: isOidcUnexpectedError },
      ];

      firebaseErrorTests.forEach(({ code, guard }) => {
        it(`${code} を正しくマッピングする`, async () => {
          vi.mocked(mockFirebaseAuth.signInWithRedirect).mockRejectedValue({
            code,
            message: "Error message",
          });

          const oidcAuth = createOIDCAuth(mockAuth as Auth, defaultConfig);
          const error = await oidcAuth.startRedirect().unwrapError();

          expect(guard(error)).toBe(true);
        });
      });

      it("非 Firebase エラーを OidcUnexpectedError にマッピングする", async () => {
        vi.mocked(mockFirebaseAuth.signInWithRedirect).mockRejectedValue(
          new Error("Generic error")
        );

        const oidcAuth = createOIDCAuth(mockAuth as Auth, defaultConfig);
        const error = await oidcAuth.startRedirect().unwrapError();

        expect(isOidcUnexpectedError(error)).toBe(true);
        expect(error.message).toBe("Generic error");
      });
    });

    describe("ユーザー検証", () => {
      it("メールアドレスがない場合はエラーを返す", async () => {
        const mockUser = createMockUser({ email: null });
        vi.mocked(mockFirebaseAuth.getRedirectResult).mockResolvedValue(
          createMockUserCredential(mockUser) as ReturnType<typeof mockFirebaseAuth.getRedirectResult> extends Promise<infer T> ? T : never
        );
        vi.mocked(mockFirebaseAuth.signOut).mockResolvedValue(undefined);

        const oidcAuth = createOIDCAuth(mockAuth as Auth, defaultConfig);
        const error = await oidcAuth.getRedirectResult().unwrapError();

        expect(isInvalidCredentialError(error)).toBe(true);
        expect(error.message).toBe("Missing email in Firebase user");
      });

      it("Google プロバイダー以外の場合はエラーを返す", async () => {
        const nonGoogleProviderData: UserInfo[] = [
          {
            providerId: "password",
            uid: "test-uid",
            displayName: "Test User",
            email: "test@example.com",
            phoneNumber: null,
            photoURL: null,
          },
        ];
        const mockUser = createMockUser({ providerData: nonGoogleProviderData });
        vi.mocked(mockFirebaseAuth.getRedirectResult).mockResolvedValue(
          createMockUserCredential(mockUser) as ReturnType<typeof mockFirebaseAuth.getRedirectResult> extends Promise<infer T> ? T : never
        );
        vi.mocked(mockFirebaseAuth.signOut).mockResolvedValue(undefined);

        const oidcAuth = createOIDCAuth(mockAuth as Auth, defaultConfig);
        const error = await oidcAuth.getRedirectResult().unwrapError();

        expect(isInvalidCredentialError(error)).toBe(true);
        expect(error.message).toBe("Unsupported authentication provider");
      });
    });
  });
});

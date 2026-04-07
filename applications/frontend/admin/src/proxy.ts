import { enforceLoginRateLimit } from "@/actions/rate-limit";
import { isE2EAuthAvailable } from "@/aspects/e2e";
import { NextRequest, NextResponse } from "next/server";
import { resolveIP } from "@/aspects/ip-address";
import { OIDCServerProvider } from "@/providers/acl/oidc/server";
import {
  isInvalidCredentialError,
  isUserDisabledError,
  isOidcPermissionDeniedError,
  type OidcAuthError,
} from "@/aspects/auth/oidc";

export const config = {
  matcher: ["/((?!_next/static|_next/image|favicon.ico|icon|logo).*)"],
};

/**
 * Enforce access control for admin routes.
 */
export async function proxy(request: NextRequest) {
  const adminSession = request.cookies.get("admin_session")?.value ?? null;
  const pathname = request.nextUrl.pathname;
  const isLoginPage = pathname === "/admin/login";
  const isLoginAttempt = isLoginPage && request.method === "POST";
  const isE2EAuthEndpoint = pathname === "/api/e2e/auth";

  if (isE2EAuthEndpoint) {
    const availability = isE2EAuthAvailable({
      e2eAuthEnabled: process.env.E2E_AUTH_ENABLED,
      useFirebaseEmulator: process.env.NEXT_PUBLIC_USE_FIREBASE_EMULATOR,
    });

    if (!availability.available) {
      return new NextResponse("Not found", { status: 404 });
    }

    return NextResponse.next();
  }

  if (isLoginPage) {
    if (isLoginAttempt) {
      const rateLimitKey = resolveIP(request);
      const rateLimitResult = await enforceLoginRateLimit(rateLimitKey);

      if (!rateLimitResult.allowed) {
        return new NextResponse(rateLimitResult.message, { status: 429 });
      }
    }

    if (adminSession) {
      return await OIDCServerProvider.verifySessionCookie(adminSession).match({
        ok: () => NextResponse.redirect(new URL("/", request.url)),
        err: (error: OidcAuthError) => {
          if (isRecoverableAuthError(error)) {
            const response = NextResponse.next();
            response.cookies.delete("admin_session");
            return response;
          }

          return new NextResponse("Internal Server Error", { status: 500 });
        },
      });
    }

    return NextResponse.next();
  }

  if (!adminSession) {
    return NextResponse.redirect(new URL("/admin/login", request.url));
  }

  return await OIDCServerProvider.verifySessionCookie(adminSession).match({
    ok: () => NextResponse.next(),
    err: (error: OidcAuthError) => {
      if (isRecoverableAuthError(error)) {
        const response = NextResponse.redirect(
          new URL("/admin/login", request.url),
        );
        response.cookies.delete("admin_session");
        return response;
      }

      return new NextResponse("Internal Server Error", { status: 500 });
    },
  });
}

function isRecoverableAuthError(error: OidcAuthError): boolean {
  return (
    isInvalidCredentialError(error) ||
    isUserDisabledError(error) ||
    isOidcPermissionDeniedError(error)
  );
}

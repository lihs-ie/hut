import { enforceLoginRateLimit } from "@/actions/rate-limit";
import { isE2EAuthAvailable } from "@/aspects/e2e";
import { NextRequest, NextResponse } from "next/server";
import { resolveIP } from "@/aspects/ip-address";
import { OIDCServerProvider } from "@/providers/acl/oidc/server";

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

  if (isLoginPage && adminSession) {
    const isValid = await OIDCServerProvider.verifySessionCookie(
      adminSession,
    ).match({
      ok: () => true,
      err: () => false,
    });

    if (isValid) {
      return NextResponse.redirect(new URL("/", request.url));
    }

    const response = NextResponse.next();
    response.cookies.delete("admin_session");
    return response;
  }

  if (isLoginPage) {
    if (isLoginAttempt) {
      const rateLimitKey = resolveIP(request);
      const rateLimitResult = await enforceLoginRateLimit(rateLimitKey);

      if (!rateLimitResult.allowed) {
        return new NextResponse(rateLimitResult.message, { status: 429 });
      }
    }

    return NextResponse.next();
  }

  if (!adminSession) {
    return NextResponse.redirect(new URL("/admin/login", request.url));
  }

  return await OIDCServerProvider.verifySessionCookie(adminSession).match({
    ok: () => NextResponse.next(),
    err: () => {
      const response = NextResponse.redirect(
        new URL("/admin/login", request.url),
      );
      response.cookies.delete("admin_session");
      return response;
    },
  });
}

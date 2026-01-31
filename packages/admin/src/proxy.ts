import { enforceLoginRateLimit } from "@/actions/rate-limit";
import { NextRequest, NextResponse } from "next/server";
import { resolveIP } from "./aspects/ip-address";

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
    return NextResponse.next();
  }

  if (isLoginPage && adminSession) {
    return NextResponse.redirect(new URL("/", request.url));
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

  return NextResponse.next();
}

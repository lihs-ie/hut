import { NextRequest, NextResponse } from "next/server";

export async function proxy(request: NextRequest) {
  const adminSession = request.cookies.get("admin_session")?.value ?? null;
  const pathname = request.nextUrl.pathname;
  const isLoginPage = pathname === "/admin/login";

  if (isLoginPage && adminSession) {
    return NextResponse.redirect(new URL("/", request.url));
  }

  if (!isLoginPage && !adminSession) {
    return NextResponse.redirect(new URL("/admin/login", request.url));
  }

  const requestHeaders = new Headers(request.headers);
  requestHeaders.set("x-url", request.url);
  requestHeaders.set("x-pathname", pathname);

  return NextResponse.next({
    request: {
      headers: requestHeaders,
    },
  });
}

import { NextResponse } from "next/server";
import type { NextRequest } from "next/server";

const INFORMATION_LEAK_HEADERS = [
  "X-Nextjs-Cache",
  "X-Nextjs-Prerender",
  "X-Nextjs-Stale-Time",
  "Server",
] as const;

/**
 * Reader から Next.js の内部情報が分かるレスポンスヘッダーを削除する。
 */
export function proxy(_request: NextRequest) {
  const response = NextResponse.next();

  for (const header of INFORMATION_LEAK_HEADERS) {
    response.headers.delete(header);
  }

  return response;
}

export const config = {
  matcher: [
    { source: "/((?!api|_next/static|_next/image|favicon.ico).*)" },
  ],
};

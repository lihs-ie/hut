import { NextResponse } from "next/server";
import type { NextRequest } from "next/server";

const INFORMATION_LEAK_HEADERS = [
  "X-Nextjs-Cache",
  "X-Nextjs-Prerender",
  "X-Nextjs-Stale-Time",
  "Server",
] as const;

const CLOUDFLARE_CLIENT_IP_HEADER = "CF-Connecting-IP";
const DISABLE_GUARD_FLAG = "true";

const isCloudflareGuardDisabled = (): boolean => {
  const nodeEnvironment = process.env.NODE_ENV;
  if (nodeEnvironment !== "production") {
    return true;
  }
  const disableFlag = process.env.READER_DISABLE_CLOUDFLARE_GUARD;
  return disableFlag === DISABLE_GUARD_FLAG;
};

const stripInformationLeakHeaders = (response: NextResponse): NextResponse => {
  for (const header of INFORMATION_LEAK_HEADERS) {
    response.headers.delete(header);
  }
  return response;
};

const createForbiddenResponse = (): NextResponse => {
  const response = new NextResponse("Forbidden", { status: 403 });
  return stripInformationLeakHeaders(response);
};

const hasCloudflareClientIp = (request: NextRequest): boolean => {
  const headerValue = request.headers.get(CLOUDFLARE_CLIENT_IP_HEADER);
  return typeof headerValue === "string" && headerValue.length > 0;
};

export function proxy(request: NextRequest) {
  if (!isCloudflareGuardDisabled() && !hasCloudflareClientIp(request)) {
    return createForbiddenResponse();
  }

  return stripInformationLeakHeaders(NextResponse.next());
}

export const config = {
  matcher: [
    { source: "/((?!api|_next/static|_next/image|favicon.ico).*)" },
  ],
};

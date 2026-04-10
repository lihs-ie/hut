import { NextRequest, NextResponse } from "next/server";
import { revalidateTag } from "next/cache";
import { timingSafeEqual } from "node:crypto";
import { z } from "zod";

const bodySchema = z.object({
  tags: z.array(z.string()),
});

const verifySecret = (request: NextRequest): boolean => {
  const secret = process.env.REVALIDATION_SECRET;
  if (!secret) {
    return false;
  }
  const providedSecret = request.headers.get("x-revalidation-secret");
  if (!providedSecret) {
    return false;
  }
  const encoder = new TextEncoder();
  const expected = encoder.encode(secret);
  const provided = encoder.encode(providedSecret);
  if (expected.byteLength !== provided.byteLength) {
    return false;
  }
  return timingSafeEqual(expected, provided);
};

export async function POST(request: NextRequest): Promise<NextResponse> {
  if (!verifySecret(request)) {
    return NextResponse.json({ message: "Unauthorized" }, { status: 401 });
  }

  let body: unknown;
  try {
    body = await request.json();
  } catch {
    return NextResponse.json({ message: "Bad Request" }, { status: 400 });
  }

  const parseResult = bodySchema.safeParse(body);

  if (!parseResult.success) {
    return NextResponse.json({ message: "Bad Request" }, { status: 400 });
  }

  for (const tag of parseResult.data.tags) {
    revalidateTag(tag);
  }

  return NextResponse.json({ revalidated: true });
}

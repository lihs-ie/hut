import { timingSafeEqual } from "node:crypto";
import { NextResponse } from "next/server";
import { z } from "zod";
import { revalidateTag } from "next/cache";

const bodySchema = z.object({
  tags: z.array(z.string()),
});

const unauthorizedResponse = (): NextResponse =>
  NextResponse.json({ error: "Unauthorized" }, { status: 401 });

const badRequestResponse = (): NextResponse =>
  NextResponse.json({ error: "Bad Request" }, { status: 400 });

export async function POST(request: Request): Promise<NextResponse> {
  const secret = process.env.REVALIDATION_SECRET;
  const providedSecret = request.headers.get("x-revalidation-secret");

  if (!secret || !providedSecret) {
    return unauthorizedResponse();
  }

  const secretBuffer = Buffer.from(secret, "utf-8");
  const providedBuffer = Buffer.from(providedSecret, "utf-8");

  if (secretBuffer.length !== providedBuffer.length) {
    return unauthorizedResponse();
  }

  if (!timingSafeEqual(secretBuffer, providedBuffer)) {
    return unauthorizedResponse();
  }

  const rawBody: unknown = await request.json().catch(() => undefined);

  if (rawBody === undefined) {
    return badRequestResponse();
  }

  const parseResult = bodySchema.safeParse(rawBody);

  if (!parseResult.success) {
    return badRequestResponse();
  }

  const { tags } = parseResult.data;

  for (const tag of tags) {
    revalidateTag(tag, {});
  }

  return NextResponse.json({ revalidated: true, tags });
}

import { NextRequest, NextResponse } from "next/server";
import { revalidateTag } from "next/cache";
import { timingSafeEqual } from "node:crypto";
import { z } from "zod";
import { ok, err, type Result, fromPromise } from "@shared/aspects/result";

const bodySchema = z.object({
  tags: z.array(z.string()),
});

type RouteError = {
  message: string;
  status: number;
};

const unauthorized = (): RouteError => ({ message: "Unauthorized", status: 401 });
const badRequest = (): RouteError => ({ message: "Bad Request", status: 400 });

const verifySecret = (
  request: NextRequest,
): Result<NextRequest, RouteError> => {
  const secret = process.env.REVALIDATION_SECRET;
  if (!secret) {
    return err(unauthorized());
  }
  const providedSecret = request.headers.get("x-revalidation-secret");
  if (!providedSecret) {
    return err(unauthorized());
  }
  const encoder = new TextEncoder();
  const expected = encoder.encode(secret);
  const provided = encoder.encode(providedSecret);
  if (expected.byteLength !== provided.byteLength) {
    return err(unauthorized());
  }
  if (!timingSafeEqual(expected, provided)) {
    return err(unauthorized());
  }
  return ok(request);
};

const parseBody = (request: NextRequest) =>
  fromPromise(
    request.json() as Promise<unknown>,
    badRequest,
  ).andThen((body) => {
    const parsed = bodySchema.safeParse(body);
    if (!parsed.success) {
      return err<{ tags: string[] }, RouteError>(badRequest());
    }
    return ok<{ tags: string[] }, RouteError>(parsed.data);
  });

export async function POST(request: NextRequest): Promise<NextResponse> {
  return verifySecret(request)
    .toAsync()
    .andThen(parseBody)
    .tap((data) => {
      for (const tag of data.tags) {
        revalidateTag(tag, {});
      }
    })
    .match<NextResponse>({
      ok: () => NextResponse.json({ revalidated: true }),
      err: (error) =>
        NextResponse.json({ message: error.message }, { status: error.status }),
    });
}

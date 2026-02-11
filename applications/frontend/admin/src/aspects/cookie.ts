import { cookies } from "next/headers";

export type SessionCookieOptions = {
  secure?: boolean;
  maxAge?: number;
};

export const setSessionCookie = async (
  identifier: string,
  options: SessionCookieOptions = {},
): Promise<void> => {
  const store = await cookies();

  store.set("admin_session", identifier, {
    httpOnly: true,
    secure: options.secure ?? process.env.NODE_ENV === "production",
    sameSite: "strict",
    maxAge: options.maxAge ?? 60 * 60 * 24,
    path: "/",
  });
};

export const getSessionCookie = async (): Promise<string | null> => {
  const store = await cookies();
  const cookie = store.get("admin_session");

  return cookie?.value ?? null;
};

export const clearSessionCookie = async (): Promise<void> => {
  const store = await cookies();

  store.delete("admin_session");
};

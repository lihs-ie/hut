"use server";

import { revalidatePath } from "next/cache";
import { cookies } from "next/headers";
import { Theme } from "@shared/domains/common/theme";

export async function toggleTheme() {
  const cookieStore = await cookies();
  const current = cookieStore.get("theme")?.value ?? Theme.LIGHT;

  const next = current === Theme.LIGHT ? Theme.DARK : Theme.LIGHT;

  cookieStore.set("theme", next, {
    path: "/",
    sameSite: "lax",
    httpOnly: true,
    secure: process.env.NODE_ENV === "production",
  });

  revalidatePath("/", "layout");
}

const isValidTheme = (value: string): value is Theme =>
  value === Theme.LIGHT || value === Theme.DARK;

export async function currentTheme(): Promise<Theme> {
  const cookieStore = await cookies();
  const value = cookieStore.get("theme")?.value;

  if (value !== undefined && isValidTheme(value)) {
    return value;
  }

  return Theme.LIGHT;
}

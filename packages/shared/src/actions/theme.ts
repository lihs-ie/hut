"use server";

import { revalidatePath } from "next/cache";
import { cookies } from "next/headers";
import { Theme } from "@shared/domains/common/theme";

export async function toggleTheme() {
  const cookieStore = await cookies();
  const current = cookieStore.get("theme")?.value ?? Theme.LIGHT;

  const next = current === Theme.LIGHT ? Theme.DARK : Theme.LIGHT;

  cookieStore.set("theme", next, { path: "/", sameSite: "lax" });

  revalidatePath("/", "layout");
}

export async function currentTheme(): Promise<Theme> {
  const cookieStore = await cookies();
  const current = cookieStore.get("theme")?.value as Theme | undefined;

  return current ?? Theme.LIGHT;
}

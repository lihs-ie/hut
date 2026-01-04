"use server";

import { Article, articleSchema } from "@/domains/articles";

export async function Persist(formData: FormData) {
  console.log("Persist article", formData);

  return;
}

export async function Terminate(identifier: string) {
  console.log("Terminate article", identifier);

  return;
}

export async function Find(identifier: string): Promise<Article> {
  console.log("Find article", identifier);

  throw new Error("Not implemented");
}

export async function Search(params: {
  freeWord?: string;
  tagIdentifiers?: string[];
}) {
  console.log("Search articles", params);

  return [];
}

import { type z } from "zod";

interface ZodFormData<T> {
  parse: (data: FormData) => T;
  safeParse: (data: FormData) => z.ZodSafeParseResult<T>;
}

export const zodFormData = <T>(schema: z.ZodType<T>): ZodFormData<T> => {
  const parse = (data: FormData): T => {
    const obj: Record<string, unknown> = {};

    data.forEach((value, key) => {
      obj[key] = value;
    });

    return schema.parse(obj);
  };

  const safeParse = (data: FormData): z.ZodSafeParseResult<T> => {
    const obj: Record<string, unknown> = {};

    data.forEach((value, key) => {
      obj[key] = value;
    });

    return schema.safeParse(obj);
  };

  return { parse, safeParse };
};

export const parseAsArray = (values: string): string[] => {
  return values.split(",").map((value) => value.trim());
};

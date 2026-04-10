import { z } from "zod";

export const revalidation = z
  .object({
    readerEndpoint: z.url(),
    secret: z.string().min(1),
  })
  .parse({
    readerEndpoint: process.env.READER_ENDPOINT,
    secret: process.env.REVALIDATION_SECRET,
  });

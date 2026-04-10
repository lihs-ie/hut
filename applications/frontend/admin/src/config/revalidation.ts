import { z } from "zod";

const revalidationSchema = z.object({
  readerEndpoint: z.url(),
  secret: z.string().min(1),
});

type Revalidation = z.infer<typeof revalidationSchema>;

const parsed = revalidationSchema.safeParse({
  readerEndpoint: process.env.READER_ENDPOINT,
  secret: process.env.REVALIDATION_SECRET,
});

export const revalidation: Revalidation | undefined = parsed.success
  ? parsed.data
  : undefined;

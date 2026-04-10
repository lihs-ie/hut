export const revalidation = {
  readerUrl: process.env.READER_URL,
  secret: process.env.REVALIDATION_SECRET,
} as const;

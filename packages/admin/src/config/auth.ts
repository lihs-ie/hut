import z from "zod";

const oidcSchema = z.object({
  allowedEmails: z.array(z.email()),
});

export const oidc = oidcSchema.parse({
  allowedEmails: JSON.parse(process.env.OIDC_ALLOWED_EMAILS || "[]"),
});

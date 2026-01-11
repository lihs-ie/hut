import z from "zod";
import { OidcAuthConfig } from "@/aspects/auth/oidc";

const oidcSchema = z.object({
  allowedEmails: z.array(z.string().email()),
});

export const oidcServer: OidcAuthConfig = oidcSchema.parse({
  allowedEmails: JSON.parse(process.env.OIDC_ALLOWED_EMAILS || "[]"),
});

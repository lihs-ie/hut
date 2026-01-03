import {
  DomainError,
  ValidationError,
  validationErrors,
} from "@/aspects/error";
import { AsyncResult, err, ok, Result } from "@/aspects/result";
import z from "zod";

export const adminIdentifierSchema = z.ulid().brand("AdminIdentifier");

export type AdminIdentifier = z.infer<typeof adminIdentifierSchema>;

export const adminNameSchema = z.string().min(1).max(20).brand("AdminName");

export type AdminName = z.infer<typeof adminNameSchema>;

export const mailAddressSchema = z
  .email({ message: "Invalid email address format" })
  .max(100, { message: "Email address must be at most 100 characters long" })
  .brand("MailAddress");

export type MailAddress = z.infer<typeof mailAddressSchema>;

export const career = z.string().max(256).brand("Career");

export type Career = z.infer<typeof career>;

export const adminSchema = z
  .object({
    identifier: adminIdentifierSchema,
    name: adminNameSchema,
    email: mailAddressSchema,
    careers: z.array(career),
    image: z.url().nullable(),
  })
  .brand("Admin");

export type Admin = z.infer<typeof adminSchema>;

export type UnvalidatedAdmin = {
  identifier: string;
  name: string;
  email: string;
  careers: string[];
  image: string | null;
};

export const validateAdmin = (
  candidate: UnvalidatedAdmin
): Result<Admin, ValidationError[]> => {
  const errors = validationErrors(adminSchema, candidate);

  if (errors.length > 0) {
    return err(errors);
  }

  return ok(adminSchema.parse(candidate));
};

export type AdminError = ValidationError | DomainError<"Admin">;

export interface AdminRepository {
  persist: (admin: Admin) => AsyncResult<void, AdminError>;
  find: (identifier: AdminIdentifier) => AsyncResult<Admin, AdminError>;
}

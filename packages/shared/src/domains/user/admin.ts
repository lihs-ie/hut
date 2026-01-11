import {
  AggregateNotFoundError,
  DomainError,
  UnexpectedError,
  validate,
  ValidationError,
} from "@shared/aspects/error";
import { AsyncResult, Result } from "@shared/aspects/result";
import z from "zod";
import { externalServiceSchema } from "../common/service";
import {
  technologyCategorySchema,
  technologyStackSchema,
} from "../common/tech";
import { Image, imageSchema } from "../common/image";

export const adminIdentifierSchema = z.ulid().brand("AdminIdentifier");

export type AdminIdentifier = z.infer<typeof adminIdentifierSchema>;

export const adminNameSchema = z.string().min(1).max(20).brand("AdminName");

export type AdminName = z.infer<typeof adminNameSchema>;

export const mailAddressSchema = z
  .email({ message: "Invalid email address format" })
  .max(100, { message: "Email address must be at most 100 characters long" })
  .brand("MailAddress");

export type MailAddress = z.infer<typeof mailAddressSchema>;

export const Role = {
  FULL_STACK: "Full Stack Developer",
} as const;

export const roleSchema = z.enum(Role).brand("Role");

export type Role = z.infer<typeof roleSchema>;

export const periodSchema = z
  .object({
    from: z.date(),
    to: z.date().nullable(),
  })
  .brand("Period");

export type Period = z.infer<typeof periodSchema>;

export const career = z.object({
  company: z.string().min(1).max(50),
  period: periodSchema,
  role: roleSchema,
  description: z.string().max(500),
});

export type Career = z.infer<typeof career>;

export const profileSchema = z
  .object({
    avatar: imageSchema,
    name: adminNameSchema,
    email: mailAddressSchema,
    bio: z.string().max(300),
    careers: z.array(career),
    externalServices: z.array(externalServiceSchema),
    techStacks: z.map(technologyCategorySchema, z.array(technologyStackSchema)),
  })
  .brand("Profile");

export type Profile = z.infer<typeof profileSchema>;

export type UnvalidatedCareer = {
  company: string;
  period: {
    from: Date;
    to: Date | null;
  };
  role: string;
  description: string;
};

export type UnvalidatedProfile = {
  avatar: string;
  name: string;
  email: string;
  careers: UnvalidatedCareer[];
  bio: string;
  externalServices: Array<{
    type: string;
    user: string;
  }>;
  techStacks: Map<string, { kind: string; from: Date; continue: boolean }[]>;
};

export const validateProfile = (
  candidate: UnvalidatedProfile,
): Result<Profile, ValidationError[]> => validate(profileSchema, candidate);

export const adminSchema = z
  .object({
    identifier: adminIdentifierSchema,
    profile: profileSchema,
  })
  .brand("Admin");

export type Admin = z.infer<typeof adminSchema>;

export type UnvalidatedAdmin = {
  identifier: string;
  profile: UnvalidatedProfile;
};

export const validateAdmin = (
  candidate: UnvalidatedAdmin,
): Result<Admin, ValidationError[]> => validate(adminSchema, candidate);

export const updateProfile = (
  admin: Admin,
  unvalidated: UnvalidatedProfile,
): Result<Admin, ValidationError[]> =>
  validateAdmin({ identifier: admin.identifier, profile: unvalidated });

export type AdminError =
  | ValidationError
  | DomainError<"Admin">
  | UnexpectedError;

export interface AdminRepository {
  persist: (
    admin: Admin,
  ) => AsyncResult<void, AggregateNotFoundError<"Admin"> | UnexpectedError>;
  find: () => AsyncResult<
    Admin,
    UnexpectedError | AggregateNotFoundError<"Admin">
  >;
}

export interface UploadAvatar {
  (avatar: string): AsyncResult<Image, UnexpectedError>;
}

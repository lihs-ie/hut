import {
  Admin,
  AdminIdentifier,
  adminIdentifierSchema,
  AdminName,
  adminNameSchema,
  AdminRepository,
  adminSchema,
  Career,
  career,
  MailAddress,
  mailAddressSchema,
  Period,
  periodSchema,
  Profile,
  profileSchema,
  Role,
} from "@shared/domains/user";

import { ulid } from "ulid";
import { DateMold } from "../common/date";
import { ImmutableMap } from "@shared/domains/common";
import { fromPromise } from "@shared/aspects/result";
import {
  AggregateNotFoundError,
  aggregateNotFoundError,
  UnexpectedError,
} from "@shared/aspects/error";
import { ExternalService } from "@shared/domains/common/service";
import { ExternalServiceMold } from "../common/service";
import { TechnologyCategory, TechnologyStack } from "@shared/domains/common/tech";
import {
  Characters,
  EnumMold,
  Forger,
  Mold,
  StringMold,
} from "@lihs-ie/forger-ts";
import { Image } from "@shared/domains/common/image";
import { ImageMold } from "../common/image";

export type AdminIdentifierProperties = {
  value: string;
};

export const AdminIdentifierMold = Mold<
  AdminIdentifier,
  AdminIdentifierProperties
>({
  pour: (properties) => adminIdentifierSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ?? ulid(Forger(DateMold).forgeWithSeed(seed).getTime()),
  }),
});

export type AdminNameProperties = {
  value: string;
};

export const AdminNameMold = Mold<AdminName, AdminNameProperties>({
  pour: (properties) => adminNameSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ??
      Forger(StringMold(1, 20, Characters.ALPHA)).forgeWithSeed(seed),
  }),
});

export type MailAddressProperties = {
  value: string;
};

export const MailAddressMold = Mold<MailAddress, MailAddressProperties>({
  pour: (properties) => mailAddressSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ??
      `${Forger(StringMold(5, 20, Characters.SLUG)).forgeWithSeed(
        seed,
      )}@example.com`,
  }),
});

export const RoleMold = EnumMold(Role);

export type PeriodProperties = {
  from: Date;
  to: Date | null;
};

export const PeriodMold = Mold<Period, PeriodProperties>({
  pour: (properties) => periodSchema.parse(properties),
  prepare: (overrides, seed) => ({
    from: overrides.from ?? Forger(DateMold).forgeWithSeed(seed),
    to: overrides.to ?? null,
  }),
});

export type CareerProperties = {
  company: string;
  period: Period;
  role: Role;
  description: string;
};

export const CareerMold = Mold<Career, CareerProperties>({
  pour: (properties) => career.parse(properties),
  prepare: (overrides, seed) => ({
    company: overrides.company ?? Forger(StringMold(1, 50)).forgeWithSeed(seed),
    period: overrides.period ?? Forger(PeriodMold).forgeWithSeed(seed),
    role: overrides.role ?? (Forger(RoleMold).forgeWithSeed(seed) as Role),
    description:
      overrides.description ?? Forger(StringMold(1, 500)).forgeWithSeed(seed),
  }),
});

export type ProfileProperties = {
  avatar: Image;
  name: AdminName;
  email: MailAddress;
  careers: Career[];
  bio: string;
  externalServices: ExternalService[];
  techStacks: Map<TechnologyCategory, TechnologyStack[]>;
};

export const ProfileMold = Mold<Profile, ProfileProperties>({
  pour: (properties) =>
    profileSchema.parse({
      avatar: properties.avatar,
      name: properties.name,
      email: properties.email,
      careers: properties.careers,
      bio: properties.bio,
      externalServices: properties.externalServices,
      techStacks: properties.techStacks,
    }),
  prepare: (overrides, seed) => ({
    avatar: overrides.avatar ?? Forger(ImageMold).forgeWithSeed(seed),
    name: overrides.name ?? Forger(AdminNameMold).forgeWithSeed(seed),
    email: overrides.email ?? Forger(MailAddressMold).forgeWithSeed(seed),
    careers:
      overrides.careers ?? Forger(CareerMold).forgeMultiWithSeed(3, seed),
    bio: overrides.bio ?? Forger(StringMold(1, 300)).forgeWithSeed(seed),
    externalServices:
      overrides.externalServices ??
      Forger(ExternalServiceMold).forgeMultiWithSeed(2, seed),
    techStacks:
      overrides.techStacks ?? new Map<TechnologyCategory, TechnologyStack[]>(),
  }),
});

export const ProfileFactory = ProfileMold;

export type AdminProperties = {
  identifier: AdminIdentifier;
  profile: Profile;
};

export const AdminMold = Mold<Admin, AdminProperties>({
  pour: (properties) =>
    adminSchema.parse({
      identifier: properties.identifier,
      profile: properties.profile,
    }),
  prepare: (overrides, seed) => ({
    identifier:
      overrides.identifier ?? Forger(AdminIdentifierMold).forgeWithSeed(seed),
    profile: overrides.profile ?? Forger(ProfileMold).forgeWithSeed(seed),
  }),
});

export type AdminRepositoryMoldProperties = {
  instancies: Admin[];
  onPersist?: (admin: Admin) => void;
};

export const AdminRepositoryMold = Mold<
  AdminRepository,
  AdminRepositoryMoldProperties
>({
  pour: (properties) => {
    let instancies = ImmutableMap.fromArray(
      properties.instancies.map((admin): [AdminIdentifier, Admin] => [
        admin.identifier,
        admin,
      ]),
    );

    const persist: AdminRepository["persist"] = (admin: Admin) =>
      fromPromise<void, AggregateNotFoundError<"Admin"> | UnexpectedError>(
        new Promise((resolve) => {
          instancies = instancies.add(admin.identifier, admin);

          properties.onPersist?.(admin);

          resolve();
        }),
        (error) => aggregateNotFoundError("Admin", String(error)),
      );

    const find: AdminRepository["find"] = () =>
      fromPromise<Admin, AggregateNotFoundError<"Admin"> | UnexpectedError>(
        new Promise((resolve, reject) => {
          const values = instancies.values();
          const firstAdmin = values.length > 0 ? values[0] : undefined;
          if (firstAdmin) {
            resolve(firstAdmin);
          } else {
            reject(aggregateNotFoundError("Admin", "Admin not found."));
          }
        }),
        (error) => error as AggregateNotFoundError<"Admin"> | UnexpectedError,
      );

    return {
      find,
      persist,
    };
  },
  prepare: (overrides, seed) => ({
    instancies:
      overrides.instancies ?? Forger(AdminMold).forgeMultiWithSeed(5, seed),
    onPersist: overrides.onPersist,
  }),
});

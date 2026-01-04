import {
  Admin,
  AdminError,
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
} from "@/domains/user";
import { Builder, Characters, Factory, StringFactory } from "../../builder";
import { ulid } from "ulid";
import { DateFactory } from "../common/date";
import { ImmutableList, ImmutableMap } from "@/domains/common";
import { fromPromise } from "@/aspects/result";
import { validationError } from "@/aspects/error";

export type AdminIdentifierProperties = {
  value: string;
};

export const AdminIdentifierFactory = Factory<
  AdminIdentifier,
  AdminIdentifierProperties
>({
  instantiate: (properties) => adminIdentifierSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ?? ulid(Builder(DateFactory).buildWith(seed).getTime()),
  }),
  retrieve: (instance) => ({
    value: instance,
  }),
});

export type AdminNameProperties = {
  value: string;
};

export const AdminNameFactory = Factory<AdminName, AdminNameProperties>({
  instantiate: (properties) => adminNameSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ??
      Builder(StringFactory(1, 20, Characters.ALPHA)).buildWith(seed),
  }),
  retrieve: (instance) => ({
    value: instance,
  }),
});

export type MailAddressProperties = {
  value: string;
};

export const MailAddressFactory = Factory<MailAddress, MailAddressProperties>({
  instantiate: (properties) => mailAddressSchema.parse(properties.value),
  prepare: (overrides, seed) => ({
    value:
      overrides.value ??
      `${Builder(StringFactory(5, 20, Characters.SLUG)).buildWith(
        seed
      )}@example.com`,
  }),
  retrieve: (instance) => ({
    value: instance,
  }),
});

export type CareerProperties = {
  value: string;
};

export const CareerFactory = Factory<Career, CareerProperties>({
  instantiate: (properties) => career.parse(properties.value),
  prepare: (overrides, seed) => ({
    value: overrides.value ?? Builder(StringFactory(1, 256)).buildWith(seed),
  }),
  retrieve: (instance) => ({
    value: instance,
  }),
});

export type AdminProperties = {
  identifier: AdminIdentifier;
  name: AdminName;
  email: MailAddress;
  careers: Career[];
  image: string | null;
};

export const AdminFactory = Factory<Admin, AdminProperties>({
  instantiate: (properties) =>
    adminSchema.parse({
      identifier: properties.identifier,
      name: properties.name,
      email: properties.email,
      careers: properties.careers,
      image: properties.image,
    }),
  prepare: (overrides, seed) => ({
    identifier:
      overrides.identifier ?? Builder(AdminIdentifierFactory).buildWith(seed),
    name: overrides.name ?? Builder(AdminNameFactory).buildWith(seed),
    email: overrides.email ?? Builder(MailAddressFactory).buildWith(seed),
    careers:
      overrides.careers ??
      Builder(CareerFactory).buildListWith(3, seed).toArray(),
    image: overrides.image ?? `https://picsum.photos/id/${seed}/200/200`,
  }),
  retrieve: (instance) => ({
    identifier: instance.identifier,
    name: instance.name,
    email: instance.email,
    careers: instance.careers,
    image: instance.image,
  }),
});

export type AdminRepositoryFactoryProperties = {
  instancies: ImmutableList<Admin>;
  onPersist?: (admin: Admin) => void;
};

export const AdminRepositoryFactory = Factory<
  AdminRepository,
  AdminRepositoryFactoryProperties
>({
  instantiate: (properties) => {
    let instancies = ImmutableMap.fromArray(
      properties.instancies
        .map((admin): [AdminIdentifier, Admin] => [admin.identifier, admin])
        .toArray()
    );

    const persist: AdminRepository["persist"] = (admin: Admin) =>
      fromPromise<void, AdminError>(
        new Promise((resolve) => {
          instancies = instancies.add(admin.identifier, admin);

          properties.onPersist?.(admin);

          resolve();
        }),
        (error) => validationError("admin", String(error))
      );

    const find: AdminRepository["find"] = (identifier) =>
      fromPromise<Admin, AdminError>(
        new Promise((resolve, reject) => {
          instancies.get(identifier).ifPresentOrElse(
            (admin) => resolve(admin),
            () =>
              reject(
                validationError(
                  "identifier",
                  `Admin with identifier ${identifier} not found.`
                )
              )
          );
        }),
        (error) => error as AdminError
      );

    return {
      find,
      persist,
    };
  },
  prepare: (overrides, seed) => ({
    instancies:
      overrides.instancies ?? Builder(AdminFactory).buildListWith(5, seed),
    onPersist: overrides.onPersist,
  }),
  retrieve: () => {
    throw new Error("Repository cannot be retrieved.");
  },
});

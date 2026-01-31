import {
  Firestore,
  QueryDocumentSnapshot,
  SnapshotOptions,
  Timestamp,
} from "firebase/firestore";
import {
  createVersion,
  FirestoreOperations,
  mapFirestoreError,
  Version,
} from "./common";
import {
  Admin,
  AdminIdentifier,
  AdminRepository,
  validateAdmin,
} from "@shared/domains/user";
import { fromPromise } from "@shared/aspects/result";
import { aggregateNotFoundError } from "@shared/aspects/error";

type PersistedCareer = {
  company: string;
  period: {
    from: Timestamp;
    to: Timestamp | null;
  };
  role: string;
  description: string;
};

type PersistedExternalService = {
  type: string;
  user: string;
};

type PersistedTechnologyStack = {
  tag: string;
  from: Timestamp;
  continue: boolean;
  type: string;
};

type PersistedAdmin = {
  identifier: string;
  profile: {
    avatar: string;
    name: string;
    email: string;
    bio: string;
    careers: PersistedCareer[];
    externalServices: PersistedExternalService[];
    techStacks: Record<string, PersistedTechnologyStack[]>;
  };
  version: number;
};

const ADMIN_DOCUMENT_ID = "admin";

export const FirebaseAdminRepository = (
  firestore: Firestore,
  operations: FirestoreOperations,
): AdminRepository => {
  const mapError = mapFirestoreError("Admin");
  const versions: Map<AdminIdentifier, Version> = new Map();

  const collection = operations
    .collection(firestore, "admin")
    .withConverter<Admin, PersistedAdmin>({
      toFirestore: (admin: Admin): PersistedAdmin => {
        const currentVersion = versions.get(admin.identifier);

        const techStacksObject: Record<string, PersistedTechnologyStack[]> = {};
        admin.profile.techStacks.forEach((stacks, category) => {
          techStacksObject[category] = stacks.map((stack) => ({
            tag: stack.tag,
            from: Timestamp.fromDate(stack.from),
            continue: stack.continue,
            type: stack.type,
          }));
        });

        return {
          identifier: admin.identifier,
          profile: {
            avatar: admin.profile.avatar,
            name: admin.profile.name,
            email: admin.profile.email,
            bio: admin.profile.bio,
            careers: admin.profile.careers.map((career) => ({
              company: career.company,
              period: {
                from: Timestamp.fromDate(career.period.from),
                to: career.period.to
                  ? Timestamp.fromDate(career.period.to)
                  : null,
              },
              role: career.role,
              description: career.description,
            })),
            externalServices: admin.profile.externalServices.map((service) => ({
              type: service.type,
              user: service.user,
            })),
            techStacks: techStacksObject,
          },
          version: currentVersion?.increment().value ?? 1,
        };
      },
      fromFirestore: (
        snapshot: QueryDocumentSnapshot<PersistedAdmin>,
        options?: SnapshotOptions,
      ): Admin => {
        const data = snapshot.data(options);

        const techStacksMap = new Map<
          string,
          { tag: string; from: Date; continue: boolean; type: string }[]
        >();
        Object.entries(data.profile.techStacks).forEach(
          ([category, stacks]) => {
            techStacksMap.set(
              category,
              stacks.map((stack) => ({
                tag: stack.tag,
                from: stack.from.toDate(),
                continue: stack.continue,
                type: stack.type,
              })),
            );
          },
        );

        const admin = validateAdmin({
          identifier: data.identifier,
          profile: {
            avatar: data.profile.avatar,
            name: data.profile.name,
            email: data.profile.email,
            bio: data.profile.bio,
            careers: data.profile.careers.map((career) => ({
              company: career.company,
              period: {
                from: career.period.from.toDate(),
                to: career.period.to ? career.period.to.toDate() : null,
              },
              role: career.role,
              description: career.description,
            })),
            externalServices: data.profile.externalServices.map((service) => ({
              type: service.type,
              user: service.user,
            })),
            techStacks: techStacksMap,
          },
        }).unwrap();

        const version = createVersion(data.version);

        versions.set(admin.identifier, version);

        return admin;
      },
    });

  const persist: AdminRepository["persist"] = (admin: Admin) => {
    return fromPromise(
      operations.runTransaction(firestore, async (transaction) => {
        const document = operations.doc(collection, ADMIN_DOCUMENT_ID);
        const currentVersion = versions.get(admin.identifier);

        if (currentVersion && currentVersion.value > 0) {
          transaction.set(document, admin, { merge: true });
          versions.set(admin.identifier, currentVersion.increment());
        } else {
          transaction.set(document, admin);
          versions.set(admin.identifier, createVersion(1));
        }
      }),
      mapError,
    );
  };

  const find: AdminRepository["find"] = () => {
    return fromPromise(
      (async () => {
        const document = operations.doc(collection, ADMIN_DOCUMENT_ID);
        const snapshot = await operations.getDoc(document);

        if (!snapshot.exists()) {
          throw aggregateNotFoundError("Admin", "Admin not found.");
        }

        return snapshot.data();
      })(),
      mapError,
    );
  };

  return {
    persist,
    find,
  };
};

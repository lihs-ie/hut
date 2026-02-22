import { Firestore, Timestamp } from "firebase-admin/firestore";
import {
  Admin,
  AdminIdentifier,
  AdminRepository,
  validateAdmin,
} from "@shared/domains/user";
import { fromPromise } from "@shared/aspects/result";
import {
  aggregateNotFoundError,
  unexpectedError,
} from "@shared/aspects/error";

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

const mapError = (error: unknown) => {
  const message = error instanceof Error ? error.message : String(error);
  return unexpectedError(message, error);
};

export const FirebaseAdminSdkAdminRepository = (
  firestore: Firestore,
): AdminRepository => {
  const versions: Map<AdminIdentifier, number> = new Map();

  const toFirestore = (admin: Admin): PersistedAdmin => {
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
      version: currentVersion ? currentVersion + 1 : 1,
    };
  };

  const fromFirestore = (data: PersistedAdmin): Admin => {
    const techStacksMap = new Map<
      string,
      { tag: string; from: Date; continue: boolean; type: string }[]
    >();
    Object.entries(data.profile.techStacks).forEach(([category, stacks]) => {
      techStacksMap.set(
        category,
        stacks.map((stack) => ({
          tag: stack.tag,
          from: stack.from.toDate(),
          continue: stack.continue,
          type: stack.type,
        })),
      );
    });

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

    versions.set(admin.identifier, data.version);

    return admin;
  };

  const persist: AdminRepository["persist"] = (admin: Admin) => {
    return fromPromise(
      firestore.runTransaction(async (transaction) => {
        const document = firestore.collection("admin").doc(ADMIN_DOCUMENT_ID);
        const data = toFirestore(admin);

        transaction.set(document, data, { merge: true });
        versions.set(admin.identifier, data.version);
      }),
      mapError,
    );
  };

  const find: AdminRepository["find"] = () => {
    return fromPromise(
      (async () => {
        const snapshot = await firestore
          .collection("admin")
          .doc(ADMIN_DOCUMENT_ID)
          .get();

        if (!snapshot.exists) {
          throw aggregateNotFoundError("Admin", "Admin not found.");
        }

        return fromFirestore(snapshot.data() as PersistedAdmin);
      })(),
      mapError,
    );
  };

  return {
    persist,
    find,
  };
};

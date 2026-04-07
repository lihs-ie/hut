import { FirebaseOIDCServerAdaptor } from "@/acl/oidc/server";
import { FirebaseAdminProvider } from "@shared/providers/infrastructure/firebase-admin";
import { oidcServer } from "@/config/auth/server";

export const OIDCServerProvider = FirebaseOIDCServerAdaptor(
  FirebaseAdminProvider.auth.instance,
  oidcServer,
);

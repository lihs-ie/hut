import { FirebaseOIDCServerAdaptor } from "@/acl/oidc/server";
import { FirebaseAdminProvider } from "@/providers/auth/admin";
import { oidcServer } from "@/config/auth/server";

export const OIDCServerProvider = FirebaseOIDCServerAdaptor(
  FirebaseAdminProvider.auth.instance,
  oidcServer,
);

import { FirebaseOIDCClientAdaptor } from "@/acl/oidc/client";
import { FirebaseAuthProvider } from "@/providers/auth/firebase";
import { oidcClient } from "@/config/auth/client";

export const OIDCClientProvider = FirebaseOIDCClientAdaptor(
  FirebaseAuthProvider.auth.instance,
  oidcClient,
);

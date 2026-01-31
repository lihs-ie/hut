import {
  createOIDCAuth,
  OidcAuthConfig,
  OidcAuthError,
  OidcUser,
} from "@/aspects/auth/oidc";
import { AsyncResult } from "@shared/aspects/result";
import { type Auth, type Unsubscribe } from "firebase/auth";

export interface OIDCClientAdaptor {
  startRedirect: () => AsyncResult<void, OidcAuthError>;
  getRedirectResult: () => AsyncResult<OidcUser | null, OidcAuthError>;
  getIdToken: () => AsyncResult<string, OidcAuthError>;
  signOut: () => AsyncResult<void, OidcAuthError>;
  getCurrentUser: () => AsyncResult<OidcUser | null, OidcAuthError>;
  onAuthStateChanged: (
    handler: (user: OidcUser | null) => void,
  ) => AsyncResult<Unsubscribe, OidcAuthError>;
}

export const FirebaseOIDCClientAdaptor = (
  auth: Auth,
  config: OidcAuthConfig,
): OIDCClientAdaptor => {
  const oidcAuth = createOIDCAuth(auth, config);

  return {
    startRedirect: () => oidcAuth.startRedirect(),
    getRedirectResult: () => oidcAuth.getRedirectResult(),
    getIdToken: () => oidcAuth.getIdToken(),
    signOut: () => oidcAuth.signOut(),
    getCurrentUser: () => oidcAuth.getCurrentUser(),
    onAuthStateChanged: (handler) => oidcAuth.onAuthStateChanged(handler),
  };
};

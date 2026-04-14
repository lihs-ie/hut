import { SignJWT, importPKCS8 } from "jose";

const OAUTH_TOKEN_ENDPOINT = "https://oauth2.googleapis.com/token";
const FIRESTORE_SCOPE = "https://www.googleapis.com/auth/datastore";
const DEFAULT_EXPIRATION_SECONDS = 3600;
const EXPIRATION_BUFFER_SECONDS = 60;

export type AccessTokenCredentials = {
  clientEmail: string;
  privateKey: string;
};

export type AccessTokenProvider = {
  getAccessToken: () => Promise<string>;
};

type CachedToken = {
  token: string;
  expiresAt: number;
};

type TokenResponse = {
  accessToken: string;
  expiresIn: number;
};

const isTokenResponse = (value: unknown): value is TokenResponse => {
  if (value === null || typeof value !== "object") {
    return false;
  }
  const candidate = value as Record<string, unknown>;
  return (
    typeof candidate.accessToken === "string" &&
    typeof candidate.expiresIn === "number"
  );
};

const parseTokenResponse = (body: unknown): TokenResponse => {
  if (body === null || typeof body !== "object") {
    throw new Error("Invalid access token response body");
  }

  const record = body as Record<string, unknown>;
  const accessTokenValue = record.access_token;
  const expiresInValue = record.expires_in;

  if (typeof accessTokenValue !== "string") {
    throw new Error("Access token response is missing access_token");
  }

  const expiresIn =
    typeof expiresInValue === "number"
      ? expiresInValue
      : DEFAULT_EXPIRATION_SECONDS;

  const result = {
    accessToken: accessTokenValue,
    expiresIn,
  };

  if (!isTokenResponse(result)) {
    throw new Error("Access token response failed validation");
  }

  return result;
};

const createSignedAssertion = async (
  credentials: AccessTokenCredentials,
): Promise<string> => {
  const privateKey = await importPKCS8(credentials.privateKey, "RS256");
  const nowSeconds = Math.floor(Date.now() / 1000);

  return await new SignJWT({
    scope: FIRESTORE_SCOPE,
  })
    .setProtectedHeader({ alg: "RS256", typ: "JWT" })
    .setIssuer(credentials.clientEmail)
    .setSubject(credentials.clientEmail)
    .setAudience(OAUTH_TOKEN_ENDPOINT)
    .setIssuedAt(nowSeconds)
    .setExpirationTime(nowSeconds + DEFAULT_EXPIRATION_SECONDS)
    .sign(privateKey);
};

const requestAccessToken = async (
  credentials: AccessTokenCredentials,
): Promise<TokenResponse> => {
  const assertion = await createSignedAssertion(credentials);

  const body = new URLSearchParams({
    grant_type: "urn:ietf:params:oauth:grant-type:jwt-bearer",
    assertion,
  });

  const response = await fetch(OAUTH_TOKEN_ENDPOINT, {
    method: "POST",
    headers: { "Content-Type": "application/x-www-form-urlencoded" },
    body: body.toString(),
  });

  if (!response.ok) {
    throw new Error(
      `Failed to obtain access token (status=${response.status})`,
    );
  }

  const parsed = (await response.json()) as unknown;
  return parseTokenResponse(parsed);
};

export const createAccessTokenProvider = (
  credentials: AccessTokenCredentials,
): AccessTokenProvider => {
  let cached: CachedToken | null = null;

  const getAccessToken = async (): Promise<string> => {
    const nowSeconds = Math.floor(Date.now() / 1000);
    if (cached !== null && cached.expiresAt > nowSeconds + EXPIRATION_BUFFER_SECONDS) {
      return cached.token;
    }

    const tokenResponse = await requestAccessToken(credentials);
    cached = {
      token: tokenResponse.accessToken,
      expiresAt: nowSeconds + tokenResponse.expiresIn,
    };

    return cached.token;
  };

  return { getAccessToken };
};

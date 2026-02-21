/**
 * Types for the auth-client package.
 */

/** Token stored on disk at ~/.config/bln/auth-<clientId>.json */
export interface StoredToken {
  access_token: string;
  expires_at: number; // Unix seconds
  scope: string;
  client_id: string;
}

/** Response from POST /device (device authorization endpoint) */
export interface DeviceAuthResponse {
  device_code: string;
  user_code: string;
  verification_uri: string;
  verification_uri_complete?: string;  // RFC 8628 §3.3 — URI with user_code pre-filled
  expires_in: number;  // seconds
  interval: number;    // seconds to wait between polls
}

/** Response from POST /device/token while still pending */
export interface TokenPendingResponse {
  error: 'authorization_pending' | 'slow_down';
}

/** Response from POST /device/token when approved */
export interface TokenApprovedResponse {
  access_token: string;
  token_type: 'Bearer';
  expires_in: number;
  scope: string;
}

export type TokenResponse = TokenPendingResponse | TokenApprovedResponse;

/** Options accepted by getToken / createAuthHeaders */
export interface GetTokenOptions {
  clientId?: string;
  scopes?: string[];
}

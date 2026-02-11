export type DeviceType = "desktop" | "mobile" | "tablet";

// js-hoist-regexp: RegExp をモジュールレベルでホイストして再生成を防ぐ
const TABLET_PATTERN = /tablet|ipad/i;
const MOBILE_PATTERN = /mobile|android|iphone/i;

/**
 * User-Agent 文字列からデバイスタイプを判定する。
 * 軽量な正規表現ベースの判定。
 */
export const detectDeviceType = (userAgent: string | null): DeviceType => {
  if (!userAgent) return "desktop";
  // js-early-exit: タブレット判定を先に行い早期リターン
  if (TABLET_PATTERN.test(userAgent)) return "tablet";
  if (MOBILE_PATTERN.test(userAgent)) return "mobile";
  return "desktop";
};

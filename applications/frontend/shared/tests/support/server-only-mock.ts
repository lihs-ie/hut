// vitest で server-only を無効化するための空モジュール。
// server-only は Next.js runtime で client component からの import を弾くが、
// vitest 内では全てのモジュールが client 判定となってしまうため、
// resolve.alias でこの空モジュールに差し替える。
export {};

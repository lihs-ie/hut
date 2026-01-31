# Eventarc Integration Roadmap

Server ActionでドメインイベントをEventarcに発行する機能の実装ロードマップ。

## 設計方針

- **アプローチ**: Workflowへの依存注入パターン
- **イベント形式**: CloudEvents 1.0準拠
- **対象**: すべてのドメインイベント（Memo, Article, Series, Tag等）

## ファイル構成

```
domains/
  common/
    event.ts              # Event<P>型 + EventPublisherインターフェース + EventType

aspects/
  events/
    cloud-event.ts        # CloudEvents形式への変換

infrastructures/
  events/
    eventarc.ts           # Eventarc実装

providers/
  events/
    index.ts              # EventPublisherのProvider
```

---

## Phase 1: 基盤整備

### 1.1 ドメインイベントの型定義拡張

**ファイル**: `domains/common/event.ts`

- [ ] `EventType` 型を定義（`"memo.persisted"`, `"memo.terminated"` 等）
- [ ] `PublishableEvent<P>` 型を定義（`Event<P>` + `type: EventType`）
- [ ] `EventPublisher` インターフェースを定義

```typescript
export type EventType =
  | "memo.persisted"
  | "memo.terminated"
  | "article.persisted"
  | "article.terminated"
  | "series.persisted"
  | "series.terminated"
  | "tag.persisted"
  | "tag.terminated";

export type PublishableEvent<P> = Event<P> & {
  type: EventType;
};

export interface EventPublisher {
  publish<P>(event: PublishableEvent<P>): AsyncResult<void, UnexpectedError>;
}
```

### 1.2 各ドメインイベントの更新

**ファイル**: `domains/memo/event.ts`, `domains/articles/event.ts` 等

- [ ] イベント生成関数で `type` を含めるように更新

```typescript
export const createMemoPersistedEvent = (
  memo: MemoIdentifier
): PublishableEvent<{ memo: MemoIdentifier }> => ({
  type: "memo.persisted",
  identifier: ulid(),
  occurredAt: new Date(),
  payload: { memo },
});
```

---

## Phase 2: インフラ層実装

### 2.1 CloudEvents変換

**ファイル**: `aspects/events/cloud-event.ts`

- [ ] `CloudEvent<P>` 型を定義
- [ ] `toCloudEvent()` 変換関数を実装

```typescript
export type CloudEvent<P> = {
  specversion: "1.0";
  type: string;
  source: string;
  id: string;
  time: string;
  datacontenttype: "application/json";
  data: P;
};

export const toCloudEvent = <P>(
  event: PublishableEvent<P>,
  source: string
): CloudEvent<P> => ({ ... });
```

### 2.2 Eventarc Publisher実装

**ファイル**: `infrastructures/events/eventarc.ts`

- [ ] `createEventarcPublisher()` を実装
- [ ] firebase-adminの初期化確認

```typescript
export const createEventarcPublisher = (
  channel: EventarcChannel
): EventPublisher => ({
  publish<P>(event: PublishableEvent<P>): AsyncResult<void, UnexpectedError> {
    ...
  },
});
```

### 2.3 Provider作成

**ファイル**: `providers/events/index.ts`

- [ ] `EventPublisherProvider` を作成
- [ ] テスト用の `createMockEventPublisher()` を作成

---

## Phase 3: Workflow統合

### 3.1 Memo Workflowの更新

**ファイル**: `workflows/memo.ts`

- [ ] `createMemoPersistWorkflow` に `publish` 依存を追加
- [ ] `createMemoTerminateWorkflow` に `publish` 依存を追加

### 3.2 Memo Workflow Providerの更新

**ファイル**: `providers/workflows/memo.ts`

- [ ] `EventPublisherProvider` を注入

---

## Phase 4: 他ドメインへの展開

### 4.1 Article

- [ ] `domains/articles/event.ts` - イベント型更新
- [ ] `workflows/article.ts` - publish依存追加
- [ ] `providers/workflows/article.ts` - Provider更新

### 4.2 Series

- [ ] `domains/series/event.ts` - イベント型更新
- [ ] `workflows/series.ts` - publish依存追加
- [ ] `providers/workflows/series.ts` - Provider更新

### 4.3 Tag

- [ ] `domains/attributes/tag/event.ts` - イベント型作成
- [ ] `workflows/tag.ts` - publish依存追加
- [ ] `providers/workflows/tag.ts` - Provider更新

---

## Phase 5: Firebase Functions側の実装

### 5.1 イベントハンドラ作成

**ファイル**: `docker/firebase/functions/src/index.ts`

- [ ] `onMemoPersistedEvent` ハンドラ作成
- [ ] `onMemoTerminatedEvent` ハンドラ作成
- [ ] 他ドメインのハンドラ作成

```typescript
export const onMemoPersistedEvent = onCustomEventPublished(
  "com.hut.memo.persisted",
  (event) => {
    // 検索インデックス更新、通知送信等
  }
);
```

---

## Phase 6: テスト

### 6.1 ユニットテスト

- [ ] `toCloudEvent()` のテスト
- [ ] `createEventarcPublisher()` のテスト（モック使用）
- [ ] Workflow統合テスト（MockPublisher使用）

### 6.2 エミュレータでの統合テスト

- [ ] Eventarcエミュレータでのイベント発行確認
- [ ] Firebase Functionsでのイベント受信確認

---

## 補足: エラーハンドリング方針

イベント発行失敗時の挙動：

```typescript
// 現状: 発行失敗でワークフロー全体がエラー
.andThen((event) => publish(event).map(() => event))

// 代替案: 発行失敗はログのみ、永続化は成功扱い
.andThen((event) =>
  publish(event)
    .tapError((error) => logger.error("Event publish failed", { error }))
    .recover(() => event)
)
```

どちらを採用するかは実装時に決定。

---

## 依存関係

```
firebase-admin >= 13.6.0  # Eventarc対応
```

既にfunctions/package.jsonに含まれている。

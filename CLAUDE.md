# 開発スタイル

TDD で開発する（探索 → Red → Green → Refactoring）。
KPI やカバレッジ目標が与えられたら、達成するまで試行する。
不明瞭な指示は質問して明確にする。

# コード設計

- 関心の分離を保つ
- 状態とロジックを分離する
- 可読性と保守性を重視する
- 静的検査可能なルールはプロンプトではなく、その環境の linterで記述する

## 実装ルール

実装にあたり以下をルールとします

- コンポーネント内のルートの className は常に container
  にする
- className は xxxYYY は禁止する。xxx と一つの単語で表現すること
  とする
- コンポーネントの props の型は type Props とする
  \*\* type HogeProps や interface Props は禁止とする
- コンポーネント内の props は引数内で展開しない
  \*\* props.title, props.xxx のように使用すること
- レスポンシブデザインを意識すること
- スタイルは既存の @applications/frontend/shared/src/global.css
  を参照すること
- component 設計は atomic design を採用する
  ** atoms: 値を流し込むだけ ロジックを持ってはいけない
  ** molecules: 他の atoms, molecules で構成される・ロジ
  ックを持つが IO を伴うロジックは持ってはいけない
  ** organisms: template を構成する単位,header, footer,
  content, sidebar などの粒度, IO を伴う処理が可能
  ** templates: organisms を配置しページを構成する,
  IO を伴う処理が可能
- 基本的にはサーバーコンポーネントを使う
  - useState などクライアントコンポーネントでのみ動作するものを使う場合だけクライアントコンポーネントにする

# 言語

- 公開リポジトリではドキュメントやコミットメッセージを英語で記述する

# 環境

- GitHub: {{ .github_username }}
- リポジトリ: ghq 管理（`~/ghq/github.com/owner/repo`）

## Commands

```bash
# Development
pnpm dev              # Start local development (emulator + reader/admin)
pnpm dev:reader       # Run reader app only (port 3000)
pnpm dev:admin        # Run admin app only (port 3001)
pnpm storybook        # Run Storybook (port 6006)

# Build & Lint
pnpm build            # Build all packages
pnpm lint             # Lint all packages

# Test
pnpm test             # Run all tests with vitest
vitest run applications/frontend/shared/tests/domains/common/date.test.ts  # Run single test
```

## Architecture

This is a pnpm monorepo with the following structure:

- `applications/frontend/reader` - Public-facing Next.js app (port 3000)
- `applications/frontend/admin` - Admin Next.js app (port 3001)
- `applications/frontend/shared` - Shared library consumed by both apps

### Shared Package Structure

The shared package follows Domain-Driven Design:

- `domains/` - Domain models, aggregates, value objects with Zod schemas for validation
  - Each domain has entities branded with Zod (e.g., `ArticleIdentifier`, `ArticleTitle`)
  - Repository interfaces defined here (implemented in infrastructures)
  - Domain events defined in `event.ts` files
- `aspects/` - Cross-cutting concerns (Result types, error handling, logging, auth)
- `infrastructures/` - Repository implementations using Firebase/Firestore
- `workflows/` - Business logic orchestrating domain operations with functional composition
- `actions/` - Next.js Server Actions
- `components/` - React components (atoms/molecules/organisms/templates)
- `config/` - Configuration (routes, profiles)

### Key Patterns

**Result Type**: Operations return `Result<T, E>` or `AsyncResult<T, E>` instead of throwing. Use `ok()`, `err()`, and chain with `andThen()`, `map()`, `match()`.

**Workflow Composition**: Workflows are curried functions that inject dependencies:

```typescript
const workflow = createArticleFindWorkflow(validate)(find)(logger);
```

**Branded Types**: Domain types use Zod `.brand()` for type safety:

```typescript
const articleIdentifierSchema = z.ulid().brand("ArticleIdentifier");
```

**Immutable Collections**: Custom immutable collection types in `domains/common/collections/` (List, Map, Set, Queue, Stack, etc.) using HAMT.

### Shared Package Exports

Import from shared using path aliases:

```typescript
import { Article } from "@shared/domains/articles";
import { ok, err } from "@shared/aspects/result";
import { createArticleFindWorkflow } from "@shared/workflows/article";
```

## Coding Conventions

- Variable/function names: No abbreviations except common ones (URL, UUID, ULID)
- TypeScript: `as any` and `as unknown` are prohibited
- Unused variables: Prefix with underscore (e.g., `_unused`)

## ブランチ運用フロー

基本の流れ: `feature/fix/chore/infra` → `staging` → `main` → `release-please` Release PR → 本番自動デプロイ

### 時系列ステップ（通常の機能リリース）

| # | アクション | 走る workflow | 主体 |
|---|-----------|--------------|------|
| 1 | feature ブランチを切る (例: `feat/issue-50`) | - | 開発者 |
| 2 | feature → staging に PR 作成 | CI / E2E / Terraform CI（infra 変更時） | 開発者 |
| 3 | レビュー後 staging にマージ | （再 push 時に staging への CI/E2E） | 開発者 |
| 4 | staging → main に PR 作成 | CI / E2E / Terraform CI（infra 変更時） | 開発者 |
| 5 | レビュー後 main にマージ | release-please workflow が起動 | 開発者 |
| 6 | release-please が Release PR 起票 (PR #40 の形式) | - | bot |
| 7 | 蓄積された機能を確認、Release PR をマージ | release-please 再起動 → tag 作成 → deploy-admin-prd 起動 → 本番自動デプロイ | 開発者 + bot |

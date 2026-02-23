# HSpec テスト手順書 - search-token-worker

## 1. 環境情報

| 項目 | 値 |
|---|---|
| GHC | 9.12.2 |
| 言語標準 | GHC2021 |
| テストFW | HSpec 2.11.x |
| プロパティテスト | QuickCheck |

## 2. セットアップ

### 2.1 ディレクトリ構造

```
applications/search-token-worker/
  src/                           # 本番コード
    Aspects/Log.hs
    Domain/Common.hs
    Domain/Event.hs
    Domain/Ngram.hs
    Domain/SearchToken.hs
    Infrastructure/Firestore.hs
    UseCase/EventHandler.hs
  test/                          # テストコード (新規作成)
    Spec.hs                      # テストドライバー (hspec-discover)
    Unit/
      Domain/
        CommonSpec.hs
        NgramSpec.hs
        EventSpec.hs
      UseCase/
        EventHandlerSpec.hs
  search-token-worker.cabal
```

ソースモジュールの構造をテストディレクトリにミラーリングする。
ファイル名は必ず `Spec.hs` で終わる。

### 2.2 cabal ファイルへの test-suite 追加

```cabal
test-suite spec
  type:             exitcode-stdio-1.0
  hs-source-dirs:   test
  main-is:          Spec.hs
  other-modules:
    Unit.Domain.CommonSpec
    Unit.Domain.NgramSpec
    Unit.Domain.EventSpec
    Unit.UseCase.EventHandlerSpec
  build-depends:
    base >= 4.17 && < 5,
    hspec >= 2.11 && < 2.12,
    hspec-discover >= 2.11 && < 2.12,
    QuickCheck >= 2.14,
    search-token-worker,
    text ^>= 2.1.4,
    containers,
    time ^>= 1.14,
    mtl
  build-tool-depends:
    hspec-discover:hspec-discover
  default-language: GHC2021
  ghc-options: -Wall -threaded -rtsopts -with-rtsopts=-N
```

ポイント:
- `build-depends` に `search-token-worker` (自パッケージ) を含めることで `src/` のモジュールをインポート可能
- `build-tool-depends` に `hspec-discover` を指定して自動テスト探索を有効化
- `-threaded -rtsopts -with-rtsopts=-N` で並列テスト実行を有効化

### 2.3 テストドライバー (test/Spec.hs)

```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

この1行のみ。`hspec-discover` が `test/` 配下の `*Spec.hs` ファイルを自動検出してテストスイートを構築する。

### 2.4 依存パッケージの解決

```bash
cabal update
cabal build spec  # テストスイートのビルドのみ
```

## 3. テストの基本構文

### 3.1 最小限のSpecファイル

```haskell
module Unit.Domain.NgramSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "generateNgramsBySize" $ do
    it "generates bigrams from a string" $ do
      True `shouldBe` True
```

規約:
- モジュール名はファイルパスと一致 (`test/Unit/Domain/NgramSpec.hs` → `Unit.Domain.NgramSpec`)
- `spec :: Spec` をエクスポートする
- `describe` でテスト対象の関数/モジュールをグルーピング
- `it` で個別のテストケースを記述

### 3.2 構造化: describe / context / it

```haskell
spec :: Spec
spec = do
  describe "generateNgramsBySize" $ do
    context "when given normal text" $ do
      it "generates correct bigrams" $ do
        ...

    context "when given empty string" $ do
      it "returns empty lists" $ do
        ...

    context "when text is shorter than min ngram size" $ do
      it "returns empty lists for all sizes" $ do
        ...
```

| 関数 | 用途 | 命名規則 |
|---|---|---|
| `describe` | テスト対象 (関数名, モジュール名) | 関数名そのまま |
| `context` | 条件分岐 | "when ..." / "with ..." で始める |
| `it` | 具体的な振る舞い | 期待する結果を記述 |

`context` は `describe` のエイリアス。意味的に使い分ける。

## 4. マッチャー (Expectations)

### 4.1 等値比較

```haskell
1 + 1 `shouldBe` 2
1 + 1 `shouldNotBe` 3
```

### 4.2 述語ベース

```haskell
[1, 2, 3] `shouldSatisfy` (not . null)
"" `shouldSatisfy` null
23 `shouldSatisfy` (> 20)
```

### 4.3 リスト操作

```haskell
[1, 2, 3] `shouldContain` [2, 3]
[1, 2, 3] `shouldStartWith` [1, 2]
[1, 2, 3] `shouldEndWith` [2, 3]
[3, 1, 2] `shouldMatchList` [1, 2, 3]  -- 順序不問
```

### 4.4 IO アクション

```haskell
pure 42 `shouldReturn` 42
```

### 4.5 例外

```haskell
evaluate (error "boom") `shouldThrow` anyException
evaluate (error "foo") `shouldThrow` errorCall "foo"
evaluate (1 `div` 0) `shouldThrow` (== DivideByZero)
```

### 4.6 Either の検証 (Contrib)

```haskell
import Test.Hspec.Expectations.Contrib (isLeft, isRight)

result `shouldSatisfy` isRight
failResult `shouldSatisfy` isLeft
```

## 5. 純粋関数のテスト

副作用のない関数は直接値を比較できる。このプロジェクトで最も多いパターン。

```haskell
module Unit.Domain.NgramSpec (spec) where

import Test.Hspec
import Domain.Ngram (generateNgramsBySize)

spec :: Spec
spec = do
  describe "generateNgramsBySize" $ do
    it "generates bigrams and trigrams" $ do
      let result = generateNgramsBySize 2 3 "abcd"
      length result `shouldBe` 2
      head result `shouldMatchList` ["ab", "bc", "cd"]
      result !! 1 `shouldMatchList` ["abc", "bcd"]

    context "when given empty string" $ do
      it "returns empty lists for all sizes" $ do
        generateNgramsBySize 2 4 "" `shouldBe` [[], [], []]

    context "when text is shorter than min ngram size" $ do
      it "returns empty lists" $ do
        generateNgramsBySize 3 4 "ab" `shouldBe` [[], []]
```

## 6. モナディックコードのテスト

### 6.1 WriterT のテスト

`runWriterT` で結果とログの両方を検証する。

```haskell
import Control.Monad.Writer (runWriterT)

spec :: Spec
spec = do
  describe "handle" $ do
    it "produces log entries" $ do
      (result, logs) <- runWriterT (handle mockPersist mockTerminate event)
      result `shouldSatisfy` isRight
      logs `shouldSatisfy` (not . null)
```

### 6.2 ExceptT + WriterT のスタック

このプロジェクトの `Infrastructure.Firestore` は `ExceptT SearchTokenError (WriterT [LogEntry] IO)` を使用する。

```haskell
import Control.Monad.Except (runExceptT)
import Control.Monad.Writer (runWriterT)

spec :: Spec
spec = do
  describe "some operation" $ do
    it "succeeds with expected logs" $ do
      (result, logs) <- runWriterT (runExceptT someOperation)
      result `shouldBe` Right ()
      length logs `shouldSatisfy` (> 0)

    it "fails with error" $ do
      (result, _logs) <- runWriterT (runExceptT failingOperation)
      result `shouldBe` Left Unexpected
```

### 6.3 モック戦略: 関数の注入

このプロジェクトの `EventHandler.handle` は既に依存関数を引数で受け取る設計になっている:

```haskell
handle :: (MonadIO m, MonadWriter [LogEntry] m)
       => Persist m -> TerminateByReference m -> Event -> m (Either SearchTokenError ())
```

テストではモック関数を注入する:

```haskell
module Unit.UseCase.EventHandlerSpec (spec) where

import Test.Hspec
import Control.Monad.Writer (runWriterT)
import Data.IORef
import Domain.SearchToken
import Domain.Event
import UseCase.EventHandler (handle)

spec :: Spec
spec = do
  describe "handle" $ do
    context "when receiving ArticleCreatedPayload" $ do
      it "calls persist with generated tokens" $ do
        persistedRef <- newIORef []
        let mockPersist tokens = do
              liftIO $ writeIORef persistedRef tokens
              pure (Right ())
            mockTerminate _ = pure (Right ())
        (result, _logs) <- runWriterT (handle mockPersist mockTerminate articleCreatedEvent)
        result `shouldBe` Right ()
        persisted <- readIORef persistedRef
        persisted `shouldSatisfy` (not . null)

    context "when receiving ArticleTerminatePayload" $ do
      it "calls terminate with correct reference" $ do
        terminatedRef <- newIORef Nothing
        let mockPersist _ = pure (Right ())
            mockTerminate reference = do
              liftIO $ writeIORef terminatedRef (Just reference)
              pure (Right ())
        (result, _logs) <- runWriterT (handle mockPersist mockTerminate articleTerminateEvent)
        result `shouldBe` Right ()
        terminated <- readIORef terminatedRef
        terminated `shouldBe` Just "article:some-article-id"
```

ポイント:
- `IORef` でモック関数の呼び出しを記録
- `Persist m` と `TerminateByReference m` はそもそも関数型エイリアスなので、直接ラムダを渡せる
- `MonadIO` 制約があるため、`liftIO` でIORefを操作

### 6.4 モック Persist / TerminateByReference の実装方針

`EventHandler.handle` のテストには `Persist m` と `TerminateByReference m` のモックが必要。
これらは関数の型エイリアスである:

```haskell
type Persist m = [SearchToken] -> m (Either SearchTokenError ())
type TerminateByReference m = Reference -> m (Either SearchTokenError ())
```

モックの目的は2つ:
1. 呼び出されたことと渡された引数を記録する
2. テストケースごとに成功/失敗の戻り値を制御する

配置場所:

```
test/
  Support/
    Mock/
      Domain/
        SearchToken.hs    -- モック Persist / TerminateByReference
```

<details>
<summary>ヒント1</summary>

`Persist m` と `TerminateByReference m` は関数の型エイリアスなので、
ラムダを直接渡せる。モックとして再利用可能にするには、
**モック生成関数**を定義して引数で振る舞いを制御する。

モック生成関数に必要な機能:
- 呼び出し時の引数を記録する（`IORef` を使う）
- テストケースごとに戻り値を切り替える（成功/失敗）

`handle` の型制約に `MonadIO m` があるため、
モック内部で `IORef` を操作するには `liftIO` が必要。

</details>

<details>
<summary>ヒント2</summary>

モック生成関数のシグネチャ:

```haskell
createMockPersist ::
  (MonadIO m) =>
  IORef [SearchToken] ->
  Either SearchTokenError () ->
  Persist m

createMockTerminateByReference ::
  (MonadIO m) =>
  IORef [String] ->
  Either SearchTokenError () ->
  TerminateByReference m
```

- 第1引数: `IORef` — 渡された引数を蓄積する記録先
- 第2引数: `Either SearchTokenError ()` — モックが返す戻り値

テスト側では `runWriterT` でモナドスタックを実行し、
`result` で成功/失敗を、`logs` でログ出力を検証する。

```haskell
(result, logs) <- runWriterT (handle mockPersist mockTerminate event)
```

</details>

<details>
<summary>ヒント3</summary>

```haskell
module Support.Mock.Domain.SearchToken
  ( createMockPersist,
    createMockTerminateByReference,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, modifyIORef')
import Domain.SearchToken (Persist, SearchToken, SearchTokenError, TerminateByReference)

createMockPersist ::
  (MonadIO m) =>
  IORef [SearchToken] ->
  Either SearchTokenError () ->
  Persist m
createMockPersist record result tokens = do
  liftIO $ modifyIORef' record (<> tokens)
  pure result

createMockTerminateByReference ::
  (MonadIO m) =>
  IORef [String] ->
  Either SearchTokenError () ->
  TerminateByReference m
createMockTerminateByReference record result reference = do
  liftIO $ modifyIORef' record (<> [reference])
  pure result
```

注意点:
- `modifyIORef'` で蓄積（`writeIORef` だと上書きになる）
- `Persist m` = `[SearchToken] -> m (Either SearchTokenError ())` なので、
  `createMockPersist` は3引数関数（`IORef` と `Either` で部分適用して `Persist m` を得る）

</details>

## 7. プロパティベーステスト (QuickCheck)

### 7.1 基本

```haskell
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do
  describe "generateNgramsBySize" $ do
    prop "always returns (maxLength - minLength + 1) lists" $
      \(Positive minLen) (Positive maxLen) text ->
        let min' = minLen `mod` 5 + 1
            max' = min' + (maxLen `mod` 3)
         in length (generateNgramsBySize min' max' text) `shouldBe` (max' - min' + 1)

    prop "all ngrams in a group have the same length" $
      \text ->
        let groups = generateNgramsBySize 2 4 text
         in all (\(n, group) -> all (\ngram -> length ngram == n) group)
                (zip [2..4] groups)
```

### 7.2 テスト回数の調整

```haskell
import Test.Hspec.QuickCheck (modifyMaxSuccess)

spec :: Spec
spec = do
  modifyMaxSuccess (const 1000) $ do
    prop "ngram invariant" $ ...
```

### 7.3 カスタム Arbitrary

```haskell
newtype NonEmptyAlphaString = NonEmptyAlphaString String
  deriving (Show)

instance Arbitrary NonEmptyAlphaString where
  arbitrary = NonEmptyAlphaString <$> listOf1 (elements ['a'..'z'])
```

## 8. テスト実行コマンド

```bash
# 全テスト実行
cabal test

# テスト出力を詳細表示
cabal test --test-show-details=direct

# 特定パターンにマッチするテストのみ
cabal test --test-show-details=direct --test-option=-m --test-option="Ngram"

# パスで絞り込み (/ 区切り)
cabal test --test-show-details=direct \
  --test-option=-m \
  --test-option="generateNgramsBySize/when given empty string"

# 最初の失敗で中止
cabal test --test-show-details=direct --test-option=--fail-fast

# 前回失敗したテストのみ再実行
cabal test --test-show-details=direct --test-option=--rerun

# 特定テストをスキップ
cabal test --test-show-details=direct --test-option=--skip --test-option="slow"

# テスト一覧表示 (実行しない)
cabal test --test-show-details=direct --test-option=--dry-run
```

## 9. コード内でのフォーカス

開発中に特定テストだけ実行したい場合:

```haskell
spec :: Spec
spec = do
  describe "fast tests" $ do
    it "test1" $ True `shouldBe` True

  fdescribe "focused tests" $ do
    fit "only this runs" $ True `shouldBe` True
```

| 通常 | フォーカス版 |
|---|---|
| `describe` | `fdescribe` |
| `it` | `fit` |
| `context` | `fcontext` |

CI で focus の外し忘れを検出する:

```bash
cabal test --test-option=--fail-on=focused
```

## 10. セットアップ/ティアダウン (フック)

```haskell
spec :: Spec
spec = do
  before setupAction $ do
    it "uses the setup result" $ \resource -> do
      resource `shouldSatisfy` isValid

  around (bracket acquire release) $ do
    it "has resource access" $ \resource -> do
      ...
```

| フック | 用途 |
|---|---|
| `before` | 各テスト前に実行、値を提供 |
| `before_` | 各テスト前に実行、値なし |
| `beforeAll` | 全テスト前に1回、値を共有 |
| `after` | 各テスト後に実行 |
| `afterAll` | 全テスト後に1回 |
| `around` | 前後処理 + 値提供 (bracket パターン推奨) |

## 11. 並列実行

```haskell
spec :: Spec
spec = parallel $ do
  describe "independent tests" $ do
    it "test 1" $ ...
    it "test 2" $ ...
```

グローバルに並列化する場合は `test/SpecHook.hs` を作成:

```haskell
module SpecHook where

import Test.Hspec

hook :: Spec -> Spec
hook = parallel
```

## 12. このプロジェクトのテスト対象と優先度

| 優先度 | モジュール | テスト対象 | テスト種別 |
|---|---|---|---|
| 高 | Domain.Ngram | `generateNgramsBySize` | 純粋 + QuickCheck |
| 高 | UseCase.EventHandler | `buildTokens`, `generateNgramIdentifiers`, `generateTagIdentifiers` | 純粋 |
| 高 | UseCase.EventHandler | `handle` (各イベントタイプ) | モナディック (モック注入) |
| 中 | Infrastructure.Firestore | `toDocument`, `toRefDocument`, `toIndexDocument` | 純粋 |
| 中 | Infrastructure.Firestore | `calculateScore` | 純粋 |
| 低 | Infrastructure.Firestore | `createPersist`, `createTerminate` | 統合テスト (Emulator必要) |
| 低 | Domain.Event | パターンマッチの網羅性 | 純粋 |

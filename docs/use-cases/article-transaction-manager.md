# Article TransactionManager 設計

状態: 関数注入方式を承認し、共通抽象とArticleの全13ユースケースへの接続を実装。
本番DBアダプターと実DB featureテストは未実装。

## Sharedへのバージョン管理共通化

他のbounded contextでも利用できるよう、バージョン管理をSharedへ移す方針。
保存バージョンはInfrastructureの関心事であり、Domain/UseCaseへ露出させない既存の合意を維持する。
詳細設計を承認し、Sharedへの移行を実装した。本番DBアダプターの実装は今回の範囲に含めない。

- 旧Infrastructure.Article.Versioningを廃止し、版番号とトランザクション内の観測状態追跡をSharedへ移した。
- ArticleIdentifierへの依存とArticle固定のエラー内容を切り離した。
- Shared.Infrastructure.Versioningへ、版番号とトランザクション内の観測状態追跡の両方を移す（合意済み）。
- DBアクセス・保存形式・具体的なトランザクション文脈の構築は各コンテキストのInfrastructureに残す。
- VersionContext identifierとして識別子の型を保持する（合意済み）。各識別子の定義は各コンテキストが所有し、異なる集約の識別子を渡す操作は型検査で拒否する。
- エラーに用いる集約名と識別子の表示方法は呼び出し側のInfrastructureから渡す（合意済み）。SharedからArticleへ依存せず、既存のDomainErrorを返す。Infrastructure専用エラー型は追加しない。
- Versionの構築には正の整数を表す専用型を要求する（合意済み）。生のIntegerを受け取るVersion構築APIにはしない。
- 正数型のコンストラクタは非公開とし、外部の数値は境界で検証する。0・負数を初期値に置き換えない。Num/Read等による未検証の構築経路は設けない。
- Shared.Domain.Common.Primitiveに汎用のPositiveIntegerを置き、newPositiveIntegerで検証する。Shared.Infrastructure.VersioningのnewVersionはPositiveIntegerを受け取る。保存バージョン自体はDomainには置かない。
- 今回はSharedへの共通化とArticle利用側の移行までとする（合意済み）。Media等への導入は各コンテキストの永続化設計時に行う。
- Sharedの分割ユニットテスト・型の取り違えを拒否するコンパイルテストと、Articleの接続回帰テストを整備し、対象の式カバレッジ90%以上を維持する。
- DB固有の数値範囲と安全な変換は各Infrastructureが担当する。正数型による保証と、DB値の実行時検証は区別する。
- 共通コードの再利用は、bounded context間で可変状態やDBトランザクションを共有する意味ではない。

## 決定

- Repositoryオブジェクト・型クラス・ArticleOperation GADTは導入しない。
- 取得・検索・永続化・削除は集約と同じDomain.Articleに関数型の別名として定義する。
- ユースケースは必要な関数とTransactionManagerを個別に受け取る。
- Persistに統一し、PersistNewは設けない。新規・更新の判断と保存バージョンはInfrastructureに閉じ込める。
- 記事の変更とOutbox追加は別々の関数とし、同一Transaction内で合成する。
- Domain/UseCaseのArticle、Command、関数の引数・戻り値に保存バージョンを追加しない。
- エラーはDomainErrorで統一する。PresentationがHTTP等の外部表現へ変換する。
- 単一の永続化リソースが対象。複数リソースを跨ぐ原子性は提供しない。

旧UseCase.Persistenceの保存クロージャー方式と、検討途中の操作GADT方式は廃止した。
以下が現行設計であり、末尾の成立性検証は保存先選定のための過去の検証記録である。

## 配置と型

```haskell
-- Domain.Article
type FindArticle m = ArticleIdentifier -> m (Maybe Article)
type PersistArticle m = Article -> m ()
type TerminateArticle m = ArticleIdentifier -> m ()
type SearchArticles m = Criteria -> m (Int, [Article])

-- Shared.Domain.Common.Transaction
runTransaction
    :: TransactionManager context m
    -> Transaction context m a
    -> m (Either DomainError a)
```

- FindArticleBySlug、FindSlugOwner、SearchPublishedArticlesもDomain.Articleに置く。
- Domain.Article.Criteriaは状態条件・ページ番号・件数を保持する値オブジェクト。
  コンストラクタは非公開で、newCriteriaが正の値、既定10件、上限100件、offsetの表現範囲を検証する。
- 検索結果は総件数と集約一覧。SummaryやCQRSのDTOは導入しない。
- 総件数は状態条件を適用した後、ページング前の件数。
  管理者用はupdatedAt DESC / identifier DESC、読者用はpublishedAt DESC / identifier DESC。
- 読者用はユースケースがPublishedOnlyのCriteriaを構築する。検索実装もそれ以外の条件を拒否する。
- requireArticleはUseCase.Helper。未取得を業務エラーへ変換し、返された集約の識別子も照合する。
- commandContextはShared.UseCase.Command。payloadだけを除き、timestamp等を保持する。
- 型付きOutbox関数はShared.UseCase.OutboxのAppend events m。
  イベントのEnvelope化・保存はInfrastructureで行い、ここでQueueへ配信しない。

## トランザクション文脈

Transactionの内部表現は、contextを受け取りm (Either DomainError a)を返す処理である。
コンストラクタと内部モジュールは非公開。公開APIは合成用のMonad、fromEither、abort、runTransaction。
MonadIO、エラー回復、ネスト用の操作は提供しない。

DependenciesではFindArticle (Transaction context m)等へ具体化する。
contextは型引数であり、Domain/UseCaseはその値や接続の構造を扱わない。
同じTransactionを合成すると、全処理に同じ文脈が渡る。

Shared.Infrastructure.TransactionのtransactionActionで具体的なDB関数を構築する。
TransactionDriverは物理トランザクションを開始し、毎回新しい文脈を作る。
具体関数は渡された文脈の接続を使い、別途閉じ込めた接続やグローバル変数へアクセスしない。
文脈には接続と、識別子ごとの取得状態・保存バージョンの管理領域を持たせ、終了時に破棄する。

Driverの責任:

- コールバックを同一物理トランザクション内で一度だけ実行する。
- Leftでロールバックし、最初の失敗以降は後続操作を行わない。
- 正常終了時はコミットし、確定を確認できた場合だけCommittedを返す。
- 確認済みの失敗はRolledBack、不明はOutcomeUnknownで区別する。結果不明をロールバック済みと断定しない。
- 例外・キャンセル時の後処理、再入防止、接続の破棄を担う。
- 自動再試行はしない。複数回実行の安全性をこの抽象で代替しない。

型によって制限するのはUseCaseの公開APIであり、信頼されたInfrastructure実装が
この契約に違反することまで型だけで防ぐわけではない。

## Infrastructureのバージョン管理

Shared.Infrastructure.VersioningにDB非依存の追跡ロジックを実装する。
VersionとVersionContextのコンストラクタは非公開。Domainに保存バージョンを持たせない。
VersionContext identifierは識別子をMapのキーとして保持し、Ord identifierを要求する。
識別子の表示関数はエラー表示専用であり、キーの同一性判定には使用しない。
型引数のroleはnominalとし、同じ内部表現の識別子同士でもcoerceによる文脈の取り違えを拒否する。

emptyVersionContextには集約名と識別子の表示関数を渡す。
期待版の直接照合を行うcheckExpectedVersionにも呼び出し側が集約名を渡す。
DBからの復元はnewPositiveIntegerによる境界検証後にnewVersionで行い、
保存時はversionIntegerで取り出す。PositiveIntegerは非公開のNaturalで値の前数を保持するため、
内部値0も正数1を表す。oneとnextPositiveIntegerは正数性を保ち、Num/Readは提供しない。

| 同一トランザクションでの観測 | Persist |
| --- | --- |
| 未取得 | insert-only |
| 不存在を取得済み | insert-only |
| 取得済み | 取得時の期待バージョンによる条件付き更新 |
| 自分で削除済み | 再作成を拒否 |

- insert衝突で既存記事を上書きしない。条件付き更新失敗をinsertへ切り替えない。
- Findと検索結果のバージョンを同じ管理領域へ記録する。
- 後から異なるバージョンを観測しても期待値を最新に置き換えず、ProcessingTargetChangedとする。
- 自分のPersist成功後は追跡中のバージョンも進める。失敗時には進めない。
- 自分でTerminateした識別子は、その後FindがNothingを返しても削除済みとして保持する。
- SQLの一意性制約・期待バージョンの照合・更新件数の検証は具体アダプター側で行う。
- Slug確認APIは予約ではない。保存時にも一意性制約が必須。

## 非同期生成結果

生成依頼のOutbox記録には、保存時の記事バージョンをInfrastructureメタデータとして保持する。
これはイベントのスキーマバージョンとは異なる。

コンシューマーの組み立て処理が対象記事と期待版を束縛したFindArticleを注入する。
同じDBトランザクション内で取得した現在版と照合し、期待版を最新に差し替えない。
手動Excerpt修正には通常のFindArticleを注入する。

不一致はProcessingTargetChanged。例として、版3から生成したExcerptが、
編集・再校正後の版5へ届いた場合がある。数値の版をDomainErrorには追加しない。
確定した不一致は変更を適用せず、コンシューマーで処理済みにする方針。
一時障害・TransactionOutcomeUnknownを同じ理由でackしない。

今回検証したのは期待版照合とユースケースの中断まで。
Outboxの版メタデータ保存、Queueのack処理、実際の生成コンシューマーは具体アダプター実装時に接続する。

## ユースケースの境界

- 更新系: 取得、ドメイン遷移、Persist/Terminate、必要なOutbox追加を同じTransaction内で行う。
- イベントのない再開・手動Excerpt修正ではOutbox追加を行わない。
- JotDown: 入力検証と識別子生成を先に行い、集約構築・Persist・Outbox追加をTransaction内で行う。
- Proofread: 読取Transactionで画像参照を取得し、終了後にMediaを問い合わせる。
  更新Transactionで記事を再取得し、画像参照を照合してから、現在の記事を校正・保存する。
  Media側の確認後の変化までArticleのトランザクションで保証しない。
- 一覧・詳細・Slug確認も、同一読取Transaction内で実行する。
- AI生成、Media問い合わせ、Queue配信はDBトランザクションの外に置く。

## 検証と残作業

test/unit以下でCriteria、Infrastructureの追跡ロジック、各ユースケース、
Sharedの合成・確定境界・失敗処理をファイル分割して検証する。
check-domain.shが式カバレッジ90%以上とコンパイル拒否テストを検証する。

インメモリDriverでは、新規・更新・削除、文脈の共有と分離、変更とOutboxの
ロールバック契約、commit結果不明時の非再試行、古い生成結果の拒否を確認する。
LegacyPersistenceとTransactionSupportは既存動作の回帰テスト用の記録アダプターに限定し、
本番ポートやDB原子性の証明には使用しない。

実DBアダプター、保存先の正式な決定、例外・キャンセル・再入の本番実装、
イベント世代メタデータ、冪等性は残作業。
Docker/Wranglerを使う実接続のfeatureテストは、具体アダプター実装時に行う。
今回のユニットテストを実DBでの検証とみなさない。

## 実現可能性の検証

要求は、記事取得・Haskellのドメイン処理・保存・Outbox追加を同一DBトランザクション内で完結させること。
取得を外で行う楽観的競合検証方式への置き換えは未承認であり、同じ保証とは扱わない。

### D1

公式公開APIとローカルMiniflare 4.20260617.1で検証した。
スクリプトはprobes/d1-transactions.cjs。引数にインストール済みminiflareのモジュールパスを渡してnodeで実行する。
一時的なローカルDBのみを使い、終了時にdisposeする。本番接続・Haskell WASM実行・featureテストではない。

- BEGIN TRANSACTION / SAVEPOINTは実行エラーとなる。
- batch内の制約違反では記事更新とOutbox追加が両方ロールバックされた。
- 条件付きUPDATEが0件でもbatchは成功し、続くOutbox追加はコミットされた。
- withSession内の更新後にアプリケーション例外を発生させても、更新は残った。

採用中のcloudflare-workers-hsのref ab1877a336fc29264bb11f9a50953113cda4dc8bも確認した。
Binding.D1にはd1Batch / d1Run / d1First等があるが、対話的なトランザクションAPIはない。
d1Batchは事前構築済みのSQL文一覧を受け取り、戻り値はD1RunResult一覧である。

結論: 現在のD1公開APIでは要求する対話的トランザクションを直接実現できない。
SELECTをbatchに含めることは可能だが、その途中結果をHaskellで処理してから同じトランザクションを継続するものではない。
Sessionsは逐次一貫性の仕組みであり、複数呼び出しを一括コミット・ロールバックする仕組みではない。
ローカルエラーに登場するstorage.transactionはDO内部ストレージ用であり、D1 bindingから呼び出せるAPIではない。

### 次の候補（未採用）

SQLite-backed Durable Objectはtransaction / transactionSyncを公開している。
ただし対象はDO自身のストレージであり、外部D1をトランザクションに含めるものではない。
transactionSyncはPromiseを返すコールバックを許可しないため、Haskell WASMの非同期呼び出しにはそのまま使わない。

### DOとHaskell WASMのローカル検証結果

[再現手順・検証コード](probes/do-transaction/README.md)を追加した。
Miniflare 4.20260617.1、GHC WASM 9.14.1.20260731、既存hutのWASMランタイムで実行し、全検証に成功した。
DO自身のSQLiteに対するstorage.transaction(async callback)内からHaskellのexportを呼び、
HaskellがSQL取得・純粋な判断・更新・更新結果の再取得・Outbox追加を行う。

- 正常終了で記事更新とOutbox追加が確定する。
- 更新後の業務エラー相当の戻り値をJS側で例外に変換するとロールバックされる。
- Outboxの一意制約違反、両方の書き込み後のHaskell例外もロールバックされる。
- Haskellの非同期待機を含む8並列要求で、リビジョン・Outbox件数の欠落や重複がない。
- 成功と失敗が混在する8並列要求でも、成功分だけが残る。
- DOインスタンスごとにデータが分離され、ランタイム再起動後もデータが残る。

これは最小のモデルと検証専用JSFFIによる成立性確認であり、既存Articleユースケースを接続したものではない。
現在のライブラリのdoStorageTransactionは固定KV操作一覧のAPIであるため、SQL操作とHaskellコールバックの橋渡しを追加する必要がある。
DomainErrorの損失のない伝達、キャンセル・クラッシュ・コミット結果不明時の挙動、性能、
本番の耐久性・PITRは未検証。採用する場合はこれらを実装とfeatureテストで確認する。
初期案のD1TransactionManagerは要件を満たさないため実装に進めず、DO用の具体実装を候補とする。
単一DOへの配置は検証用の候補であり、保存先の正式な変更はまだ決定していない。

### 根拠

- [D1 batch API](https://developers.cloudflare.com/d1/worker-api/d1-database/)
- [D1 Sessionsの一貫性](https://developers.cloudflare.com/d1/best-practices/read-replication/)
- [DO SQLite storage transaction API](https://developers.cloudflare.com/durable-objects/api/sqlite-storage-api/)

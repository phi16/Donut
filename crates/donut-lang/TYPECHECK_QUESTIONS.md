# Type Checker 実装に向けた未解決事項

## 現状

- パイプライン: tokenize → parse (syntree) → convert (semtree) → resolve (名前解決 → item) → load (item → FreeCell)
- resolve: 名前解決とスコープ管理を行い `(Module, Vec<Error>)` を返す
- load: item から FreeCell への変換。型チェック（次元の一致、boundary の一致）を部分的に実施
- `*` は resolve では未定義警告が出るが、load 側で `Ty::Zero`（0-cell の型）として特別扱い

## 未解決の仕様質問

1. **`*` の扱い**: 現在 load で特別扱い。将来的に組み込みシンボルとして resolve レベルで認識させるか、現状の特殊扱いで十分か
2. **レベル体系**: `Comp(0)` (空白), `Comp(1)` (`;`), `Comp(2)` (`;;`) ... が圏のレベルに対応？ n-cell の boundary が (n-1)-cell の composition で表される？
3. **composition の型付け規則**: `f: A → B`, `g: B → C` のとき `f g: A → C` か `g f: A → C` か（diagrammatic order?）。レベルをまたいだ composition の型付けは？
4. **`~` と `~>` の意味**: `~` は等式/2-cell、`~>` は functor。具体的な型付け規則は？
5. **`;*` の意味**: 任意個の composition？ワイルドカード的なレベル？
6. **パラメータの型的扱い**: `f[x: T]` の `x` は型チェック時にどう扱う？ 単に `x: T` としてスコープに入れるだけか、parametric な量化か？
7. **`with` の型的意味**: メンバ追加は名前空間だけか、型的にも意味がある？（`invertible` の例のように構造体的な型が形成される？）
8. **型の等価性 / 正規化**: composition は結合的に正規化される？ `(f g) h` と `f (g h)` は同じ型？
9. **エラーの粒度**: 型チェッカーが報告すべきエラーの種類（型の不一致、レベルの不一致、boundary の不一致、等）

## アーキテクチャ上の課題

### resolve と type-check の統合

現在 resolve は名前解決のみで、型チェックは load 内で部分的に行われている（次元の一致、boundary の一致）。

将来的には resolve 内で型チェックを同時に行い、Item に「計算済みの型」を格納する方向が望ましい:
- 宣言処理時に一度だけ型の計算が行われる
- `+=` では型が確定済みの Item を共有/移動するだけで済む
- load は型情報を信頼して FreeCell 変換に専念できる

型システムの仕様が固まった段階でこの統合を検討する。

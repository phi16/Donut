# Type Checker 実装に向けた未解決事項

## 把握済み

- パイプライン: tokenize → parse (syntree) → convert (semtree) → check (名前解決)
- 現在の check: 名前解決のみ（スコープ管理、定義/参照、モジュールメンバ、with/where/+=）
- semtree の構造: Val, Path, Lit, Op(Comp/CompStar/Arrow), DeclUnit（型注釈 ty フィールドあり）
- 演算子の種類: composition (空白 `;` `;;` `;;;`...), arrow (`→` `~` `~>`), CompStar (`;*`)
- 宣言形式: `x: T`, `x: T = v`, `x = v`, `f[p: T]: R = v`

## 未解決の仕様質問

1. **`*` の意味**: `C: *` は「C はオブジェクト（0-cell）」？ `*` は組み込み定数？
2. **レベル体系**: `Comp(0)` (空白), `Comp(1)` (`;`), `Comp(2)` (`;;`) ... が圏のレベルに対応？ n-cell の boundary が (n-1)-cell の composition で表される？
3. **composition の型付け規則**: `f: A → B`, `g: B → C` のとき `f g: A → C` か `g f: A → C` か（diagrammatic order?）。レベルをまたいだ composition の型付けは？
4. **`~` と `~>` の意味**: `~` は等式/2-cell、`~>` は functor。具体的な型付け規則は？
5. **`;*` の意味**: 任意個の composition？ワイルドカード的なレベル？
6. **パラメータの型的扱い**: `f[x: T]` の `x` は型チェック時にどう扱う？ 単に `x: T` としてスコープに入れるだけか、parametric な量化か？
7. **`with` の型的意味**: メンバ追加は名前空間だけか、型的にも意味がある？（`invertible` の例のように構造体的な型が形成される？）
8. **型の等価性 / 正規化**: composition は結合的に正規化される？ `(f g) h` と `f (g h)` は同じ型？
9. **エラーの粒度**: 型チェッカーが報告すべきエラーの種類（型の不一致、レベルの不一致、boundary の不一致、等）

## 実装前の準備タスク

- [ ] check が Module/Item（スコープ辞書）を返すようにする（現在はエラーリストのみ）
- [ ] `*` の扱いに関する文法追加の検討

## アーキテクチャ上の課題メモ

### check と type-check の統合について

現在の方針: check (名前解決) が Item に生の `A<Val>` を Rc で保持し、後の type-check フェーズで処理する。
しかし `x += y` のように Item が複製される場合、同じ Val に対して type check が複数回走ることになり非効率。

**将来的には check 内で type check を同時に行い、Item には「計算済みの型 (Type)」を格納すべき。**
これにより:
- Val を Item に保持する必要がなくなる（Rc 不要）
- 型の計算が宣言処理時に一度だけ行われる
- `+=` では型が確定済みの Item を共有/移動するだけで済む

型システムの仕様が固まった段階でこの統合を検討する。

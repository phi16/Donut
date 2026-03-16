# 例: pentagon

モノイドの結合律に関する **pentagon identity**（五角恒等式）を Donut で記述する例です。

## 全体コード

```
u: *
x: u → u
μ: x x → x
α: μ x; μ → x μ; μ
icl: (x μ; μ) x → x μ x; μ x
icr: x μ x; x μ → x (μ x; μ)
top =
    α x; μ ;;
    icl; μ ;;
    x μ x; α ;;
    icr; μ ;;
    x α; μ
ic0: μ x x; x μ → μ μ
ic1: μ μ → x x μ; μ x

kl: (μ x; μ) x → μ x x; μ x
kr: x x μ; x μ → x (x μ; μ)
bot =
    kl; μ ;;
    μ x x; α ;;
    (ic0 ;; ic1); μ ;;
    x x μ; α ;;
    kr; μ

pentagon: top → bot
result = pentagon
```

## 解説

### 基本構造

```
u: *
x: u → u
μ: x x → x
```

- `u` — 0-cell（対象）
- `x` — 1-cell（`u` 上の射）
- `μ` — 2-cell（二項演算。`x` を2つ受け取り `x` を返す）

これはモノイドの骨格です。

### 結合律 (associator)

```
α: μ x; μ → x μ; μ
```

`α` は 3-cell です。source と target はそれぞれ `x` を3つ合成する2通りの括弧付けを表します:

- `μ x; μ` — `μ(μ(a, b), c)` — 左から結合
- `x μ; μ` — `μ(a, μ(b, c))` — 右から結合

### interchange cell

```
icl: (x μ; μ) x → x μ x; μ x
icr: x μ x; x μ → x (μ x; μ)
```

4つの `x` を結合する際に、括弧の位置を入れ替える 3-cell です。

### 2つの経路

`x` を4つ結合するとき、括弧の付け方は5通りあります:

```
((ab)c)d  →  (a(bc))d  →  a((bc)d)  →  a(b(cd))

((ab)c)d  →  (ab)(cd)  →  a(b(cd))
```

完全左結合 `((ab)c)d` から完全右結合 `a(b(cd))` に至る経路が2通りあり、それぞれが `top` と `bot` に対応します。

#### top: 上の経路

```
top =
    α x; μ ;;        -- ((ab)c)d → (a(bc))d
    icl; μ ;;         -- (a(bc))d → a(bc)d
    x μ x; α ;;      -- a(bc)d   → a(b(cd))
    icr; μ ;;         -- 括弧の調整
    x α; μ            -- 最終調整
```

各ステップは 3-cell で、`;;` による 2次合成で繋いでいます。`α x; μ` は「左3つに α を適用し、残りの `x` は恒等的に通す」ことを意味します。

#### bot: 下の経路

```
bot =
    kl; μ ;;           -- ((ab)c)d → (ab)(cd)
    μ x x; α ;;       -- (ab)(cd) に α を適用
    (ic0 ;; ic1); μ ;; -- interchange
    x x μ; α ;;       -- 左側に α を適用
    kr; μ              -- → a(b(cd))
```

こちらは途中で `(ab)(cd)` という「2つずつに分ける」括弧付けを経由します。

#### pentagon: coherence

```
pentagon: top → bot
```

`top` と `bot` はどちらも同じ source（完全左結合）と target（完全右結合）を持つ 3-cell です。`pentagon` はこの2つの経路の間の **4-cell** であり、「どちらの経路で括り直しても結果は同じ」という coherence 条件を表現しています。

これが Mac Lane の **pentagon identity** — モノイダル圏の公理の中核です。

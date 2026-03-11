# 例: pentagon

モノイドの結合律に関する **pentagon identity**（五角恒等式）を Donut で記述する例です。

## 全体コード

```
u: *
x: u → u
m: x x → x
a: m x; m → x m; m
chl: (x m; m) x → x m x; m x
chr: x m x; x m → x (m x; m)
aaa =
    a x; m ;;
    chl; m ;;
    x m x; a ;;
    chr; m ;;
    x a; m
ch0: m x x; x m → m m
ch1: m m → x x m; m x

kl: (m x; m) x → m x x; m x
kr: x x m; x m → x (x m; m)
oao =
    kl; m ;;
    m x x; a ;;
    (ch0 ;; ch1); m ;;
    x x m; a ;;
    kr; m

pentagon: aaa → oao
result = pentagon
```

## 解説

### 基本構造

```
u: *
x: u → u
m: x x → x
```

- `u` — 0-cell（対象）
- `x` — 1-cell（`u` 上の射）
- `m` — 2-cell（二項演算。`x` を2つ受け取り `x` を返す）

これはモノイドの骨格です。

### 結合律 (associator)

```
a: m x; m → x m; m
```

`a` は 3-cell です。source と target はそれぞれ `x` を3つ合成する2通りの括弧付けを表します:

- `m x; m` — `m(m(a, b), c)` — 左から結合
- `x m; m` — `m(a, m(b, c))` — 右から結合

### interchange cell

```
chl: (x m; m) x → x m x; m x
chr: x m x; x m → x (m x; m)
```

4つの `x` を結合する際に、括弧の位置を入れ替える 3-cell です。

### 2つの経路

`x` を4つ結合するとき、括弧の付け方は5通りあります:

```
((ab)c)d  →  (a(bc))d  →  a((bc)d)  →  a(b(cd))

((ab)c)d  →  (ab)(cd)  →  a(b(cd))
```

完全左結合 `((ab)c)d` から完全右結合 `a(b(cd))` に至る経路が2通りあり、それぞれが `aaa` と `oao` に対応します。

#### aaa: 上の経路（associator を内側から3回）

```
aaa =
    a x; m ;;       -- ((ab)c)d → (a(bc))d  : 左3つに a を適用
    chl; m ;;        -- (a(bc))d → a(bc)d    : interchange
    x m x; a ;;     -- a(bc)d   → a(b(cd))  : 真ん中を右に括り直し（ではなく右3つに a を適用）
    chr; m ;;        -- 括弧の調整
    x a; m           -- 最終調整
```

各ステップは 3-cell で、`;;` による 2次合成で繋いでいます。`a x; m` は「左3つに associator `a` を適用し、残りの `x` は恒等的に通す」ことを意味します。

#### oao: 下の経路（外側から）

```
oao =
    kl; m ;;         -- ((ab)c)d → (ab)(cd)  : 括弧の組み替え
    m x x; a ;;     -- (ab)(cd) → (ab)(cd)  : 右2つに a を適用
    (ch0 ;; ch1); m ;; -- interchange
    x x m; a ;;     -- 左2つに a を適用
    kr; m            -- → a(b(cd))
```

こちらは途中で `(ab)(cd)` という「2つずつに分ける」括弧付けを経由します。

#### pentagon: coherence

```
pentagon: aaa → oao
```

`aaa` と `oao` はどちらも同じ source（完全左結合）と target（完全右結合）を持つ 3-cell です。`pentagon` はこの2つの経路の間の **4-cell** であり、「どちらの経路で括り直しても結果は同じ」という coherence 条件を表現しています。

これが Mac Lane の **pentagon identity** — モノイダル圏の公理の中核です。

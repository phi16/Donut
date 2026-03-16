# 次元の自動リフト

低次元のセルが高次元の位置で使われた場合、自動的に **identity cell** としてリフト（昇格）されます。

## 例

```
u: *
h: u → u = u
```

`u` は 0-cell ですが、`u → u`（1-cell の型）の位置で使われています。この場合 `u` は `u` 上の identity 1-cell（恒等射）として解釈されます。

## 合成での自動リフト

合成の中で次元の異なるセルが混在する場合、低い方が自動的にリフトされます。

```
u: *
x: u → u
μ: x x → x

// μ は 2-cell。x は 1-cell だが、2-cell の位置で使われると id(x) にリフトされる
α: μ x; μ → x μ; μ
```

`μ x` の `x` は 2-cell の位置にいるので、自動的に `id(x)` (identity 2-cell) にリフトされます。

## 仕組み

k-cell が n-cell（n > k）の位置で使われた場合、identity cell で包んで次元を合わせます。リフトは必要な回数だけ自動的に適用されます。

- 0-cell `A` を 1-cell の位置で → `A` 上の identity 1-cell
- 1-cell `x` を 2-cell の位置で → `id(x)`（identity 2-cell）
- 1-cell `x` を 3-cell の位置で → `id(id(x))`（2回リフト）

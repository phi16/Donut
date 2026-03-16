# 宣言と型

## 型宣言（Decl）

名前と型を宣言します。値は自動的に生成されます。

```
u: *
x: u → u
```

`*` は 0-cell の型です。`u → u` は `u` から `u` への 1-cell の型です。

## 値の定義（Alias）

名前に値を束縛します。型は値から推論されます。

```
f = x x
```

明示的な型注釈も可能です:

```
f: u → u = x x
```

## 型エイリアスとしての利用

型を表す値を alias に束縛すると、型として使えます。

```
T = x x → x
m: T           // m: x x → x と同じ
```

パラメトリックな型エイリアスも可能です:

```
Endo[X: *] = X → X
f: Endo[u]     // f: u → u と同じ
```

エイリアスの連鎖も展開されます: `T = x x → x` → `S = T` → `m: S` は `m: x x → x` と解釈されます。

## 型の階層

| 型 | 意味 | 例 |
|---|---|---|
| `*` | 0-cell | `u: *` |
| `A → B` | 1-cell 以上（射） | `x: u → u` |
| `A ~ B` | 等価性 | `e: x ~ y` |
| `A ~> B` | functor | `F: src.C ~> tgt.D` |
| `meta` | メタ値（nat, rat, color 等） | `n: nat` |

### Arrow の次元

Arrow の次元は source/target から自動的に決まります:

```
u: *                  // 0-cell
x: u → u             // 1-cell (0-cell → 0-cell)
μ: x x → x           // 2-cell (1-cell → 1-cell)
α: μ x; μ → x μ; μ   // 3-cell
```

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
f = x x; x
```

明示的な型注釈も可能です:

```
f: x x → x = x x; x
```

## 型の階層

| 型 | 意味 | 例 |
|---|---|---|
| `*` | 0-cell | `u: *` |
| `A → B` | 1-cell 以上（射） | `x: u → u` |
| `A ~ B` | equivalence | `e: x ~ y` |
| `A ~> B` | functor | `F: src.C ~> tgt.D` |
| `meta` | メタ値（nat, rat, color 等） | `n: nat` |

### Arrow の次元

Arrow の次元は source/target から自動的に決まります:

```
u: *            // 0-cell
x: u → u       // 1-cell (0-cell → 0-cell)
m: x x → x     // 2-cell (1-cell → 1-cell)
a: m x; m → x m; m  // 3-cell
```

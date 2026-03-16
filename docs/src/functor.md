# Functor

Functor は2つの構造間のマッピングを定義します。プリミティブの対応だけ指定すれば、合成セルは自動的に変換されます。

## 宣言

`~>` で functor の型を宣言し、各プリミティブの対応を `F(src) = tgt` の形で記述します。

```
nat = {
    C: *
    Nat: C → C
    zero: C → Nat
    succ: Nat → Nat
    add: Nat Nat → Nat
}

u8 = {
    D: *
    U8: D → D
    zero: D → U8
    succ: U8 → U8
    add: U8 U8 → U8
}

compile: nat.C ~> u8.D
compile(nat.Nat)  = u8.U8
compile(nat.zero) = u8.zero
compile(nat.succ) = u8.succ
compile(nat.add)  = u8.add
```

## 自動変換

functor を定義すると、プリミティブセルの合成で作られたセルにも自動的に適用できます。

```
// nat のプリミティブだけで構成されたセル
two = nat.zero; nat.succ; nat.succ

// compile(two) は自動的に u8.zero; u8.succ; u8.succ になる
compiled_two = compile(two)
```

functor はソース構造のプリミティブの合成を再帰的に辿り、各プリミティブを対応するターゲットに置き換えます。ソース構造に属さないプリミティブが含まれている場合はエラーになります。

## functoriality チェック

functor のマッピングは整合性がチェックされます。各マッピング `F(X) = Y` に対して、`X` の source/target を functor で変換した結果が `Y` の source/target と一致するか検証されます。一致しない場合はエラーになります。

## 制約の自動生成

パラメータのセルに functor を適用すると、マッピングが未定義なため即座には解決できません。この場合 **制約** が自動生成されます。

```
src = {
    C: *
    X: C → C
}
tgt = {
    D: *
    Y: D → D
}
F: src.C ~> tgt.D
F(src.X) = tgt.Y

h[m: src.C → src.X] = F(m)
```

`F(m)` の `m` はパラメータであり `F` のマッピングに含まれないので、Donut は `F(m)` の結果を表すフレッシュなセルを生成します。`h` の型は制約付きで表示されます:

```
h[m: src.C → src.X | F(m) = ...]: tgt.D → tgt.Y
```

`|` の後が制約です。制約はユーザーが記述するものではなく、自動的に生成されます。

### 呼び出し時の検証

`h` を具体的なセルで呼び出すとき、制約が検証されます。

```
src += { u: C → X }
tgt += { v: D → Y }
F(src.u) = tgt.v

result = h[src.u]   // OK: F(src.u) = tgt.v が定義されている
```

`F` のマッピングに含まれないセルを渡すとエラーになります。

### 制約の伝搬

制約付きの定義を別の定義から呼ぶと、制約が伝搬します。

```
g[n: src.C → src.X] = h[n]
// g にも h と同じ制約が付く
```

### ネストした functor

functor を合成して適用することもできます。

```
f(g(m))
// g(m) と f(g(m)) の両方に制約が生成される
```

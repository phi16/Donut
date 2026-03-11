# Functor

Functor は2つの構造間のマッピングを定義します。プリミティブの対応だけ指定すれば、合成セルは自動的に変換されます。

## 宣言

`~>` で functor の型を宣言し、各プリミティブの対応を `F(src) = tgt` の形で記述します。

```
import "ui"

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
double = nat.dup; nat.add

// compile(double) は自動的に u8.dup; u8.add になる
compiled_double = compile(double)
```

## functoriality チェック

functor のマッピングは整合性がチェックされます。source/target の境界が一致しない場合はエラーになります。

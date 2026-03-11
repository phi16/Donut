# 例: mandelbrot

`import "sys"` のプリミティブを使って **Mandelbrot 集合** を描画するプログラムです。すべてのロジックが 1-cell の合成として記述されています。

## 全体コード

```
import "sys"

fdup = val.dup[f32]

square :=
  val.dup[f32x2];
  f32x2.unpack f32x2.unpack;
  (fdup; f32.mul) (fdup; f32.mul) (f32.lit[2] f32.mul; f32.mul);
  f32.sub f32;
  f32x2.pack

step: f32x2 f32x2 → f32x2 f32x2 =
  square val.dup[f32x2]; f32x2.add f32x2

rep4[x: C → C, f: x → x]: x → x = f; f; f; f
step4 := rep4[f32x2 f32x2, step]
step16 := rep4[f32x2 f32x2, step4]
step64 := rep4[f32x2 f32x2, step16]

coloring: f32x2 → f32 := f32x2.unpack;
  (fdup; f32.mul) (fdup; f32.mul);
  f32.add f32.lit[2];
  f32.le;
  f32.lit[1] f32.lit[0] bool;
  bool.ind[f32]

result = f32x2.lit[0,0] (f32x2 f32x2.lit[0.6,0.5]; f32x2.sub; f32.lit[3] f32x2; f32x2.scale);
  step64; 
  f32x2 val.drop[f32x2];
  coloring
```

## 解説

### 前提: sys ライブラリ

`import "sys"` により、以下のプリミティブが使えます:

- `f32`, `f32x2` — 浮動小数点型。すべて `C → C` の 1-cell
- `f32.add`, `f32.mul`, `f32.sub` — 算術演算（2つの `f32` を受け取り `f32` を返す 2-cell）
- `f32x2.pack` / `f32x2.unpack` — `f32 f32 ↔ f32x2` の変換
- `val.dup[x]` — `x → x x`（値の複製）
- `val.drop[x]` — `x → C`（値の破棄）

Donut では関数は 2-cell の合成であり、すべてのデータは 1-cell の「ワイヤー」として左から右に流れます。

### fdup: f32 の複製

```
fdup = val.dup[f32]
```

`f32 → f32 f32`。1つの `f32` 値を複製して2つにします。頻出するのでエイリアスしています。

### square: 複素数の二乗

```
square :=
  val.dup[f32x2];
  f32x2.unpack f32x2.unpack;
  (fdup; f32.mul) (fdup; f32.mul) (f32.lit[2] f32.mul; f32.mul);
  f32.sub f32;
  f32x2.pack
```

型は `f32x2 → f32x2`。複素数 \(z = a + bi\) の二乗 \(z^2 = (a^2 - b^2) + 2abi\) を計算します。

データフローを追うと:

1. **`val.dup[f32x2]`** — 入力 `(a, b)` を複製: `(a, b) (a, b)`
2. **`f32x2.unpack f32x2.unpack`** — 展開: `a b a b`
3. **`(fdup; f32.mul) (fdup; f32.mul) (f32.lit[2] f32.mul; f32.mul)`**
   - 最初の2つ: `a` を複製して `a*a`、`b` を複製して `b*b`
   - 3つ目: 定数 `2` と `a` と `b` を掛けて `2*a*b`
   - 結果: `a² b² 2ab`
4. **`f32.sub f32`** — `a² - b²` と `2ab`
5. **`f32x2.pack`** — `(a² - b², 2ab)` にパック

`:=` で定義しているので、`square` は独立したセルとして宣言されつつ、評価時にはこの合成として計算されます。

### step: Mandelbrot の反復ステップ

```
step: f32x2 f32x2 → f32x2 f32x2 =
  square val.dup[f32x2]; f32x2.add f32x2
```

型は `f32x2 f32x2 → f32x2 f32x2`。入力は `(z, c)` で、\(z \leftarrow z^2 + c\) を計算して `(z', c)` を返します。

1. **`square val.dup[f32x2]`** — `z` に `square` を適用して `z²` にし、`c` を複製: `z² c c`
2. **`f32x2.add f32x2`** — `z² + c` と `c`: `(z², c)`

`c` を保持したまま `z` を更新するので、繰り返し適用できます。

### rep4 と反復

```
rep4[x: C → C, f: x → x]: x → x = f; f; f; f
step4 := rep4[f32x2 f32x2, step]
step16 := rep4[f32x2 f32x2, step4]
step64 := rep4[f32x2 f32x2, step16]
```

`rep4` はパラメトリックな定義で、任意の 2-cell `f: x → x` を4回繰り返します。

- `step4` = step を 4 回 = 4 回反復
- `step16` = step4 を 4 回 = 16 回反復
- `step64` = step16 を 4 回 = 64 回反復

`:=` で定義しているので、それぞれ独立したセルとして扱いつつ、評価時には展開されます。

### coloring: 発散判定

```
coloring: f32x2 → f32 := f32x2.unpack;
  (fdup; f32.mul) (fdup; f32.mul);
  f32.add f32.lit[2];
  f32.le;
  f32.lit[1] f32.lit[0] bool;
  bool.ind[f32]
```

型は `f32x2 → f32`。最終的な `z` の値から色を決定します。

1. **`f32x2.unpack`** — `(a, b)` を `a b` に展開
2. **`(fdup; f32.mul) (fdup; f32.mul)`** — `a²` と `b²` を計算
3. **`f32.add f32.lit[2]`** — `a² + b²` と定数 `2`
4. **`f32.le`** — `a² + b² ≤ 2` → `bool`
5. **`f32.lit[1] f32.lit[0] bool`** — 定数 `1`、定数 `0`、判定結果を並べる
6. **`bool.ind[f32]`** — `bool` の値で分岐。真なら `1`（集合内）、偽なら `0`（発散）

### result: 全体の組み立て

```
result = f32x2.lit[0,0] (f32x2 f32x2.lit[0.6,0.5]; f32x2.sub; f32.lit[3] f32x2; f32x2.scale);
  step64; 
  f32x2 val.drop[f32x2];
  coloring
```

型は `C → f32`。ピクセル座標 `f32x2`（入力）を受け取り、色 `f32` を返します。

1. **初期値の準備**: `z = (0, 0)` と `c` を用意。`c` は入力座標を `(0.6, 0.5)` を中心に、スケール `3` で変換したもの
2. **`step64`** — 64 回反復
3. **`f32x2 val.drop[f32x2]`** — `c` を捨てて `z` だけ残す
4. **`coloring`** — 発散判定して色に変換

## ポイント

- すべてのロジックが **2-cell の合成** で表現されている
- 分岐（if/else）は `bool.ind` で実現
- ループは `rep4` のパラメトリック定義を入れ子にして実現
- `:=`（定義）を使うことで、中間セルに名前を付けつつ評価時は展開

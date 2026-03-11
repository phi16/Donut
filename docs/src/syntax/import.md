# import と use

組み込みライブラリを読み込むための2つの方法があります。

## import

`import` はライブラリの中身をスコープに読み込みます。

```
import "sys"

// f32, f32.add 等が直接使える
x = f32.lit[1]
```

名前を付けて import すると、モジュールとしてスコープに入ります:

```
s = import "sys"

// s.f32, s.f32.add 等でアクセス
x = s.f32.lit[1]
```

## use

`use` も同様にライブラリの中身をスコープに展開します。

```
use "base"

// nat, rat 等が直接使える
n: nat = 42
```

## import と use の違い

`import` と `use` はどちらも無名で使うとライブラリの中身をスコープに展開します。違いは origin の扱いです。`import` で読み込まれた宣言はライブラリ由来としてマークされ、donut-app のセル一覧には表示されません。`use` で読み込まれた宣言にはそのようなマークが付きません。

## 組み込みライブラリ一覧

| ライブラリ | 内容 |
|---|---|
| `"sys"` | プリミティブ型と演算（`f32`, `u32`, `bool`, `val.dup` 等） |
| `"base"` | メタ型の定義（`nat`, `rat`, `color`, `decorator`） |
| `"ui"` | デコレータ関連（`rgb`, `hsv`, `hue`, `gray`, `style` 等） |

`"ui"` は内部で `"base"` を `use` しているため、`import "ui"` だけで `nat` や `rat` も使えます。

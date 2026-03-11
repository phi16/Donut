# デコレータ

デコレータは宣言の表示スタイルを制御します。宣言の前に `[...]` で記述します。

使用するには `import "ui"` が必要です。

## 色の指定

```
import "ui"

// HSV で指定
[style.color[hsv[0.6, 1, 1]]]
x: u → u

// RGB で指定
[style.color[rgb[255, 0, 128]]]
m: x x → x

// グレースケール
[style.color[gray[80]]]
u: *

// 色相のみ指定（彩度・明度は自動）
[style.color[hue[0.33]]]
y: u → u
```

## 自動カラー

デコレータを指定しない場合、自動的に色が割り当てられます。色はセルの次元と宣言順に基づいて決まります。

## Lighten / Darken

自動色をベースに明るさを調整できます。

```
[style.color[lighten[0.3]]]
a: x → x

[style.color[darken[0.2]]]
b: x → x
```

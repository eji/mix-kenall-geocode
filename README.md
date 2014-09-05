mix-kenall-geocode
======================

## 概要

このプログラムは、日本郵便のKEN_ALL.CSVと国土交通省国土政策局国土情報課の位置情報CSVから
郵便番号と位置情報を組み合わせてTableauに食わせるためのCSVを出力します。

## 使い方

```
mix-kenall-geocode KEN_ALL.CSV 01_2012.csv
```

標準出力に以下のフォーマットでCSVが出力されます。

```
国名,都道府県名,市町村名,郵便番号(Postal Code),緯度,経度
```



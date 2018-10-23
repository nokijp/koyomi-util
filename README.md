# Koyomi

日本の暦を扱うためのコマンドラインツールです。

主な機能は以下の通りです。

- 旧暦
- 祝日判定
- 六曜計算


## インストール方法

```bash
$ git clone https://github.com/nokijp/koyomi-util.git
$ cd koyomi-util
$ stack install 
```

ビルドには [Stack](https://www.haskellstack.org/) が必要です。

また、`~/.local/bin` にパスを通す必要があります。


## 主な使い方

ここにない使い方は `--help` を参照してください。`koyomi kyureki --help` のように、各サブコマンドでも確認できます。

| 使い方 | 説明 |
|---|---|
| `koyomi` | `koyomi day` と同じ |
| `koyomi day` | 暦の情報を表示する |
| `koyomi day 2000/01/01` | 特定の日付の暦の情報を表示する |
| `koyomi kyureki` | 旧暦を表示する |
| `koyomi kyureki 2000/01/01` | 特定の日付の旧暦を表示する |
| `koyomi kyureki -f '%y-%02M-%02d'` | フォーマットを指定して旧暦を表示する（詳細は後述） |
| `koyomi holiday` | 祝日であれば、その名前を表示する |
| `koyomi holiday 2000/01/01` | 特定の日付の祝日を調べる |
| `koyomi holiday --exit-code` | 祝日かどうかを終了コードで返す |
| `koyomi holiday -w` | 土日の場合も出力する（祝日優先） |
| `koyomi rokuyo` | 今日の六曜を表示する |
| `koyomi rokuyo 2000/01/01` | 特定の日付の六曜を表示する |
| `koyomi sekki` | 今日の二十四節気を表示する |
| `koyomi sekki 2000/01/01` | 特定の日付の二十四節気を表示する |

### 旧暦フォーマット

| 指定子 | 説明 | 出力例 |
|---|---|---|
| `%y` | 年 | `"2000"` |
| `%L` | 閏月の場合のみ `"閏"` を出力 | `""`、`"閏"` |
| `%m` | 月 | `"1"`、`"12"` |
| `%M` | `%L%m` と同じ | `"閏10"` |
| `%b` | 月の日本語名 | `"神無月"` |
| `%B` | `%L%b` と同じ | `"閏神無月"` |
| `%d` | 日 | `"1"`、`"31"` |
| `%2m` | 空白補完 | `" 1"` |
| `%02m` | ゼロ補完 | `"01"` |

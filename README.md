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

| コマンド | 動作 |
|---|---|
| `koyomi` | `koyomi day` と同じ |
| `koyomi day [DATE]` | 暦の情報を表示する |
| `koyomi kyureki [DATE]` | 旧暦を表示する |
| `koyomi kyureki -f '%y-%02M-%02d'` | フォーマットを指定して旧暦を表示する（詳細は後述） |
| `koyomi holiday [DATE]` | 祝日であれば、その名前を表示する |
| `koyomi holiday --exit-code` | 祝日かどうかを終了コードで返す |
| `koyomi holiday -w` | 土日の場合も出力する（祝日優先） |
| `koyomi rokuyo [DATE]` | 六曜を表示する |
| `koyomi sekki [DATE]` | 二十四節気を表示する |

`[DATE]` は以下のように利用できます。

| `[DATE]` | 動作 |
|---|---|
| 指定なし | 今日（日本標準時）の情報を表示する |
| `2000/01/01` | 2000年1月1日の情報を表示する |
| `2000/01` | 2000年1月の情報を表示する |
| `2000` | 2000年の情報を表示する |

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

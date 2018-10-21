# Koyomi

日本の暦を扱うためのコマンドラインツールです。

主な機能は以下の通りです。

- 祝日判定
- 六曜計算


## インストール方法

```bash
$ git clone https://github.com/nokijp/koyomi-util.git
$ cd koyomi-util
$ stack install 
```

ビルドには [Stack](https://www.haskellstack.org/) が必要です。

`~/.local/bin` にパスを通すのも忘れないでください。


## 主な使い方

ここにない使い方は `--help` を参照してください。`koyomi holiday --help` のように、各サブコマンドでも確認できます。

| 使い方 | 説明 |
|---|---|
| `koyomi holiday` | 今日が祝日であれば、その名前を表示する |
| `koyomi holiday 2000/01/01` | 特定の日付の祝日を調べる |
| `koyomi holiday --exit-code` | 祝日かどうかを終了コードで返す |
| `koyomi rokuyo` | 今日の六曜を表示する |
| `koyomi rokuyo 2000/01/01` | 特定の日付の六曜を表示する |

- gmnlisp を [v0.7.16] へ更新
- gmnlisp [v0.7.15] でオプションになった `(eval)` と `(load)` を有効にするため、`import _ "github.com/hymkor/gmnlisp/eval"` を追加した
- `*executable-name*` にsmake の実行ファイルのパスを設定するようにした ([lispect](https://github.com/hymkor/lispect) と互換)
- 条件分岐と let を組み合わせたマクロ `(if-some)`, `(when-some)`, `(cond-let)` を追加
- 環境変数PATHで指定されるディレクトリリストから実行ファイルを検索する関数 `(which)` を追加
- `(spawn)`, `(sh)`: 実行前に標準出力、標準エラー出力を Flush するようにした

[v0.7.15]: https://github.com/hymkor/gmnlisp/releases/tag/v0.7.15
[v0.7.16]: https://github.com/hymkor/gmnlisp/releases/tag/v0.7.16

v0.5.1
======
Jun 10, 2025

- gmnlisp を [v0.7.14] へ更新

[v0.7.14]: https://github.com/hymkor/gmnlisp/releases/tag/v0.7.14

v0.5.0
======
May 15, 2025

- `(getenv "OS")` が `"Windows_NT"` と一致する時 `T` となる変数 `*windows*` を追加
- Windows では `"NUL"`, それ以外の OS では `"/dev/null"` となる変数 `*dev-null*` を追加
- Windows では`".exe"` 、それ以外の OS では `""` となる変数 `*exe-suffix*` を追加
- パスの区切り文字として `*path-separator*`、 パスリストの区切り文字として `*path-list-separator*` を追加
- `spawn` を Go で、`x`,`spawnlp`, `spawnvp` を Lisp で再実装した
- `probe-directory` を実装
- `joinpath` や `pathjoin` を非推奨とし、かわりに使う `join-path` を追加
- `(sh-)`  と同様に機能する `sh-ignore-error` を追加。`(sh-)` は非推奨とした
- gmnlisp を [v0.7.11] へ更新

[v0.7.11]: https://github.com/hymkor/gmnlisp/releases/tag/v0.7.11

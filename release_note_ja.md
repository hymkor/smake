v0.8.0
======
Jul 21, 2025

- os.Stderr となっていた `(sh)`, `(make)`, `(chdir)`, `(touch)` のメッセージの出力先を `(error-output)` の設定先とした。
- 環境変数 PATH のディレクトリリストを検索して最初に見付かった実行ファイルのフルパスを得る `(lookpath)` を実装した ( Go製なので、Lisp版の `(which)` より速いが、最初に見付かったものしか列挙しない)
- 最後に評価した値を表示するオプション `-p` を追加
- gmplisp を [v0.7.19] へ更新

[v0.7.19]: https://github.com/hymkor/gmnlisp/releases/tag/v0.7.19

v0.7.0
======
Jul 7, 2025

- `-f` オプションでカンマで区切った複数のルールファイルの指定をサポート. デフォルトは `"Makefile.lsp,smake.lsp"` 
- `(spawn ...)` によって発生した状態を識別するための関数 `(executable-not-found-p)`, `(exit-error-p)`, `(exit-code)` を追加
- `(ansi-to-utf8)` を実装
- `(spawn)` や `(sh)` で標準出力や標準エラー出力を `(standard-output)` や `(error-output)` へリダイレクトするようにした
- Go で実装していた `(q)`, `(shell)`, `(sh-ignore-error)` を Lisp で作り直した
- `(file-for-each)`, `(which)`, `(updatep)`, `(probe-directory)` の実装を改善
- すべての書き込みを破棄するグローバルな出力ストリーム出力先として `*discard*` を実装
- gmnlisp を [v0.7.17] へ更新

[v0.7.17]: https://github.com/hymkor/gmnlisp/releases/tag/v0.7.17

v0.6.0
======
Jun 24, 2025

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

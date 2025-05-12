SMake: Make Powered by S-expressions
====================================

SMake is a build tool similar to `make` on UNIX systems, but it uses S-expressions for its Makefile syntax.

### Example Usage

```
$ smake clean
chdir "examples/cc"
C:\Users\hymkor\src\smake\smake.exe clean
chdir "C:\Users\hymkor\src\smake"
rm ".smake.exe~"
mv "smake.exe" ".smake.exe~"

$ go run github.com/hymkor/smake@latest
Found update files: ("orig_test.go" "orig.go" "match.go" "main_windows.go" "main_unix.go" "main.go" "io.go" "gnu_test.go" "gnu.go" "go.sum" "go.mod" "embed.lsp" "Makefile.lsp")
go fmt
go build -ldflags -s -w -X main.version=v0.4.2-6-g940b278
```

### Key Features

- **Build Rules in Lisp:**  
    The Makefile for smake is written entirely in an extended version of ISLisp, providing a powerful and flexible structure for defining build rules. 
- **Execution Behavior:**  
    `smake` reads and executes Makefile.lsp in the current directory.
- **No Installation Required:**  
    If Go is available, you can use smake without installation via `go run github.com/hymkor/smake@latest`

[Makefile.lsp](./Makefile.lsp):

```Makefile.lsp
(defglobal EXE     (shell "go env GOEXE"))
(defglobal CURDIR  (getwd))
(defglobal NAME    (notdir CURDIR))
(defglobal TARGET  (string-append NAME EXE))
(defglobal SOURCE  (wildcard "*.go"))
(defglobal NUL     (if windows "NUL" "/dev/null"))
(defglobal VERSION
  (catch
    'notag
    (with-handler
      (lambda (c) (throw 'notag "v0.0.0"))
      (shell (string-append "git describe --tags 2>" NUL)))))

(case $1
  (("get")
   (sh "go get -u"
       "go get -u github.com/hymkor/gmnlisp@master"
       "go mod tidy"))

  (("touch")
   (dolist (fname SOURCE)
     (touch fname)))

  (("clean")
   (pushd "examples/cc"
     (spawnlp $0 "clean"))
   (dolist (fname (wildcard "*~"))
     (rm fname))
   (if (-e TARGET)
     (mv TARGET (string-append "." TARGET "~"))))

  (("upgrade") ; upgrade the installed program with the newly built version
   (if (probe-file TARGET)
     (let ((delimiter (elt (if windows ";" ":") 0)))
       (dolist (dir (string-split delimiter (getenv "PATH")))
         (if (and (not (equalp CURDIR dir))
                  (probe-file (joinpath dir TARGET)))
           (progn
             (format (standard-output) "copy \"~A\" to \"~A\" ? [Y or N] " TARGET dir)
             (if (equalp (read-line (standard-input) nil nil) "y")
               (cp TARGET dir))))))))

  (("test")
   (assert-eq (match "^a*$" "aaaa") '("aaaa"))
   (assert-eq (match "^v([0-9]+)\.([0-9]+)\.([0-9]+)$" "v10.20.30")
              '("v10.20.30" "10" "20" "30"))
   (assert-eq (match "^a*$" "hogehoge") nil)
   (assert-eq (catch
                'fail
                (with-handler
                  (lambda (c) (throw 'fail 'NG))
                  (match "(" "hogehoge")))
              'NG)
   (sh "go test"))

  (("dist")
   (dolist (goos '("linux" "windows"))
     (dolist (goarch '("386" "amd64"))
       (env (("GOOS" goos) ("GOARCH" goarch))
         (let* ((exe (shell "go env GOEXE"))
                (target (string-append NAME exe)))
           (rm target)
           (sh "go build")
           (spawnlp
             "zip"
             (string-append NAME "-" VERSION "-" goos "-" goarch ".zip")
             target))))))

  (("release")
   (let ((b (create-string-output-stream)))
     (format b "gh release create -d --notes \"\" -t")
     (format b " \"~A\"" VERSION)
     (format b " \"~A\"" VERSION)
     (dolist (zip (wildcard (string-append NAME "-" VERSION "-*.zip")))
       (format b " \"~A\"" zip))
     (sh (get-output-stream-string b))))

  (("clean-zip")
   (dolist (fname (wildcard "*.zip"))
     (rm fname)))

  (("manifest")
   (sh "make-scoop-manifest *.zip > smake.json"))

  (("readme")
   (sh "example-into-readme"))

  (t
    (let ((ufiles (updatep TARGET "Makefile.lsp" "embed.lsp" "go.mod" "go.sum" SOURCE)))
      (if ufiles
        (progn
          (format (error-output) "Found update files: ~S~%" ufiles)
          (sh "go fmt")
          (spawnlp "go" "build" "-ldflags"
                   (string-append "-s -w -X main.version=" VERSION)))
        (progn
          (format (error-output) "No files updated~%")
          )
        ); if
      ); let
    ); t
  ); case

; vim:set lispwords+=env,mapc,pushd,while,doenv:
```

Other examples:

- [examples/cc/Makefile.lsp](./examples/cc/Makefile.lsp) for C Project

Install
-------

Download the zipfile for your environment from [Releases](https://github.com/hymkor/smake/releases) and unzip.

### Use Go-installer

```
go install github.com/hymkor/smake@latest
```

### Use scoop-installer

```
scoop install https://raw.githubusercontent.com/hymkor/smake/master/smake.json
```

or

```
scoop bucket add hymkor https://github.com/hymkor/scoop-bucket
scoop install smake
```

## How to build SMake

```
go build
```

## Lisp References

### Base interpreter

+ [hymkor/gmnlisp](https://github.com/hymkor/gmnlisp)

### ISLisp Documents (English)

+ [ISLISP - Wikipedia](https://en.wikipedia.org/wiki/ISLISP)
+ [ISLisp Home Page][ISLisp]

[ISLisp]: http://islisp.org/

### ISLisp Documents (Japanese)

+ [JISX3012:1998 プログラム言語ＩＳＬＩＳＰ](https://kikakurui.com/x3/X3012-1998-01.html)
+ [M.Hiroi's Home Page / お気楽 ISLisp プログラミング超入門](http://www.nct9.ne.jp/m_hiroi/clisp/islisp.html)

## The functions available in Makefile.lsp

### (updatep TARGET SOURCES...)

It returns the list of newer files in SOURCES than TARGET

### (spawnlp "COMMAND" "ARG-1" "ARG-2" ...)
### (spawnvp "COMMAND" '("ARG-1" "ARG-2" ...))

Execute the external executable directly. If it failes, top.

### (q "COMMAND" "ARG-1" "ARG-2" ...)

Execute the external executable directly and return its standard-output as string.

### (sh "SHELL-COMMAND")

Execute the shell command by CMD.exe or /bin/sh. If it fails, stop.

### (sh- "SHELL-COMMAND")

Same as (sh) but ignores errors.

### (shell "SHELL-COMMAND")

Execute the shell command and return its standard-output as string.
Same as $(shell "..") of GNU Make.

### (echo STRING...)

Same as the UNIX command echo.

### (rm FILENAME...)

Same as the UNIX command rm.

### (touch FILENAME...)

Same as the UNIX command touch.

### (dolist (KEY '(VALUE...)) COMMANDS...)

### (getenv "NAME")

Return the value of the environment variable NAME. If it does not exist, return nil.

### (setenv "NAME" "VALUE")

Set the environment variable "NAME" to "VALUE".

### (env (("NAME" "VALUE")...) COMMANDS...)

Set the environment variables and execute COMMANDS.
Then, restores them to thier original values.

### (wildcard "PATTERN"...)

Expand PATTERNs as a wildcard and return them as a list.

### (abspath "FILEPATH")

Same as $(abspath FILEPATH) of GNU Make

### (dir "FILEPATH")

Same as $(dir FILEPATH) of GNU Make

### (notdir "FILEPATH")

Same as $(notdir FILEPATH) of GNU Make

### (basename "FILEPATH")

Same as $(basename FILEPATH) of GNU Make

### (pathjoin "DIR" .. "FNAME") , (joinpath "DIR"... "FNAME")

Make path with "DIR"... "FNAME".
(joinpath) is an alias of (pathjoin).
Same as filepath.Join of golang.

### (-e FILENAME)

If FILENAME exists, it returns t. Otherwise nil.
Same as -e of Perl.

### (-d DIRNAME)

If DIRNAME exists and it is a directory, it returns t. Otherwise nil.
Same as -d of Perl.

### (chdir "DIRNAME")

Change the current working directory to "DIRNAME"

### (getwd)

Returns the current working directory.

### (pushd "DIRNAME" COMMANDS)

Change the current directory to "DIRNAME" and execute COMMANDS like (progn).
After COMMANDS, return to the original current directory.

### (cp SRC... DST)

Copy file SRC... to DST (directory or new filename)

### (mv SRC... DST)

Move file SRC... to DST (directory or new filename)

### (string-split SEP SEQUENCE)

`(string-split #\: "a:b:c")` =&gt; `("a" "b" "c")`

### (shellexecute "ACTION" "PATH" \["PARAM"\] \["DIRECTORY"\])

Call Windows-API: shellexecute

### (string-fields "STRING")

Split "STRING" with whilte-spaces. This function is similar with [strings.Fields](https://pkg.go.dev/strings@go1.20.1#Fields) in golang

### (let), (format) and so on

They are compatible functions with ISLisp. See also [hymkor/gmnlisp](https://github.com/hymkor/gmnlisp)

### windows

It is `t` (true) when %OS% is `Windows_NT`

### (match REGULAR-EXPRESSION STRING)

If `REGULAR-EXPRESSION` matches `STRING`, `(match)` returns a list containing the entire matched part followed by the captured groups (submatches). If there is no match, it returns `nil`.

## The built-in variables

- $/ - OS-specific path separator (Windows `\` , UNIX `/` )
- \*args\* - the command-line arguments
- $1...$9 - the same as `(elt *args* N)`
- $0 ... the current executing smake filename

These are available in `(make)` block

- $@ - the target filename
- $&lt; - the first source filename
- $? - the updated source filenames

License
-------

MIT License

Author
------

HAYAMA\_Kaoru (hymkor)

---

- [Deprecated functions](./deprecated.md)

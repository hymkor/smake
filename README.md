SMake: Make Powered by S-expressions
====================================

SMake is a build tool similar to `make` on UNIX systems, but it uses S-expressions for its Makefile syntax.

Why smake?
----------

Makefiles written for GNU Make often depend heavily on `/bin/sh` or other Unix-specific shell features, making it difficult to write portable build scripts that run the same on both Windows and Linux.

**smake** is a minimal build tool that avoids this problem by embedding all behavior in a Lisp dialect (ISLisp-like). All build logic is expressed using built-in functions, avoiding external shell commands and Unix-specific syntax.

This approach enables truly cross-platform builds without writing platform-specific conditionals, and without assuming Unix-like tools on Windows.

Example Usage
-------------

### Using `smake` (Installed)

```
> cd "./examples/cc"

> smake
gcc -c main.c
gcc -c sub.c
gcc -o cc.exe main.o sub.o

> smake clean
rm "main.o"
rm "sub.o"
rm "cc.exe"
```

### Using `smake` (via `go run`)

```
> cd "./examples/cc"

> go run github.com/hymkor/smake@latest
go: downloading github.com/hymkor/smake v0.5.0
gcc -c main.c
gcc -c sub.c
gcc -o cc.exe main.o sub.o

> go run github.com/hymkor/smake@latest clean
rm "main.o"
rm "sub.o"
rm "cc.exe"
```

[examples/cc/Makefile.lsp](./examples/cc/Makefile.lsp):

```examples/cc/Makefile.lsp
;; # Equivalent Makefile for GNU Make (for reference)
;; ifeq ($(OS),Windows_NT)
;;   EXE=.exe
;; else
;;   EXE=
;; endif
;; AOUT=$(notdir $(CURDIR))$(EXE)
;; OFILES=$(subst .c,.o,$(wildcard *.c))
;; $(AOUT): $(OFILES)
;;     gcc -o $@ $(OFILES)
;; .c.o:
;;     gcc -c $<

(defun c-to-o (c) (string-append (basename c) ".o"))

(defglobal c-files (wildcard "*.c"))
(defglobal o-files (mapcar #'c-to-o c-files))
(defglobal target  (string-append (notdir (getwd)) *exe-suffix*))

(case $1
  (("clean")
   (dolist (obj o-files)
     (if (probe-file obj)
       (rm obj)))
   (if (probe-file target)
     (rm target)))

  (t
    (dolist (c-src c-files)
      (if (updatep (c-to-o c-src) c-src)
        (spawn "gcc" "-c" c-src)))
    (apply #'spawn "gcc" "-o" target o-files))
  ) ; case

; vim:set lispwords+=apply,make:
```

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

How to build SMake
------------------

```
go build
```

Lisp References
---------------

### Base interpreter

+ [hymkor/gmnlisp](https://github.com/hymkor/gmnlisp)

### ISLisp Documents (English)

+ [ISLISP - Wikipedia](https://en.wikipedia.org/wiki/ISLISP)
+ [ISLisp Home Page][ISLisp]

[ISLisp]: http://islisp.org/

### ISLisp Documents (Japanese)

+ [JISX3012:1998 プログラム言語ＩＳＬＩＳＰ](https://kikakurui.com/x3/X3012-1998-01.html)
+ [M.Hiroi's Home Page / お気楽 ISLisp プログラミング超入門](http://www.nct9.ne.jp/m_hiroi/clisp/islisp.html)

Information about ISLisp is still limited, but if you're looking for more insights, ChatGPT can be surprisingly knowledgeable and might provide helpful answers. Feel free to give it a try.

The functions available in Makefile.lsp
---------------------------------------

### (updatep TARGET SOURCES...)

It returns the list of newer files in SOURCES than TARGET

### (spawn "COMMAND" "ARG-1" "ARG-2" ...)

Execute the external executable directly. If it fails, the process will stop with an error.

### (q "COMMAND" "ARG-1" "ARG-2" ...)

Execute the external executable directly and return its standard-output as string.

### (sh "SHELL-COMMAND")

Execute the shell command by CMD.exe or /bin/sh. If it fails, the build process stops with an error.

### (sh-ignore-error "SHELL-COMMAND")

Equivalent to `(sh)` but ignores errors.

### (shell "SHELL-COMMAND")

Execute the shell command and return its standard-output as string.
Equivalent to `$(shell "..")` of GNU Make.

### (echo STRING...)

Equivalent to the UNIX echo command.

### (rm FILENAME...)

Equivalent to the UNIX rm command.

### (touch FILENAME...)

Equivalent to the UNIX touch command.

### (dolist (KEY '(VALUE...)) COMMANDS...)

### (getenv "NAME")

Return the value of the environment variable NAME. If it does not exist, return nil.

### (setenv "NAME" "VALUE")

Set the environment variable `"NAME"` to `"VALUE"`.

### (env (("NAME" "VALUE")...) COMMANDS...)

Set the environment variables and execute COMMANDS.
Then, restores them to their original values.

### (wildcard "PATTERN"...)

Expand PATTERNs as a wildcard and return them as a list.

### (abspath "FILEPATH")

Equivalent to `$(abspath FILEPATH)` of GNU Make

### (dir "FILEPATH")

Equivalent to `$(dir FILEPATH)` of GNU Make

### (notdir "FILEPATH")

Equivalent to `$(notdir FILEPATH)` of GNU Make

### (basename "FILEPATH")

Equivalent to `$(basename FILEPATH)` of GNU Make

### (join-path "DIR" .. "FNAME")

Make path with "DIR"... "FNAME".

### (probe-file FILENAME)

If FILENAME exists, it returns t. Otherwise nil.
Equivalent to `-e FILENAME` of Perl.

### (probe-directory DIRNAME)

If DIRNAME exists and it is a directory, it returns t. Otherwise nil.
Equivalent to `-d FILENAME` of Perl.

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

Split "STRING" with white-spaces. This function is similar with [strings.Fields](https://pkg.go.dev/strings@go1.20.1#Fields) in golang

### (let), (format) and so on

These are standard functions compatible with ISLisp. See also [hymkor/gmnlisp](https://github.com/hymkor/gmnlisp)

### (match REGULAR-EXPRESSION STRING)

If `REGULAR-EXPRESSION` matches `STRING`, `(match)` returns a list containing the entire matched part followed by the captured groups (submatches). If there is no match, it returns `nil`.

### (if-some (VAR EXPR) THEN ELSE)

Evaluate `EXPR` and bind the result to `VAR`.
If the result is not `nil`, execute the THEN expression.
Otherwise, execute the ELSE expression.
Note: only **one expression** is allowed for both THEN and ELSE.
Also, `VAR` is visible in both THEN and ELSE parts.

Example:

```lisp
(if-some (val (getenv "CONFIG"))
  (format t "Config is: ~A~%" val)
  (format t "No config found.~%"))
```

The built-in variables
----------------------

### \*windows\*

Evaluates to `t` when the environment variable `%OS%` is `"Windows_NT"` (i.e., on Windows).

### \*dev-null\*

Evaluates to `"NUL"` on Windows, and to `"/dev/null"` on other systems.

### \*exe-suffix\*

The file extension used for executables on the current operating system (e.g., `".exe"` on Windows; empty string on Unix).

### \*argv\* (formerly \*args\*)

The command-line arguments passed to the program.

### $0, $1, $2, .. $9

Equivalent to `(and (<= N (length *args*)) (elt (cons "path-to-smake" *args*) N))` where `"path-to-smake"` is the full path to the SMake executable.

### \*path-separator\*

OS-specific path separator (Windows `"\"` , UNIX `"/"` )

### \*path-list-separator\*

OS-specfic path list separator (Windows `";"`, UNIX `":"`)

License
-------

MIT License

Author
------

HAYAMA\_Kaoru (hymkor)

---

- [Deprecated functions](./deprecated.md)

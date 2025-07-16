;; experimantal scripts
(load "smake-lake.lsp")

(defun c-to-o (c) (string-append (basename c) ".o"))

(defglobal c-files (wildcard "*.c"))
(defglobal o-files (mapcar #'c-to-o c-files))
(defglobal target  (string-append (notdir (getwd)) *exe-suffix*))

(task "clean"
      ()
      "clean-files"

      (mapc (lambda (obj) (if (probe-file obj) (rm obj))) o-files)
      (if (probe-file target) (rm target)))

(file target ("main.o" "sub.o")
      "link"
      (apply #'spawn "gcc" "-o" target o-files))

(file "main.o" ("main.c")
      "compile"
      (spawn "gcc" "-c" "main.c"))

(file "sub.o" ("sub.c")
      "compile"
      (spawn "gcc" "-c" "sub.c"))

(lake (or $1 target))

;;; match test ;;;

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

;;; if-some/when-some/cond-let test ;;;

(assert-eq (if-some
             (c (+ 1 2)) ; test-form
             (+ c 3) ; then-form
             0) ; else-form
           6) ; expect
(assert-eq (if-some
             (c nil) ; test-form
             (+ c 3) ; then-form
             "NONE") ;else-form
           "NONE") ; expect
(assert-eq (when-some
             (c (string-append "1" "2")) ; test-form
             (string-append c "3")); then-form
           "123"); expect
(assert-eq (when-some
             (c nil) ; test-form
             (string-append c "3")); then-form
           nil); expect
(mapc
  (lambda (pattern)
    (let ((target (car pattern))
          (expect (cadr pattern)))
      (assert-eq
        (cond-let
          ((foo (elt target 0)) 'first)
          ((bar (elt target 1)) 'second)
          (t 'otherwise))
        expect)))
  '(((1 nil) first) ((nil 2) second) ((nil nil) otherwise)))

;;; spawn error test

(defun spawn-error-handler
  (c)
  (cond
  ((executable-not-found-p c)
   (throw 'spawn-error 'not-found))
  ((exit-error-p c)
   (throw 'spawn-error (exit-code c)))
  (t
    (signal-condition c nil))))

(assert-eq
  (catch
    'spawn-error
    (with-handler #'spawn-error-handler
      (spawn "not-exist-command")
      'NG))
  'not-found)

(assert-eq
  (catch
    'spawn-error
    (with-handler #'spawn-error-handler
      (sh "not-exist-command")
      'NG))
  (if *windows* 1 127))

(assert-eq
  (catch
    'spawn-error
    (with-handler #'spawn-error-handler
      (sh-ignore-error "not-exist-command")
      'NG))
  'NG)

(assert-eq
  (catch
    'spawn-error
    (with-handler #'spawn-error-handler
      (if *windows*
        (spawn "cmd" "/c" "exit 77")
        (spawn "sh" "-c" "exit 77"))
      'NG))
  77)

(assert-eq
  (catch
    'spawn-error
    (with-handler #'spawn-error-handler
      (sh-ignore-error "exit 77")
      'NG))
  'NG)

(defglobal *newline*
  (if *windows*
    (string-append (create-string 1 #\return ) (create-string 1 #\linefeed))
    (create-string 1 #\linefeed)))

(assert-eq
  (let ((B (create-string-output-stream)))
    (with-standard-output
      B
      (if *windows*
        (spawn "cmd" "/c" "echo あはは")
        (spawn "echo" "あはは")))
    (ansi-to-utf8 (get-output-stream-string B)))
  (string-append "あはは" *newline*))

(assert-eq
  (let ((B (create-string-output-stream)))
    (with-standard-output
      B
      (sh "echo あはは"))
    (ansi-to-utf8 (get-output-stream-string B)))
  (string-append "あはは" *newline*))

(assert-eq
  (let ((B (create-string-output-stream)))
    (with-standard-output
      B
      (sh-ignore-error "echo あはは"))
    (ansi-to-utf8 (get-output-stream-string B)))
  (string-append "あはは" *newline*))

(assert-eq (probe-directory ".") t)
(assert-eq (probe-directory "not-exist") nil)
(assert-eq (probe-directory *executable-name*) nil)

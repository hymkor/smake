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
(dolist (pattern '(((1 nil) first) ((nil 2) second) ((nil nil) otherwise)))
  (let ((target (car pattern))
        (expect (cadr pattern)))
    (assert-eq
      (cond-let
        ((foo (elt target 0)) 'first)
        ((bar (elt target 1)) 'second)
        (t 'otherwise))
      expect)))

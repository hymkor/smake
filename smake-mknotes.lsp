(defun make-release-notes (w)
  (labels
    ((read-changes
       (in)

       (let ((line nil) (flag nil) (result nil))
         (block line-loop
           (while (setq line (read-line in nil nil))
             (if (match "^v\\d+\\.\\d+\\.\\d+" line)
               (if flag
                 (return-from line-loop result)
                 (setq flag t))
               )
             (if flag
               (setq result (append result (list line))))))))
     (dump
       (L w)

       (while L
         (format w "~A~%" (car L))
         (setq L (cdr L))))

     (recent-changes
       (fnames)

       (block fnames-loop
         (while fnames
           (if (probe-file (car fnames))
             (with-open-input-file
               (in (car fnames))
               (let ((L (read-changes in))(s (create-string-output-stream)))
                 (dump (cdddr L) s)
                 (return-from fnames-loop 
                              (cons (car L) (get-output-stream-string s))))
               ))
           (setq fnames (cdr fnames))))
       )

     )
    )
  (let ((L '(("## Changes in ~A (English)~%" "release_note.md" "release_note_en.md")
             ("## Changes in ~A (Japanese)~%" "release_note_ja.md"))))
    (while L
      (let ((r (recent-changes (cdr (car L)))))
        (format w (car (car L)) (car r))
        (format w "~A~%" (cdr r)))
      (setq L (cdr L))))
  )

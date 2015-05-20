;; xml.lsp

(define (xml-parse-file file (option (+ 1 4 16)))
  (xml-type-tags nil nil nil nil)
  (let ((data (or (read-file file)
                  (throw-error (sys-error)))))
    (or (xml-parse data option)
        (let ((err (xml-error)))
          (throw-error (list (err 0) (err 1)
                             (slice data (err 1) 100)))))))

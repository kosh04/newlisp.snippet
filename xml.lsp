;; xml.lsp

(define (xml-parse* data (option (+ 1 4 16)))
  (xml-type-tags nil nil nil nil)
  (or (xml-parse data option)
      (let (err (xml-error))
        (throw-error (list (err 0) (err 1)
                           (slice data (err 1) 100))))))

(define (xml-parse-file file)
  (xml-parse* (or (read-file file)
                  (throw-error (sys-error)))))

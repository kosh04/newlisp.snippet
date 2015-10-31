;;; keyword.lsp --- provide :keyword symbol

(new Class 'Keyword)

(define (keyword? x)
  (and (symbol? x)
       (starts-with (term x) ":")))

(define (keyword name)
  (cond ((keyword? name) name)
        ((symbol? name)
         (let (s (sym (string ":" (term name))))
           (constant (global s) s)))
        ((string? name)
         (let (s (sym (string ":" name)))
           (constant (global s) s)))))

(define (keywordize kvp)
  (let ((k (kvp 0))
        (v (kvp 1)))
    (list (keyword (replace "--" k "")) v)))

;;(keywordize '("--oh" "hai"))  ;=> (:oh "hai")

;; eof

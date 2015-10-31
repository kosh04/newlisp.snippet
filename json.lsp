;; json.lsp

; (define (json-parse-file file)
;   (or json2expr
;       (module "json.lsp"))
;   (json2expr (read-file file)))

(define (json-parse-file file)
  (or (json-parse (read-file file))
      (throw-error (json-error))))

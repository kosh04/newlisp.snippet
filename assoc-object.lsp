;;; assoc-object.lsp

;; @module assoc-object
;; @description Assoc List as self-evaluate object

;; @syntax (mkassoc name0 name1 ...)
;; @syntax (mkassoc (key value) ...)
;; @return assoc-list
(define-macro (mkassoc)
  "make assoc list by symbols/lists."
  (cons '@ (map (lambda (x)
                  ;; TODO: "key" or 'key
                  (cond ((symbol? x)
                         (list x (eval x)))
                        ((list? x)
                         (list (first x) (eval (last x))))))
                (args))))

(constant '@ mkassoc)

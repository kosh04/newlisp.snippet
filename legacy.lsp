;; regacy.lsp --- legacy functions (newLISP v.10.1.7)

(module "macro.lsp")

(macro (replace-assoc Key Alist Rep)
  (setf (assoc Key Alist) Rep))

;; @syntax (replace-assoc key alist replace)
;; (define-macro (replace-assoc )
;;   (setf (assoc (eval (args 0))
;;                (eval (args 1)))
;;         (eval (args 2))))

(macro (set-nth Idx Seq Rep)
       (setf (Seq Idx) Rep))

(define (error-number err) (nth 0 (or err (sys-error))))
(define (error-text err) (nth 1 (or err (sys-error))))

(define (concat) (join (args)))

(define (environ) (env))
(define (getenv var) (env var))
(define (putenv var value) (env var value))

(define read-url get-url)
(define (net-cleanup)
  "Closes all open sockets."
  (map net-close (net-sessions))
  true)

(define (read-process str-process) (exec str-process))
(define (write-process str-process str-stdin) (exec str-process str-stdin))

(context MAIN)
;; EOF

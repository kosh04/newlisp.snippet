;; regacy.lsp --- legacy functions (current newLISP v.10.2.4)

;; (module "macro.lsp")

;; @syntax (replace-assoc key alit)
;; @syntax (replace-assoc key alist replace)
(define-macro (replace-assoc )
  (case (length $args)
    (2 (replace (assoc (eval (args 0))
                       (eval (args 1)))
                (eval (args 1))))
    (3 (setf (assoc (eval (args 0))
                    (eval (args 1)))
             (eval (args 2))))
    (true
     (throw-error "missing argument")))
  (eval (args 1)))

;; (macro (replace-assoc Key Alist Rep)
;;   (setf (assoc Key Alist) Rep))

;; set-assoc/assoc-set
;; ref-set


;; @syntax (set-nth int-nth-1 [int-nth-2 ...] list|array exp-replacement)
;; @syntax (set-nth int-nth-1 str str-replacement)

;; @syntax (set-nth (list|array int-nth-1 [int-nth-2 ...]) exp-replacement)
;; @syntax (set-nth (str int-nth-1) str-replacement)

;; set-nth works like nth-set, except instead of returning the replaced
;; element, it returns the entire changed expression. For this reason,
;; set-nth is slower on larger data objects.
(define-macro (set-nth )
  (cond
    ;; (set-nth (seq idx) rep) -> (setf (seq idx) rep)
    ((and (list? (args 0))
          (or (list? (eval (args 0 0)))
              (array? (eval (args 0 0)))
              (string? (eval (args 0 0)))
              (and (context? (eval (args 0 0)))
                   (default (eval (args 0 0))))
              ))
     (setf (eval (args 0)) (eval (args 1))))
    ;; (set-nth idx str rep) -> (setf (str idx) rep)
    ((and (number? (eval (args 0)))
          (string? (eval (args 1))))
     (setf ((eval (args 1)) (eval (args 0)))
           (eval (args 2))))
    ;; (set-nth idx1 [idx2 ...] seq rep) -> (setf (seq '(idx1 idx2 ...)) rep)
    ((number? (eval (args 0)))
     (setf ((eval (args -2)) (map eval (0 -2 (args))))
           (eval (args -1))))
    (true
     (throw-error (list "value or sequence expected" (args 0))))
    ))

;; Sets the int-nth element of a list or array with the evaluation of
;; exp-replacement and returns the old element.
(define nth-set set-nth)

(define set! setq)

;; NOTE: Old inc/dec function returns integer (now float)
(define _inc inc)
(constant 'inc
          (lambda-macro (place (num 1))
            (++ (eval (eval place)) (eval num))))
(define _dec dec)
(constant 'dec
          (lambda-macro (place (num 1))
            (-- (eval (eval place)) (eval num))))

(define (error-number err) (nth 0 (or err (sys-error))))
(define (error-text err) (nth 1 (or err (sys-error))))

(define (concat) (join (args)))

(define (environ)
  (map (lambda (e)
         (string (e 0) "=" (e 1)))
       (env)))
(define (getenv var) (env var))
(define (putenv var value) (env var value))

(define read-url get-url)
(define (net-cleanup)
  "Closes all open sockets."
  (map net-close (net-sessions))
  true)

(define (read-process str-process) (exec str-process))
(define (write-process str-process str-stdin) (exec str-process str-stdin))

;; newlisp v.10.1.2
(define (name sym-context (bool nil))
  (cond (bool (prefix sym-context))
        (true (term sym-context))))

(unless (< (sys-info -2) 9909)
  (define _write-line write-line)
  (constant 'write-line
            (lambda (buffer fdevice)
              (_write-line fdevice buffer))))

;; Swithces break mode on or off.
;; (define break trace)

(context MAIN)
;; EOF

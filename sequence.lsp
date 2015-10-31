;;; sequence.lsp

(define (sample seq)
  (unless (or (list? seq)
              (string? seq))
    (throw-error "Error:LIST_OR_STRING_EXPECTED"))
  ;;(first (randomize seq true))
  (seq (rand (length seq))))

;; syntax: (range [from] to [step])
(define (range)
  (case (length (args))
    (1 (sequence 0 (args 0)))
    (2 (sequence (args 0) (args 1)))
    (3 (sequence (args 0) (args 1) (args 2)))
    (true '())))

;; `union' is built-in since v.10.4.1
; (define (union list1 list2)
;   (let (acc)
;     (dolist (l list1)
;       (if (not (member l list2))
;           (push l acc -1)))
;     (append acc list2)))

(define (fold f init lst)
  (let (result init)
    (dolist (s lst)
      (setq result (f s result)))
    result))

;; (define (foldl f init lst)
;;   (let (result init)
;;     (dolist (s lst)
;;       (setq result (f result s)))
;;     result))

;; (define (foldr f init lst)
;;   (let (result init)
;;     (dolist (s (reverse lst))
;;       (setq result (f s result)))
;;     result))

(define (foldl f init lst)
  (letex (~f f)
    (fold (fn (x y) (~f y x)) init lst)))

(define (foldr f init lst)
  (letex (~f f)
    (fold (fn (x y) (~f x y)) init (reverse lst))))

;; (define (reduce f lst)
;;   (foldl f (first lst) (rest lst)))

;; yet another `reduce'
(define (reduce f lst)
  "uses a binary operation Function, to combine the elements of LST."
  (apply f lst 2))

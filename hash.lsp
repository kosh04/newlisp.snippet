;;; hash.lsp --- Context base hash table

(new Tree 'Hash)

(define (hash? x)
  (and (context? x)
       ;; exists foo:foo
       (context? x (term x))))

;; @syntax (hash-create <Symbol>)
(define (hash-create x)
  (new Tree x))

(define (hash-get hash key)
  ;;(context hash key)
  (hash key))

(define (hash-set hash key value)
  ;;(context hash key value)
  (hash key value))

(define (hash-remove hash key)
  (let (s (sym (format "_%s" key) hash nil))
    (if s (delete s))))

(define (hash-clear hash)
  (map delete (symbols hash))
  nil)

(define (hash-map kv-fn hash)
  (let (acc)
    (dotree (h hash true)
      (push (kv-fn (rest (term h)) (eval h)) acc))
    acc))

(define (hash-keys hash)
  ;;(map first (hash))
  (hash-map (fn (key _) key) hash))

(define (hash-values hash)
  ;;(map last (hash))
  (hash-map (fn (_ value) value) hash))

(define (hash-items hash)
  ;;(hash)
  (hash-map (fn (key value) (list key value)) hash))

(define hash->assoc hash-items)

(define (hash-key? hash key)
  ;;(member key (hash-keys hash))
  (context? hash (format "_%s" key)))

(define (hash-size hash)
  (length (hash)))

(define (hash-empty? hash)
  (empty? (hash)))

;; eof

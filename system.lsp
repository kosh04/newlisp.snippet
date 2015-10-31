;;; system.lsp

(define (has x)
  (let ((features '(("library"   0x040)
                    ("utf8"      0x080)
                    ("newlisp64" 0x100)
                    ("ipv6"      0x200)
                    ("ffi"       0x400))))
    (!= 0 (& (or (lookup (lower-case x) features) 0)
             (sys-info -1)))))

(define (die)
  (if (args) (write 2 (apply format (args))))
  (exit))

(define (%bits i (len 64))
  (replace " " (format (string "%" len "s") (bits i)) "0"))

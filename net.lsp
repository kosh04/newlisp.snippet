;;; net.lsp - newLISP network utility

(define (url-encode url)
  (join (map (lambda (c)
               (if (regex "[^-A-Za-z0-9$_.+!*'(|),]" (char c))
                   (format "%%%2X" c)
                   (char c)))
             (unpack (dup "b" (length url)) url))))

;; URL translation of hex codes with dynamic replacement
(define (url-decode url)
  ;; (PCRE_CASELESS 1)
  (replace "%([0-9A-F][0-9A-F])" url (char (int $1 0 16)) 1))

;; FIXME: (sys-error) が更新されるのはまずいかもしれない
(define (socket? x)
  (and (or (net-local x)
           (net-peer x))
       true))

(define (net-wait socket (mode "read") (ms 1000))
  (until (net-select socket mode ms)
    (if (net-error) (println (net-error)))))

(context MAIN)
;;; EOF

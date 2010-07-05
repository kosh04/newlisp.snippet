;;; net.lsp - newLISP network utility

(define (net-connect! )
  (or (apply net-connect (args))
      (throw-error (net-error))))

(define (url? pathname)
  (starts-with pathname "(http|ftp|mailto|news|telnet|file|man|)://" 1))
(define uri? url?)

;; (define (url-encode url)
;;   (replace "[^-A-Za-z0-9$_.+!*'(|),]" url (format "%%%02X" (char $it)) 0))

;; URL translation of hex codes with dynamic replacement
(define (url-encode url (literal "-A-Za-z0-9$_.+!*'(|),"))
  (setq literal (regex-comp (append "[" literal "]")))
  (join (map (lambda (c)
               (if (regex literal (char c) 0x10000)
                   (char c)
                 (format "%%%02X" c)))
             ;; 8-bit clean でエンコードした方が衛生的
             (unpack (dup "b" (length url)) url))))

;; `replace'は1バイト文字を扱うことが保証されている?
;; (define (url-encode url (literal "-A-Za-z0-9$_.+!*'(|),"))
;;   (replace (append "[^" literal "]")
;;            url
;;            (let (c $0)
;;              (format (dup "%%%02X" (length c))
;;                      (unpack (dup "b" (length c)) c)))
;;            0))

(define (url-decode url)
  ;; (PCRE_CASELESS 1)
  ;; (replace "+" url " ") ; optional
  (replace "%([[:xdigit:]]{2})" url (pack "b" (int $1 0 16)) 0))

;; FIXME: (sys-error) が更新されるのはまずいかもしれない
(define (socket? x)
  (and (or (net-local x)
           (net-peer x))
       true))

(define (net-wait socket (mode "read") (ms 1000))
  (until (net-select socket mode ms)
    (if (net-error) (println (net-error)))))

(define (curl url) (print (get-url url)) true)
(define (curl--head url) (print (get-url url "header")) true)
(define curl-I curl--head)
;; (curl--head "http://www.newlisp.org/")

;; (define (wget url (outfile (basename url))) (write-file outfile (get-url url)))
;; (wget "http://www.newlisp.org/index.cgi")

(define nslookup net-lookup)

(context MAIN)
;;; EOF

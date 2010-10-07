;;; net.lsp - newLISP network utility

(define (net-connect!)
  (or (apply net-connect (args))
      (throw-error (net-error))))

(define (net-lookup!)
  (or (apply net-lookup (args))
      (throw-error (net-error))))

(define (url? pathname)
  (starts-with pathname "(http|ftp|mailto|news|telnet|file|man|)://" 1))

(define uri? url?)

;; `replace'は1バイト文字を扱うことが保証されている?
;; (define (url-encode url)
;;   (replace "[^-A-Za-z0-9$_.+!*'(|),]" url (format "%%%02X" (char $it)) 0))

;; URL translation of hex codes with dynamic replacement
(define (url-encode url (literal "-A-Za-z0-9$_.+!*'(|),"))
  (setq literal (append "[" literal "]"))
  (join (map (lambda (c)
               (if (regex literal (char c))
                   (char c)
                 (format "%%%02X" c)))
             ;; 8-bit clean でエンコードした方が衛生的
             (unpack (dup "b" (length url)) url))))

(define (url-decode url)
  ;; (PCRE_CASELESS 1)
  ;; (replace "+" url " ") ; optional
  (replace "%([[:xdigit:]]{2})" url (pack "b" (int $1 0 16)) 0))

;; FIXME: (sys-error) が更新されるのはまずいかもしれない
(define (socket? x)
  (if (or (net-local x)
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

;; IPアドレスと数値の変換 (IPv4)
;; see also - http://www.nuevatec.com/ip-to-country.html

;; @syntax: (ip->number str-ipaddr|list-ipaddr)
;; @example: (ip->number "192.168.0.1") => 3232235521
;; @example: (ip->number '(127 0 0 1))  => 2130706433
(define (ip->number ip)
  (if (string? ip)
      ;; "127.0.0.1" -> (127 0 0 1)
      (setq ip (map (lambda (n)
                      (int n nil 10))
                    (parse ip "."))))
  (first (unpack ">lu" (pack "bbbb" ip))))

;; @syntax: (number->ip int-ipaddr [return-list?])
;; @example: (number->ip 4294967040) => "255.255.255.0"
;; @example: (number->ip 4294967040 true) => (255 255 255 0)
(define (number->ip num (lst? nil))
  (unless (number? num)
    (throw-error "number expected"))
  (let ((ip (unpack "bbbb" (pack ">lu" num))))
    (if lst?
        ip
      (join (map string ip) "."))))

(context MAIN)
;;; EOF

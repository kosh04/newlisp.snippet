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
(define (url-encode url (literal ""))
  (join (map (lambda (c)
               (if (or (regex "[-A-Za-z0-9$_.+!*'(|),]" (char c))
                       (member (char c) literal))
                   (char c)
                 (format "%%%02X" c)))
             ;; 8-bit clean
             (unpack (dup "b" (length url)) url))))

(define (url-decode url (opt nil))
  (if opt (replace "+" url " "))
  (replace "%([[:xdigit:]]{2})" url (pack "b" (int $1 0 16)) 0))

(define (query-string str)
  "Translate query_string STR to list."
  (map (lambda (item)
         (let ((kv (parse item "=")))
           (list (first kv)
                 (url-decode (last kv) true))))
       (parse str "&")))

;; FIXME: (sys-error) が更新されるのはまずいかもしれない
(define (socket? x)
  (if (or (net-local x)
          (net-peer x))
      true))

(define (net-wait socket (mode "read") (ms 1000))
  (until (net-select socket mode ms)
    (if (net-error)
        (println (net-error))))
  nil)

;; IPアドレスと数値の変換 (IPv4)
;; see also - http://www.nuevatec.com/ip-to-country.html

;; @example (inet_aton "192.168.0.1") => "\192\168\000\001"
(define (inet_aton ip)
  (if (string? ip)
      ;; "127.0.0.1" -> (127 0 0 1)
      (setq ip (map (lambda (n)
                      (int n nil 10))
                    (parse ip "."))))
  (pack "bbbb" ip))

;; @example (inet_ntoa "\192\168\000\001")  => "192.168.0.1"
(define (inet_ntoa byte-ipaddr)
  (format "%d.%d.%d.%d" (unpack "bbbb" byte-ipaddr)))

;; @syntax (ip->number str-ipaddr|list-ipaddr)
;; @example (ip->number "192.168.0.1") => 3232235521
;; @example (ip->number '(127 0 0 1))  => 2130706433
(define (ip->number ip)
  (first (unpack ">lu" (inet_aton ip))))

;; @syntax (number->ip int-ipaddr [return-list?])
;; @example (number->ip 4294967040)      => "255.255.255.0"
;; @example (number->ip 4294967040 true) => (255 255 255 0)
(define (number->ip num (lst? nil))
  (unless (number? num)
    (throw-error "value expected"))
  (let ((ip (inet_ntoa (pack ">lu" num))))
    (if lst?
        (map int (parse ip "."))
        ip)))

(context MAIN)
;;; EOF

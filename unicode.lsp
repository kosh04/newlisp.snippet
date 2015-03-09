;;; unicode.lsp

(define (utf8?)
  "Non-nil means newLISP is UTF-8 eoncoding are supported."
  ;; (= (& (sys-info -1) 128) 128)
  (primitive? MAIN:utf8))

(when (utf8?)
(define (unicode-escape str)
  ;; PCRE_UTF8=0x8000 - enable multibyte character
  (replace "[^[:ascii:]]" str (format "\\u%04x" (char $0)) 0x800))

(define (unicode-unescape str)
  (replace "\\\\u([[:xdigit:]]{4})" str (char (int $1 0 16)) 0))
)                               ; (utf8?)

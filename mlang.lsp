;; @module mlang.lsp
;; @description CodePage Conversion Library (Windows only)
;; @version 0.1 (2012-04-15) first commit.
;; @author KOBAYASHI Shieru (kosh) <shigeru.kb[at]gmail.com>
;; @location none

;; @links
;; - Introduction to MLang
;;   http://msdn.microsoft.com/en-us/library/aa741220(v=vs.85).aspx
;; - ConvertINetString function
;;   http://msdn.microsoft.com/en-us/library/aa741106(v=vs.85).aspx
;; - Code Page Identifiers
;;   http://msdn.microsoft.com/en-us/library/dd317756.aspx

;; - wiconv - Command-line tool
;;   http://openmya.hacker.jp/hasegawa/wiconv/
;; +- `regtool list '\HKCR\MIME\Database\Codepage'`
;; +- `regtool list '\HKLM\System\CurrentControlSet\Control\Nls\CodePage'`

;;; Code:


(unless (member ostype '("Win32" "Windows" "Cygwin"))
  (throw-error "mlang.lsp is available only on Windows platform."))

(context 'MLang)

(define _export_names
  (list
   "ConvertINetMultiByteToUnicode"
   "ConvertINetString"
   "ConvertINetUnicodeToMultiByte"
   "IsConvertINetStringAvailable"
   "LcidToRfc1766A"
   "Rfc1766ToLcidA"
   ))

(dolist (_name _export_names)
  (import "mlang.dll" _name))

;(import "kernel32.dll" "GetACP" "long" "void")             ; UINT GetACP(VOID);
(import "kernel32.dll" "GetConsoleCP" "long" "void")       ; UINT GetConsoleCP(VOID);
;(import "kernel32.dll" "GetConsoleOutputCP" "long" "void") ; UINT GetConsoleOutputCP(VOID);

(define *consoleCP*
  (if (or (and (= ostype "Cygwin")
               (env "LANG")
               (ends-with (env "LANG") "UTF-8") 1) ; e.g. ja_JP.UTF-8
          utf8)
      65001
      (GetConsoleCP)))

(define codepages
  '(
    ("SHIFT_JIS" 932)           ; ANSI/OEM Japanese; Japanese (Shift-JIS)
    ("SJIS" 932)
    ("UTF-16" 1200)
    ("UTF-16LE" 1200)
    ("UTF-16BE" 1201)           ; unicodeFFFE
    ("WINDOWS-1252" 1252)       ; ANSI Latin 1; Western European (Windows)
    ("EUC-JP" 20932)            ; Japanese (JIS 0208-1990 and 0121-1990)
    ("ISO-8859-1" 28591)        ; ISO 8859-1 Latin 1; Western European (ISO)
    ("ISO-8859-15" 28605)       ; ISO 8859-15 Latin 9
    ("ISO-2022-JP" 50220)       ; ISO 2022 Japanese with no halfwidth Katakana; Japanese (JIS)
#    ("euc-jp" 51932)            ; EUC Japanese ?
    ("UTF-7" 65000)             ; Unicode (UTF-7)
    ("UTF-8" 65001)             ; Unicode (UTF-8)
    ))

;; How to convert a string to a specific codepage?
(define (lookup-codepage x)
  (or (if (integer? x) x)
      (if (string? x) (lookup (upper-case x) codepages))
      (throw-error (list "unknown codepage" x))))

(define (available? fromcode tocode)
  (let ((srcCP (lookup-codepage fromcode))
        (dstCP (lookup-codepage tocode)))
    (= 0 (IsConvertINetStringAvailable srcCP dstCP))))

(define (convert src fromcode tocode)
  (letn ((srcCP (lookup-codepage fromcode))
         (dstCP (lookup-codepage tocode))
         (&mode (pack "lu" 0))
         (&srclen (pack "lu" (length src)))
         (dstlen (+ (* (length src) 2) 1)) ; XXX
         (dst (dup "\000" dstlen))
         (&dstlen (pack "lu" dstlen))
         (ret 0))
    (setq ret (ConvertINetString &mode srcCP dstCP src &srclen dst &dstlen))
    ;(setq ret (& ret 0x0000ffff))
    (if (= ret 0x00000001)      ; S_FALSE
        (throw-error (list "The specified conversion is not supported on the system." srcCP dstCP)))
    (if (= ret 0x80004005)      ; E_FAIL
        (throw-error "An error has occurred."))
    (slice dst 0 (first (unpack "lu" &dstlen)))))

(define (encode str tocode)
  (convert str *consoleCP* tocode))

(define (decode str fromcode)
  (convert str fromcode *consoleCP*))

(define (to-utf8 str)
  (convert str *consoleCP* "UTF-8"))

(context MAIN)

; (when (find "-" (main-args))
;   (while (read-line)
;     (write-line 1 (MLang:decode (current-line) "utf-8")))
;   (exit))
true

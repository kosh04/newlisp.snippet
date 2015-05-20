;; @module zlib.lsp
;; @description yet another zlib module

;; @link
;; - http://www.zlib.net/manual.html

(context 'zlib)

(case ostype
  ("Windows"
   (define _libz "/Users/shigeru/bin/zlib1.dll")
   ;;(define _libz "/Users/shigeru/bin/libz-1.dll")
   ;;(define _libz "/Users/shigeru/Dropbox/xyzzy/bin/zlib1.dll")
   )
  ("Win32"
   (define _libz "/Users/shigeru/bin/zlib1.dll"))
  ("Cygwin"
   (define _libz "cygz.dll")))

;; Utils
(define-macro (mkassoc)
  "make assoc list by symbols/lists."
  (map (lambda (x)
         (cond ((symbol? x)
                (list (term x) (eval x)))
               ((list? x)
                (list (string (first x)) (eval (last x))))))
       (args)))

(define (second x) (x 1))

;; Version
(define ZLIB_VERSION "1.2.3")
(define ZLIB_VERNUM 0x1230)

;; Stream Data Structure

(define _struct_z_stream
  '(
    ("next_in"  "void*")        ; z_const Bytef *next_in;
    ("avail_in" "unsigned int") ; uInt avail_in;
    ("total_in" "long")         ; uLong total_in;    // XXX: "unsigned long"  not available
    ("next_out" "void*")        ; Bytef *next_out;
    ("avail_out" "unsigned int"); uInt avail_out;
    ("total_out" "long")        ; uLong total_out;
    ("msg"    "void*")          ; z_const char *msg; // XXX: "char*" will be fail in v.10.6.2
    ("state"  "void*")          ; struct internal_state FAR *state;
    ("zalloc" "void*")          ; alloc_func zalloc;
    ("zfree"  "void*")          ; free_func zfree;
    ("opaque" "void*")          ; voidpf opaque;
    ("data_type" "int")         ; int data_type;
    ("adler"    "long")         ; uLong adler;
    ("reserved" "long")         ; uLong reserved;
    ))

(define (struct* s types)
  (apply struct (cons s types)))

(struct* 'z_stream (map second _struct_z_stream))

;;(struct 'gz_header_s ...)

;; constants
(define Z_NO_FLUSH      0)
(define Z_PARTIAL_FLUSH 1)
(define Z_SYNC_FLUSH    2)
(define Z_FULL_FLUSH    3)
(define Z_FINISH        4)
(define Z_BLOCK         5)
(define Z_TREES         6)

(define Z_OK             0)
(define Z_STREAM_END     1)
(define Z_NEED_DICT      2)
(define Z_ERRNO         -1)
(define Z_STREAM_ERROR  -2)
(define Z_DATA_ERROR    -3)
(define Z_MEM_ERROR     -4)
(define Z_BUF_ERROR     -5)
(define Z_VERSION_ERROR -6)

(define Z_NO_COMPRESSION         0)
(define Z_BEST_SPEED             1)
(define Z_BEST_COMPRESSION       9)
(define Z_DEFAULT_COMPRESSION   -1)

(define Z_FILTERED            1)
(define Z_HUFFMAN_ONLY        2)
(define Z_RLE                 3)
(define Z_FIXED               4)
(define Z_DEFAULT_STRATEGY    0)

(define Z_BINARY   0)
(define Z_TEXT     1)
(define Z_ASCII    Z_TEXT)   ; for compatibility with 1.2.2 and earlier
(define Z_UNKNOWN  2)

(define Z_DEFLATED   8)

(define Z_NULL  0)

;; Basic Functions
(import _libz "zlibVersion"
        "char*"
        "void"
)

;;(import _libz "deflateInit_" "int" "void*" "int" "char*" "int")
(import _libz "deflateInit_")

;; (import _libz "deflateInit"
;;         "int"
;;         "void*"                         ; z_streamp strm
;;         "int"                           ; int level
;; )

(import _libz "deflate" "int" "void*" "int")
(import _libz "deflateEnd" "int" "void*")

(import _libz "inflateInit_" "int" "void*" "char*" "int")
(import _libz "inflate" "int" "void*" "int")
(import _libz "inflateEnd" "int" "void*")

(import _libz "compress")
(import _libz "uncompress")
(import _libz "gzopen")
(import _libz "gzread")
(import _libz "gzclose")
(import _libz "gzwrite")



;; @syntax (zlib:version)
;; @return String: version info
;; @example
;; (zlib:version) ;=> "1.2.3"
(define (version)
  (zlibVersion))

;; ZEXTERN int ZEXPORT uncompress OF((Bytef *dest, uLongf *destLen,
;;                                    const Bytef *source, uLong sourceLen));
;;(define (decompress))

;; hack
;; #define deflateInit(strm, level) \
;;         deflateInit_((strm), (level),       ZLIB_VERSION, sizeof(z_stream))
(define (deflate-init)
  (letn ((z (pack z_stream))
         (status (deflateInit_ z Z_DEFAULT_COMPRESSION (version) (length z))))
    (unpack z_stream z)
    )
  )

(context MAIN)

(println "zlib version: " (zlib:version))

(new Class 'z_stream)

(context z_stream)

;;(struct 'z_stream:struct "int" ...)

(define (z_stream:z_stream)
  (cons (context) (pack zlib:z_stream)))

(define (z_stream:_unpack)
  (unpack zlib:z_stream (self 1)))

(define (z_stream:slot key (value nil))
  (let ((z (:_unpack (self)))
        (i (or (find key (map first zlib:_struct_z_stream))
               (throw-error (list "unknown slot key" key)))))
    (when value
      (setf (z i) value)
      (setf (self 1) (pack zlib:z_stream z)))
    (z i)))

(define (z_stream:pp)
  (println (string
            (map (lambda (type val)
                   (list (type 0) (type 1) val))
                 zlib:_struct_z_stream
                 (:_unpack (self))))))


;; @module zlib.lsp
;; @description yet another zlib module

;; @link
;; - http://www.zlib.net/manual.html

(load "utils.lsp")

(new Class 'z_stream)

(context 'zlib)

(case ostype
  ("Windows"
   ;;(define _libz "zlib1.dll")
   (define _libz "libz-1.dll"))
  ("Win32"
   (define _libz "libz-1"))
  ("Cygwin"
   (define _libz "cygz.dll"))
  ("Linux"
   (define _libz "libz.so"))
  ("OSX"
   (define _libz "libz.dylib"))
  )

;; Utils

(define (second x) (x 1))

;; Version

(define ZLIB_VERSION "1.2.3")
(define ZLIB_VERNUM 0x1230)

;; Stream Data Structure

(define _z_stream_members
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

(struct* '_z_stream (map second _z_stream_members))

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

(import _libz "zlibVersion" "char*" "void")

(import _libz "deflateInit_" "int"
        "void*"                 ; z_streamp strm
        "int"                   ; int level
        "char*"                 ; const char *version
        "int"                   ; int stream_size
)
(define-macro (deflateInit )
  (deflateInit_ (eval (args 0))
                (eval (args 1))
                ZLIB_VERSION
                (length (eval (args 0)))))

(import _libz "deflate" "int" "void*" "int")
(import _libz "deflateEnd" "int" "void*")

(import _libz "inflateInit_" "int"
        "void*"                 ; z_streamp strm
        "char*"                 ; const char *version
        "int"                   ; int stream_size
)
(define (inflateInit strm)
  (inflateInit_ strm ZLIB_VERSION (length strm)))

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

(define CHUNK 16384)

(define (def src (level Z_DEFAULT_COMPRESSION))
  (let ((z (z_stream))
        (dst (dup "\000" (length src)))
        (ret -1))
    (:slot z "zalloc" 0)
    (:slot z "zfree" 0)
    (:slot z "opaque" 0)
    (setf ret (deflateInit (z 1) level))
    (MAIN:assert (= ret Z_OK))
    (:slot z "avail_in" (length src))
    (:slot z "next_in" (address src))
    (:slot z "avail_out" (length dst))
    (:slot z "next_out" (address dst))
    (setf ret (deflate (z 1) Z_FINISH))
    (MAIN:assert (= ret Z_STREAM_END))
    (MAIN:assert (= 0 (:slot z "avail_in")))
    (deflateEnd (z 1))
    (slice dst 0 (:slot z "total_out"))
    ))

(define (inf src)
  (letn ((z (z_stream))
         (dst-tmp (dup "\000" CHUNK))
         (dst "")
         (ret -1))
    (:slot z "zalloc" Z_NULL)
    (:slot z "zfree"  Z_NULL)
    (:slot z "opaque" Z_NULL)
    (:slot z "avail_in" 0)
    (:slot z "next_in" Z_NULL)
    (setq ret (inflateInit (z 1)))
    (MAIN:assert (= ret Z_OK))
    (:slot z "avail_in" (length src))
    (:slot z "next_in" (address src))
    (do-while (= (:slot z "avail_out") 0)
      (:slot z "avail_out" (length dst-tmp))
      (:slot z "next_out" (address dst-tmp))
      (setq ret (inflate (z 1) Z_NO_FLUSH))
      (println ",,,inflate=" ret)
      (MAIN:assert (not (member ret (list Z_NEED_DICT Z_DATA_ERROR Z_MEM_ERROR))))
      ;;(:pp z)
      (extend dst (slice dst-tmp 0 (- (length dst-tmp) (:slot z "avail_out"))))
      )
    (MAIN:assert (= ret Z_STREAM_END))
    (inflateEnd (z 1))
    dst))

;; constructor

(define (z_stream:z_stream)
  (cons (context) (pack _z_stream)))

(define (z_stream:_unpack)
  (unpack _z_stream (self 1)))

(define (z_stream:slot key (value nil))
  (let ((z (:_unpack (self)))
        (i (or (find key (map first _z_stream_members))
               (throw-error (list "unknown slot key" key)))))
    (when value
      (setf (z i) value)
      (setf (self 1) (pack _z_stream z)))
    (z i)))

(define (z_stream:pp)
  (println (string
            (map (lambda (type val)
                   (list (type 0) (type 1) val))
                 _z_stream_members
                 (:_unpack (self))))))

(context MAIN)

(println "zlib version: " (zlib:version))

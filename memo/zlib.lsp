;; memo/zlib.lsp

(load "zlib.lsp")
(load "unittest.lsp")

(case ostype
  ("Win32"
   (import "msvcrt" "printf"))
  ("Cygwin"
   (import "cygwin1" "printf"))
  ("Linux"
   (import "libc.so.6" "printf")))

(define-test "simple"
(local (a b c z zin)
(setf a "Hello Hello Hello Hello Hello Hello!"
      b (dup "\000" 50)
      c (dup "\000" 50))

(printf "Uncompressed size is: %lu\n" (length a))
(printf "Uncompressed data is: '%.*s'\n" (length a) a)

(setf z (z_stream))

(:slot z "zalloc" 0)
(:slot z "zfree"  0)
(:slot z "opaque" 0)
(:slot z "avail_in" (length a))
(:slot z "next_in" (address a))
(:slot z "avail_out" (length b))
(:slot z "next_out" (address b))

(printf "deflate: %d,%d,%d\n"
 (zlib:deflateInit (z 1) zlib:Z_DEFAULT_COMPRESSION)
 (zlib:deflate (z 1) zlib:Z_FINISH)
 (zlib:deflateEnd (z 1))
 )

;;(:pp z)
(printf "Compressed size is: %lu\n" (:slot z "total_out"))
(printf "Compressed data is: '%.*s'\n" (:slot z "total_out") b)

;; inflate b into c
(setf zin (z_stream))
(:slot zin "zalloc" 0)
(:slot zin "zfree"  0)
(:slot zin "opaque" 0)
(:slot zin "avail_in" (- (:slot z "next_out") (address b)))
(:slot zin "next_in" (address b))
(:slot zin "avail_out" (length c))
(:slot zin "next_out" (address c))

(printf "inflate: %d,%d,%d\n"
 ;(zlib:inflateInit_ (zin 1) (zlib:version) (length (zin 1)))
 (zlib:inflateInit (zin 1))
 (zlib:inflate (zin 1) zlib:Z_NO_FLUSH)
 (zlib:inflateEnd (zin 1))
 )

(printf "Uncompressed size is: %lu\n" (:slot zin "total_out"))
(printf "Uncompressed data is: '%.*s'\n" (:slot zin "total_out") c)
))

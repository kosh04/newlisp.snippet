;; c-typedef.lsp

;; FIXME: why not exist "unsigned long" in libffi

;; for libffi
(setf
 _void "void"
 _u_char "byte"
 _u_short "unsigned short int"
 _u_int "unsingned int"
 _u_long "unsigned int"                 ; FIXME
 _uint8_t "byte"
 _uint16_t "unsigned short int"
 _uint32_t "unsigned int"
 _uint64_t "long long"                  ; FIXME
 )

;; for pack/unpack
(setf
 _p_char "c"
 _p_uchar "b"
 _p_byte "b"
 _p_short "d"
 _p_ushort "u"
 _p_int "ld"
 _p_uint "lu"                           ; FIXME
 ;;_p_long "Ld"
 ;;_p_ulong "Lu"
 _p_float "f"
 _p_double "lf"
 _p_void* "lu"
 _p_int8_t "c"
 _p_int16_t "d"
 _p_int32_t "lu"
 _p_uint8_t "b"
 _p_uint16_t "u"
 _p_uint32_t "lu"
 ;;_p_size_t ""
 _p_uintptr_t _p_void*
 )

;; winapi-typedef.lsp

;; Windows Data Types
;; https://msdn.microsoft.com/en-us/library/windows/desktop/aa383751%28v=vs.85%29.aspx

(setf
 _BOOL "int"
 _BOOLEAN "byte"
 _BYTE "byte"
 _CCHAR "char"
 _CHAR "char"
 _WORD "unsigned short int"
 _DWORD "unsigned int"                  ; "unsigned long"
 _UCHAR "char*"
 _PUCHAR "byte"
 _ULONG "unsigned int"                  ; "unsigned long"
 _USHORT "unsigned short int"
 _PVOID "void*"
 _HANDLE _PVOID
 _LPSTR "char*"
 ;; _LPWSTR "char*"
 _LPTSTR _LPSTR
 _LPVOID "void*"
 _UINT "unsigned int"
 )

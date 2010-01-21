;;; winapi.lsp --- use Win32API

(context 'Win32API)

(import "user32.dll" "MessageBoxA")
(import "kernel32.dll" "GetShortPathNameA")
(import "kernel32.dll" "GetLongPathNameA")
(import "shell32.dll" "ShellExecuteA")

(define PATH_MAX 512)


(context MAIN)

;(define NULL 0)

(define (message-box text (title "newLISP"))
  (let ((MB_OK 0))
    (Win32API:MessageBoxA 0 text title MB_OK)))

(define (get-short-path-name pathname)
  (unless (file? pathname)
    (throw-error (list "No such file or directory" pathname)))
  (setq pathname (real-path pathname)) ; to fullpath
  (letn ((len Win32API:PATH_MAX)
         (buf (dup (char 0) (+ len 1)))
         (ret (Win32API:GetShortPathNameA pathname buf len)))
    (slice buf 0 ret)
    ;; (GetShortPathNameA pathname buf len) (get-string buf)
    ))

(define (get-longpathname pathname)
  (letn ((len Win32API:PATH_MAX)
         (buffer (dup (char 0) (+ len 1)))
         (r (Win32API:GetLongPathNameA pathname buffer len)))
    (if (= r 0) (throw-error '("GetLongPathNameA" "failure")))
    (slice buffer 0 r)))

(define (shell-execute app)
  (let ((SW_SHOWNORMAL 1) e)
    (setf e (Win32API:ShellExecuteA 0 "open" app 0 0 SW_SHOWNORMAL))
    ;(if (< e 32) )
    ))
;(shell-execute "C:\\PROGRA~1\\newlisp\\newlisp.exe")
;(shell-execute "C:/")
;(shell-execute "http://www.newlisp.org/")

(context MAIN)
;;; EOF

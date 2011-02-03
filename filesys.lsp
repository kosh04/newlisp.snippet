;; filesys.lsp --- Fileystem Utilities

;; NOTE: Linuxでは存在しないファイルにrealpathを使えない
(define (merge-pathnames pathname (defaults "."))
  (or (real-path (cond
                   ((file? pathname) pathname)
                   ((starts-with pathname "~/")
                    (append (env "HOME") (1 pathname)))
                   ((regex "^[\\|/]" pathname) pathname)
                   (true (append defaults "/" pathname))))
      pathname))
(define (user-homedir-pathname) (real-path))
(define (pwd) (real-path))
(define (namestring pathname) (merge-pathnames pathname))
(define set-default-directory change-dir)
(define (cd path) (change-dir (or path (env "HOME") "/")))
(define (file-exist-p pathname)
  (or (file? pathname)
      (directory? pathname)))
(define (probe-file pathname)
  (and (file-exist-p pathname)
       (real-path pathname)))
(define (truename pathname)
  (or (probe-file pathname)
      (error "%s: No such file or directory" pathname)))

;; FIXME: "/"
;; (define (basename path (sfx ""))
;;   (if (= (path -1) "/")
;;       (setq path (chop path)))
;;   (catch
;;       (for (idx 1 (length path))
;;         (when (= (path (- idx)) "/")
;;           (setq path ((- 1 idx) path))
;;           (throw 'found))))
;;   (if (ends-with path sfx)
;;       (setq path (chop path (length sfx))))
;;   path)

(define (basename path (sfx ""))
  (if (= (path -1) "/")
      (setq path (chop path)))
  (setq path (last (or (parse path "[\\/]" 0) '("/"))))
  (if (ends-with path sfx)
      (setq path (chop path (length sfx))))
  path)

;; "/usr/lib"   => "/usr"
;; "/usr/"      => "/"
;; "usr"        => "."
;; "/"          => "/"
;; "."          => "."
;; ".."         => "."
(define (dirname path)
  (if (and (find (path -1) "/\\")
           (!= "/" path))
      (setq path (chop path)))
  (catch
      (begin
        (for (idx 1 (length path))
          (when (find (path (- idx)) "/\\")
            (setq path (chop path idx))
            (throw 'found)))
        (setq path ".")))
  (cond ((empty? path) "/")
        (true          path)))

(define (file-length pathname)
  "Retun PATHNAMEs file size as byte."
  (file-info pathname 0))

;; same as `concatenate'
;; `string' を使うと文字列以外も変換するので注意
(define (pathname) (join (args)))

(define (pathname? str)
  (or (file? str)
      (directory? str)))

;; ホームディレクトリの展開とエラー検出も行うread-file
(define (read-file! file)
  (unless (starts-with file "http://")
    (setq file (namestring file)))
  (or (read-file file)
      (throw-error (cons file (sys-error)))))

(define (make-temp-file-name (prfx "nl") (suffix "tmp") dir dir?)
  (unless dir
    (setq dir (or (env "TEMP") (env "TMP") (real-path "/tmp") (real-path "."))))
  (let ((accessfn (if dir? directory? file?))
        (pid (getpid))
        (tbl "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"))
    (loop
       (let ((filename (format "%s/%s%d%s.%s" dir prfx pid
                               (perm tbl 3) suffix)))
         (unless (accessfn filename)
           (write-file filename "")     ; touch
           (return (real-path filename)))))))
;; (make-temp-file-name) => "C:\\tmp\\nl-180059w.tmp"

;; FIXME: s/mktemp/mkstemp
(define mktemp make-temp-file-name)


(context MAIN)
;;; EOF

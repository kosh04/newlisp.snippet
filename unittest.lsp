;;; unittest.lsp

;; usage:

;; (load "unittest.lsp")
;; (define-test "foo"
;;   (= (foo) 'result)
;;   (integer? (foo 777)))
;; (Test:run)
;; -> run all test and exit


(new Tree 'TestPool)

;;
(context 'Test)

(define *test-name* nil)

(define-macro (mkassoc)
  "make assoc list by symbols."
  (map (lambda (x)
         (list (term x) (eval x)))
       (args)))

(define (identity obj)
  "Returns its argument."
  obj)

(define (Test:add name form (value nil) (pass nil))
   (TestPool name (mkassoc name form value pass)))

;; syntax: (define-test "name" test0...testN)
(define-macro (define-test name)
  (Test:add name (args) nil nil))

(define (names)
  (map first (TestPool)))

(define (passes)
  (map (lambda (name)
         (lookup "pass" (TestPool name)))
       (names)))

(define (pass-all?)
  (find-all true? (passes)))

(define (pass-count)
  (length (filter true? (passes))))

(define (fail-count)
  (length (filter nil? (passes))))

(define (Test:log)
  (println (apply format (args))))

(define (report-result result form)
  (Test:log "%s ... %s: %s"
            (if result "pass" "FAIL")
            (string *test-name*)
            (string form)))

(define (run-1 name)
  (let ((*test-name* name)
        (form (lookup "form" (TestPool name))))
    (local (values noerr pass)
      (setf noerr (catch (map (lambda (expr)
                                (let ((result (eval expr)))
                                  (report-result result expr)
                                  result))
                              form)
                    'values)
            pass (and (true? noerr)
                      (list? values)    ; not "ERR:"
                      (for-all identity values)))
      (Test:add name form values pass)
      ;;(Test:log "%s" (string (TestPool name)))
      )))

(define (run)
  (dotree (t TestPool true)
    (run-1 (lookup "name" (eval t))))
  (Test:log "---")
  (Test:log "Passed: %d" (pass-count))
  (Test:log "Failed: %d" (fail-count))
  (exit (if (pass-all?) 0 -1)))

(context MAIN)

;;;###export
(define define-test Test:define-test)

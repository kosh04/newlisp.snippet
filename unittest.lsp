;;; unittest.lsp

;; @module arglist.lsp
;; @description Simple Unit Testing Tool
;; @version 0.2

;; Example:

;; (load "unittest.lsp")
;; (define (foo (x 0)) (* x x))
;; (define-test "foo"
;;   (= (foo 10) 100)
;;   (= (foo) 0))
;; (Test:run)
;; -> run all test and exit


(new Class 'Test)
(new Tree 'TestPool)

;;
(context 'Test)

(define-macro (mkassoc)
  "make assoc list by symbols."
  (map (lambda (x)
         (list (term x) (eval x)))
       (args)))

(define (identity obj)
  "Returns its argument."
  obj)

(setq @name 1
      @form 2
      @value 3
      @pass  4
      ;; @finished 5
      )

(define (Test:Test name form (value nil) (pass nil))
  (list (context) name form value pass))

(define (Test:add name form (value nil) (pass nil))
  (TestPool name (Test name form value pass)))

;; @syntax (define-test test-name test0...testN)
(define-macro (define-test name)
  (Test:add name (args) nil nil))

(define (keys ctx)
  (map first (ctx)))

(define (passes)
  (map (lambda (name)
         ((TestPool name) @pass))
       (keys TestPool))
  )

(define (pass-all?)
  (for-all true? (passes)))

(define (pass-count)
  (length (filter true? (passes))))

(define (fail-count)
  (length (filter nil? (passes))))

(define (Test:log)
  (println (apply format (args))))

(define (report-result result test-naem err form)
  (Test:log "%s ... %s: %s => %s"
            (if (and (nil? err) result) "pass" "FAIL")
            (string test-name)
            (string form)
            (string result))
  (when err
    (Test:log ">>> %s" result)))

(define (run-1 test-name pool)
  (local (values pass)
    (setf values (map (lambda (expr)
                        (local (val noerr)
                          (setf noerr (catch (eval expr) 'val))
                          (report-result val test-name (not noerr) expr)
                          (if noerr val nil)))
                      ((pool test-name) @form))
          pass (for-all identity values))
    (setf ((pool test-name) @value) values
          ((pool test-name) @pass) pass)
    ))

(define (run)
  (dotree (t TestPool true)
    (run-1 ((eval t) @name) TestPool))
  (Test:log "---")
  (Test:log "Passed: %d" (pass-count))
  (Test:log "Failed: %d" (fail-count))
  (exit (if (pass-all?) 0 -1)))

(context MAIN)

;;;###export
(define define-test Test:define-test)

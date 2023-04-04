;;;; cps305-utils.lisp

(in-package :cps305-utils)

(defun run-tests (lab)
  (case lab
    (:lab01 (test-lab01))
    (:lab02 (test-lab02))
    (:lab03 (test-lab03))
    (:lab04 (test-lab04))
    (:lab05 (test-lab05))
    (:lab06 (test-lab06))
    (:lab07 (test-lab07))
    (:lab08 (test-lab08))
    (:lab09 (test-lab09))
    (otherwise (format t "Invalid lab identifier. Lab identifiers are in the form :labXX, where XX is the lab number, e.g., :lab03."))))


(defun pprint-test-cases (tcs)
  (when tcs
    (let* ((pair (car tcs))
           (tname (car pair))
           (bdy (cadr pair)))
      (format t "Function ~a - " (subseq (string tname) 5))
      (pprint-tc-bdy bdy))
    (pprint-test-cases (cdr tcs))))

(defun procnames (e)
  (cond ((null e) nil)
        ((symbolp e) (read-from-string (symbol-name e)))
        ((atom e) e)
        (t (cons (procnames (car e)) (procnames (cdr e))))))

(defun pprint-assertions (al)
  (when al
    (let ((a (procnames (car al))))
      (cond ((= (length a) 3) (format t "    ~S should return ==> ~S~%" (second a) (third a)))
            ((eql (car a) 'not) (format t "    ~S should return ==> NIL~%" (second a)))
            (t (format t "    ~S should return ==> T~%" (car a)))))
    (pprint-assertions (cdr al))))

(defun pprint-bindings (b)
  (format t "  Initial bindings:~%    ~S~%" b))

(defun pprint-tc-bdy (bdy)
  (when bdy
    (let ((expr (car bdy)))
      (cond ((equal (symbol-name (car expr)) (symbol-name 'check)) (format t "Assertions: ~%")
             (pprint-assertions (cdr expr)))
            ((eql (car expr) 'let) (pprint-bindings (cadr expr))
             (pprint-tc-bdy (cddr expr)))
            (t (pprint-bindings expr))))
    (pprint-tc-bdy (cdr bdy))))

(defun show-examples (lab &optional (function nil))
  "Pretty prints the test cases associated with lab key and the function"
  (setf *test-cases* nil)
  (run-tests lab)
  (if function
      (let ((tc (find (read-from-string (concatenate 'string "test-" (string function))) (procnames *test-cases*) :key #'car)))
        (format t "--~a~%" tc)
        (if tc (pprint-test-cases (list tc)) (format t "Could not find function ~a~%" function)))
      (pprint-test-cases *test-cases*)))

  
  

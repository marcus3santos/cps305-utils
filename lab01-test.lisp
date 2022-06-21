;;;; lab01.lisp


;; Example of test cases we will use to test and mark an assignment submission

(defvar *balance* 0)

(defun withdraw (x) x)

(defun deposit (x) x)

(deftest test-withdraw ()
  (:= *balance* 100)
  (check 
    (equal (withdraw 0) 100)
    (equal (withdraw 10) 90)
    (equal (withdraw 20) 70)
    (equal (withdraw 60) 10)
    (not (withdraw 80))
    (not (withdraw 10001))
    (not (withdraw -80))
    (not (withdraw -1))
    (not (withdraw -10001))))

(deftest test-deposit ()
  (:= *balance* 100)
  (check
    (equal (deposit 0) 100)
    (not (deposit 10001))
    (equal (deposit 10) 110)
    (equal (deposit 20) 130)
    (not (deposit -10001))
    (not (deposit -1))
    (not (deposit -80))))


(defun test-lab01 ()
  "Calls the test cases and 'forgets' the functions that were tested."
  (test-withdraw)
  (test-deposit))




;;;; lab01-test.lisp

(deftest test-square ()
  (check
    (equal (square 0) 0)
    (equal (square -1) 1)
    (equal (square 4) 16)))

(deftest test-fact ()
  (check
    (equal (fact 0) 1)
    (equal (fact 1) 1)
    (equal (fact 5) 120)))


(deftest test-comb ()
  (check
    (equal (comb 6 0) 1)
    (equal (comb 6 3) 20)
    (equal (comb 7 7) 1)))


(deftest test-largest ()
  (check
    (equal (largest 5 4 1) 5)
    (equal (largest 1 4 5) 5)
    (equal (largest 0 2 -1) 2)))

(defun test-lab01 ()
  "Calls the test cases and 'forgets' the functions that were tested."
  (test-square)
  (test-fact)
  (test-comb)
  (test-largest))

(test-lab01)


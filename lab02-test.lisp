;; Test cases

(in-package #:cps305-utils)

(deftest test-grade ()
  (check
    (equal (grade 50) "F")
    (equal (grade 79) "B+")
    (equal (grade 89) "A")))

(defun test-lab02 ()
  (test-grade))



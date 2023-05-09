;;;; lab01-test.lisp

(in-package #:cps305-utils)

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

(deftest test-enter-garage ()
  (check
   (equal (progn
            (max-capacity 1 3)
            (enter-garage 1)
            (enter-garage 1))
          1)
   (equal (progn
            (max-capacity 2 3)
            (enter-garage 2)
            (enter-garage 2))
          1)))

(deftest test-exit-garage ()
  (check
   (equal (progn
            (max-capacity 1 3)
            (enter-garage 1)
            (enter-garage 1)
            (exit-garage 1))
          2)
   (equal (progn
            (max-capacity 2 3)
            (enter-garage 2)
            (exit-garage 2))
          3)))

(defun test-lab01 ()
  (test-square)
  (test-fact)
  (test-enter-garage)
  (test-exit-garage))




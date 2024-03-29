(in-package #:cps305-utils)


(deftest test-has-loop ()
  (check
    (equal (let ((g '((1 . nil))))
	     (has-loop g 1))
	   nil)
    (equal (let ((g '((1 . 2) (2 . 3) (3 . nil))))
	     (has-loop g 1))
	   nil)
    (equal (let ((g '((1 . (2 3)) (2 . nil) (3 . nil))))
	     (has-loop g 1))
	   nil)
    (equal (let ((g '((1 . (2 3)) (2 . (3 4)) (3 . nil) (4 . nil))))
	     (has-loop g 2))
	   nil)
    (equal (let ((g '((1 . 1))))
	     (has-loop g 1))
	   T)
    (equal (let ((g '((1 . 2) (2 . 1))))
	     (has-loop g 2))
	   T)
    (equal (let ((g '((1 . 2) (2 . (4 3)) (4 . nil) (3 . 1))))
	     (has-loop g 2))
	   T)
    (equal (let ((g '((1 . 2) (2 . (4 3)) (4 . nil) (3 . 1))))
	     (has-loop g 4))
	   nil)))


(defun test-lab09 ()
  (test-has-loop))




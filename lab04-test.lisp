;; lab04-test.lisp

(defstruct movie
  title  director year type)

(defparameter *size* 3) 

(defvar *db* (make-array *size*  :initial-element nil))

(deftest test-delete-movie ()
  (check
    (equal (progn
	     (setf *size* 3)
	     (setf *db* (make-array *size*  :initial-element nil))
	     (add-movie (make-movie :title "Test1"))
	     (add-movie (make-movie :title "Test2"))
	     (add-movie (make-movie :title "Test3"))
	     (delete-movie "Test2")
	     (movie-title (aref *db* 1)))
	   "Test3")))

(defun test-lab04 ()
  (test-delete-movie))

	     
	     

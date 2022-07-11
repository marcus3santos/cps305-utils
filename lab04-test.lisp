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
	   "Test3")
    (equal (progn
	     (setf *size* 3)
	     (setf *db* (make-array *size*  :initial-element nil))
	     (add-movie (make-movie :title "Test1"))
	     (add-movie (make-movie :title "Test2"))
	     (add-movie (make-movie :title "Test3"))
	     (delete-movie "Test3")
	     (aref *db* 2))
           nil)
    (equal (progn
             (setf *size* 3)
             (setf *db* (make-array *size*  :initial-element nil))
             (delete-movie "Test1"))           
           nil)
    (equal (progn
	     (setf *size* 3)
	     (setf *db* (make-array *size*  :initial-element nil))
	     (add-movie (make-movie :title "Test1"))
             (delete-movie "Test1")
             (aref *db* 0))
           nil)))
    
    
(deftest test-sort-title ()
  (check
    (equal (progn
             (setf *size* 4)
             (setf *db* (make-array *size*  :initial-element nil))
             (add-movie (make-movie :title "Test2" :year 1966))
             (add-movie (make-movie :title "Test1" :year 2022))
             (add-movie (make-movie :title "Test3" :year 1984))
             (sort-title)
             (list (movie-title (aref *db* 0)) (movie-title (aref *db* 1)) (movie-title (aref *db* 2))))
           '("Test1" "Test2" "Test3"))
    (equal (progn
             (setf *size* 4)
             (setf *db* (make-array *size*  :initial-element nil))
             (sort-title))
           nil)))
  
(deftest test-sort-year ()
  (check
    (equal (progn
             (setf *size* 4)
             (setf *db* (make-array *size*  :initial-element nil))
             (add-movie (make-movie :title "Test2" :year 1966))
             (add-movie (make-movie :title "Test1" :year 2022))
             (add-movie (make-movie :title "Test3" :year 1984))
             (sort-year)
             (list (movie-year (aref *db* 0)) (movie-year (aref *db* 1)) (movie-year (aref *db* 2))))
           '(1966 1984 2022))
    (equal (progn
             (setf *size* 4)
             (setf *db* (make-array *size*  :initial-element nil))
             (sort-year))
           nil)))



(defun test-lab04 ()
  (test-sort-year)
  (test-sort-title)
  (test-delete-movie))

	     
	     

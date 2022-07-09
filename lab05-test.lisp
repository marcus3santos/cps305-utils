(defstruct deque
  front
  back)


(deftest test-arith-eval ()
  (check
    (equal (arith-eval '([ 1 + 1 ])) 2)
    (equal (arith-eval '([ [ minf 2 3 ] + 1 ])) 3)
    (equal (arith-eval '([ [ sqr 2 ] + 1 ])) 5)
    (equal (arith-eval '([ [ minf 2 3 ] + [ sqr 2 ] ])) 6)
    (equal (arith-eval '([ [ [ sqr 1 ] + [ sqr 2 ] ] + [ [ sqr 3 ] + [ sqr 4 ] ] ])) 30)
    (equal (arith-eval '([ [ [ minf 10000 1 ] + [ sqr 1 ] ] + [ 1 + [ 10 / 10 ] ] ])) 4)
    (equal (arith-eval '([ [ [ [ minf 10000 1 ] + [ sqr 1 ] ] + [ 1 + [ 10 / 10 ] ] ] - 1 ] ])) "Error")
    (equal (arith-eval '([ [ minf 2 3 ] + 1 )) "Error")))

(deftest test-deque ()
  (let ((d (make-deque)))
    (check
      (equal (progn (push-front 1 d)
                    (push-front 2 d)
                    (push-front 3 d))
             '(3 2 1))
      (equal (pop-back d) 1)
      (equal (pop-front d) 3)
      (equal (pop-back d) 2)
      (equal (progn (push-back 4 d)
                    (push-back 5 d)
                    (push-back 6 d))
             '(6 5 4))
      (equal (pop-front d) 4)
      (equal (pop-back d) 6)
      (equal (pop-front d) 5)
      (equal (progn (push-front 1 d)
                    (push-back 2 d)
                    (push-front 3 d)
                    (pop-back d)) 2)
      (equal (pop-back d) 1)
      (equal (pop-back d) 3)
      (equal (push-back 4 d) '(4))
      (equal (pop-front d) 4))))

(defun test-lab05 ()
  (test-arith-eval)
  (test-deque))



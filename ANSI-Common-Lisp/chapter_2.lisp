(defun show-squares ( start end)
  ;; iterator, starting val, increment
  (do ((i start (+ i 1)))
      ;; list containing more or one expressions
      ;; first expression is the break clause
      ;; everything else is like a progn -> rets last expression
      ((> i end)
       (print "yoyo")
       (print "momo")
       'done)
    (format t "~A * ~A equals ~A~%" i i (* i i))))

;; # 3
(defun our-fourth (ls)
  (car (cdr (cdr (cdr ls)))))

;; # 4
(defun greater-a-versus-b (a b)
  (if (> a b)
      a
      b))

;; # 7
;; Using only operators introduced in this chapter,
;; define a function that takes a list as an argument and returns true if one of its elements is a list.
(defun check-for-a-list-elem (ls)
  (let ( (ret nil))
    (dolist (element ls)
      (if (listp element)
	  (setf ret element)))
    ret))
	  
;; # 8
;; Give iterative and recursion definitions of a func that takes in an integer and prints out as many dots

(defun print-out-n-dots-iterative(some-integer)
  (do ((i 0 (+ i 1)))
      ((= i some-integer))
    (format t ".~%")))  
    
(defun print-out-n-dots-iterative(some-integer)
  (dotimes (i some-integer)
    (format t ". ")))

(defun print-out-n-dots-recursive(some-integer)
  (if (equal some-integer 0)
      (format t "~%")
      (progn (format t ". ") (print-out-n-dots-recursive (- some-integer 1)))))

;; takes a list and returns the number of times the symbol a occurs in it
(defun num-repeating-a-iterative( ls )
  (let ( (s 0))
    (dolist (i ls)
      (if (equal i 'a)
	  (setf s (+ 1 s))))
    s))

(defun num-repeating-a-recursive (ls) 
  (if ls
      (+ (if (equal (car ls) 'a)
	     1
	     0)
	 (a-rec (cdr ls)))
      0))
  

      
  

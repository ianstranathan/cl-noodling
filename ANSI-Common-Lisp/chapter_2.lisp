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
	  

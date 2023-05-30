;; assoc list is a list of conses
;; (assoc key ls) -> pair w/ key
;; --
;; subseq is like slice in python
;; --
;; set operations are kinda nice: union, intersection, set-difference
;; --
;; append is the same as in python

;; --------------------------------------------------
;; # 2.
(defun my-union-iterative(a b)
  (let ((ra (reverse a)))
    (dolist (x b)
      (if (not (member x ra))
	  (push x ra)))
    (reverse ra)))

(defun my-union-recursive (a b &optional x)
  (if (atom b)
      (reverse a)
      (if (not x) ; reverse list a outside the recursion
	  (my-union-recursive (reverse a) b T)
	  (if (member (car b) a)
	      (my-union-recursive a (cdr b) T)
	      (my-union-recursive (cons (car b) a) (cdr b) T)))))

;; --------------------------------------------------
;; # 3.
(defun occurrences (ls)
  ;; takes in a list and returns an assoc list sorted by
  ;; how many occurences each element has
  (let ((ret () ))
    (dolist (i ls)
      (let ((assoc-val (assoc i ret)))
	(if assoc-val
	    (setf (cdr assoc-val) (+ (cdr assoc-val) 1))
	    (setf ret (cons (cons i 1) ret)))))
    (sort ret #'> :key #'cdr)))
    

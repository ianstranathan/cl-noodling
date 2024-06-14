(defun interleave-two-lists(ls-one ls-two)
  ;; links and names have to be same size
  (labels ((recur (acc ls-a ls-b)
	     (if (or ls-a ls-b)
		 (recur (cons (car ls-b) (cons (car ls-a) acc))
			(cdr ls-a)
			(cdr ls-b))
		 (nreverse acc))))
    (recur nil ls-one ls-two)))

  
(defun split-str (str delimiter)
  (labels ((recur (ls curr-index)
             (let ((next-index (position delimiter str :start curr-index)))
               (if next-index
                   (recur (cons (subseq str curr-index next-index) ls) (1+ next-index))
		   (nreverse (cons (subseq str curr-index) ls))))))
    (recur nil 0)))

(defun collect-leaves ( tree )
  (let ((leaves ()))
    (labels ((walk (tree)
	       (cond
		 ((null tree))
		 ((atom tree)
		  (push tree leaves))
		 (t (walk (car tree))
		    (walk (cdr tree))))))
      (walk tree))
    (nreverse leaves)))

(defun pathname-assert (path &optional (another-path #P"c:/"))
  ;; (uiop:pathname-parent-directory-pathname #P"c:/") loops forever
  (assert (apply #'> (mapcar (lambda (x) (length (namestring x))) (list path another-path)))))

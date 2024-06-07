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

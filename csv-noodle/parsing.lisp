(defparameter *excel-src-path* "C:/Work/cl-access-health/excel-src/")


;; csv field can be seperated by comma, however there can be commas within a field itself
;; => must first check if you've hit a comma, then
(defun loop-over-csv-recursive ( file-name
				 &optional (formatting-fn  #'identity ))
  "takes in a file name and loops through it collecting according to some predicate"
  (let* ((file-path (concatenate 'string *excel-src-path* file-name))
	 (in (open file-path :if-does-not-exist nil))
	 (first-row (read-line in nil)))
    (labels ((recur (acc)
	       (let ((next-row (read-line in nil)))		 
		 (if next-row
		     (recur (cons (funcall formatting-fn next-row) acc))
		     (nreverse acc)))))
      (recur (list (funcall formatting-fn first-row))))))		


(defun split-str (str delimiter)
  "This function splits a string array into a list containing subsequences according to a delimiter
   if no delimiter is provided, it defaults to a comma #\,"
  (labels ((recur (ls curr-index)
             (let ((next-index (position delimiter str :start curr-index)))
               (if next-index
                   (recur (cons (subseq str curr-index next-index) ls) (1+ next-index))
		   (nreverse (cons (subseq str curr-index) ls))))))
    (recur nil 0)))


(defun csv-embedded-comma-split (str &optional (formatting-fn  #'identity ))
  ;; ,\"Fever,UTI, Kidney stone\", 
  ;; ,\"Something\", ;; both quotes have indices lower than the comma
  ;; if the next quote is indexed lower than the comma  
  (labels ((recur (ls curr-index)
             (let* ((next-comma-index (position #\, str :start curr-index))
		    (next-quote-index (position #\" str :start curr-index))
		    (quote-index-before-next-comma (position #\" str :from-end t :start curr-index :end next-comma-index))
		    ;; must have comma seperated field entries within two quotation marks to be well formed csv
		    (closing-index (if (eql next-quote-index
					    quote-index-before-next-comma)
				       ;; if there is no closing quote, go to end of quote
				       ;; keep quotations on end uniform, so add one to index
				       (+ 1 (position #\" str :start (+ 1 next-comma-index)))
				       
					 ;; otherwise, just use the comma index
				       next-comma-index)))
	       (if closing-index
		   (recur (cons (funcall formatting-fn (subseq str curr-index closing-index)) ls)
			  (1+ closing-index))
		   (nreverse (cons (subseq str curr-index) ls))))))
    (recur nil 0)))


(defun split-csv-row ( str )
  (csv-embedded-comma-split str (lambda (s)
				  (string-trim '(#\") s))))
  





(in-package :access-health)

;; example file
(defparameter file "tools_admit_and_visits.csv")

;; example columns we care about
(defparameter cols (list "Client Id" "Admit Date" "Location"))

;; csv file as a list of lists (each row is a sublist)
(defun collect-across-csv (a-file col-ls)
  (let* ((csv-ls (loop-over-csv-recursive a-file #'split-csv-row))
	 (col-indices (mapcar (lambda (col)
					  (position col (first csv-ls) :test #'string=))
					col-ls)))
    (labels ((row-collect-fn (acc ls)
	       (cons (mapcar (lambda (i) (nth i ls)) col-indices) acc))
	     (recur (acc ls)
	       (if ls
		   (recur (row-collect-fn acc (car ls)) (cdr ls))
		   (reverse acc))))
      (recur nil csv-ls))))
	       

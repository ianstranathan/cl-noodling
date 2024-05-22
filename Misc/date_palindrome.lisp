(defun my-reverse (ls)
  ;; should just use built in reverse, but w/e
  (labels (( recur ( ls &optional rev-ls)
	     (if ls
		 (recur (cdr ls)(cons (car ls) rev-ls))
		 rev-ls)))
    (recur ls)))

(defun ls-is-a-palindrome (ls)
  (equal (my-reverse ls) ls))

(defun sub-ls-to-denary ( ls)
  (+ (* 10 (first ls)) (second ls)))

(defun is-a-leap-year (num)
  ;; only valid on current interval
  (eql (mod num 4) 0))

(defun days-in-a-month (date-ls)
  (if (eql (first date-ls) 2) ; is it  february
      (if (is-a-leap-year (third date-ls)) ; is the year a leap year
	  29
	  28)
      (regular-days-in-month (first date-ls)))) ; not february

(defun regular-days-in-month (month-int)
  ;; month-int: a number between 1-12
  ;; a func to give periodic numbers along integers, purposefully mathy
  ;; probably should just do an even/ odd pattern, but also w/e
  (labels (( func (x)
	     (truncate (abs (sin (* (/ PI 2) x))))))
    ;; offset the number by one if you get to August
    (if ( < (- month-int 8) 0)	    
        ;; superposition with 0 and 1 shader-style
	(+ (* 31 (func month-int))       (* 30 (- 1 (func month-int))))
	(+ (* 31 (func (- month-int 1))) (* 30 (- 1 (func (- month-int 1))))))))
    
(defun incrementer (num-ls &key time-type)
  ;; returns the increment of a sublist num on its respective modulus
  ;; 12 - month; days in the month - day; 100 year
  (cond ((equal time-type "day")
	 (mod-1 (+ 1 (second num-ls))            ; incremenet the day
		(days-in-month (first num-ls)))) ; mod on [1 : days-in-month]
	((equal time-type "month")
	 (mod-1 (+ 1 (first num-ls)) 12))
	((equal time-type "year")
	 (mod (+ 1 (third num-ls)) 100))))

(defun mod-1 (num modulus)
  ;; range 1 to modulus
  (+ 1 (mod (- num 1) modulus)))

(defun denary-to-unary(num)
  ;; breaks two digit number into its unary components, only two digit numbers
  (if ( < num 10)
      (list num)
      (list (truncate (/ num 10)) (mod num 10))))

(defun flatten-num-ls (ls)
  (apply #'append (mapcar #'denary-to-unary ls)))

(defun num-ls-is-palindrome (ls)
  (ls-is-a-palindrome (flatten-num-ls ls)))

(defun ordered-incrementer (date-ls)
  (labels ((unit-flipped-over (next curr)
	     (if ( < (- next curr) 0)
		 T))
	   (incr-if-base-unit-flipped (next curr this-time-type)
	     ;; if next isn't null and the base unit flipped over
	     (if (and next (funcall #'unit-flipped-over next curr ))
		 (incrementer date-ls :time-type this-time-type)))
	   (take-next-or-not (next curr)
	     (if next
		 next
		 curr)))
	   
    (let* ((next-day   (incrementer date-ls :time-type "day"))
	   (next-month (funcall
			#'incr-if-base-unit-flipped next-day (second date-ls) "month"))
	   (next-year  (funcall
			#'incr-if-base-unit-flipped next-month (first date-ls) "year")))
      (list (take-next-or-not next-month (first date-ls))
	    next-day
	    (take-next-or-not next-year (third date-ls))))))

(defun get-all-the-palindromes-between-dates (a-date-ls
					      b-date-ls)
  (labels ((recur (curr-date-ls &optional acc)
	     (if (equal curr-date-ls
			b-date-ls)
		 acc
		 (if (num-ls-is-palindrome curr-date-ls)
		     (recur (ordered-incrementer curr-date-ls)(cons curr-date-ls acc))
		     (recur (ordered-incrementer curr-date-ls) acc)))))
    (funcall #'recur a-date-ls)))
	   
(defun palindrome-printout ()
  (dolist (i (get-all-the-palindromes-between-dates '(12 28 61) '(5 6 24)))
    (print i)))

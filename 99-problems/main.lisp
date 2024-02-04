
;; find the last element of a list
(defun p-01 (ls)
  (if (cdr ls)
      (p-01 (cdr ls))
      (car ls)))

;; find the last two elements of a list
(defun p-02 (ls)
  ;; if 
  (if (cdr (cdr ls)) 
      (p-02 (cdr ls))
      ls))

;; find the n'th element of a list
(defun p-03 (ls index)
  (if (<= index 1)
      (car ls)
      (p-03 (cdr ls) (- index 1))))

;; find the number of elements in a list
(defun p-04 (ls &optional count)
  (if ls
      (if count
	  (p-04 (cdr ls) (1+ count))
	  (p-04 (cdr ls) 1))
      (if count
	  count
	  0)))

;; reverse a list
(defun p-05 (ls &optional rev-ls)
  (if ls
      (p-05 (cdr ls) (cons (car ls) rev-ls))
      rev-ls))

;; determine if a list is a palindrome
(defun p-06 (ls)
  (equal (p-05 ls) ls))

;; flatten a list
(defun flatten (ls)
  (if ls
      (let ((elem (car ls))
	    (_rest-of-list (cdr ls)))
        (if (atom elem)
	    (append (cons elem nil) (flatten _rest-of-list))
            (append (flatten elem)  (flatten _rest-of-list))))
      nil))

(defun my-append (ls-1 ls-2)
  (if ls-1
      ;; (my-append '(a b c) (1 2 3))
      ;; -> (cons 'a (cons 'b (cons 'c '( 1 2 3))))
      (cons (car ls-1) (my-append (cdr ls-1) ls-2))
      ls-2))


;; Eliminate consecutive duplicates of list elements
;; '(a a a a b c c a a d e e e e)
(defun p-08 (ls &optional last-elem acc)
  (if ls
      (if (equal (car ls) last-elem)
	  (p-08 (cdr ls) (car ls) acc)
	  (p-08 (cdr ls) (car ls) (cons (car ls) acc)))
      (p-05 acc)))


;; Pack consecutive duplicates of list elements into sublists
;; '(a a a a b c c a a d e e e e))
;; ((A A A A) (B) (C C) (A A) (D) (E E E E))
(defun p-09 (ls &optional last-elem last-list acc)
  ;; (print last-list)
  ;; (print ls)
  ;; (print (equal (car ls) last-elem))
  ;; (print "--------------------")
  (if ls
      (if (or (equal (car ls) last-elem)
	      (null last-elem))
	  ;; if equal cons with last-list
	  (p-09 (cdr ls)
		(car ls)
		(cons (car ls) last-list)
		acc)
	  (p-09 (cdr ls)
		(car ls)
		(cons (car ls) nil)
		(cons last-list acc)))
      ;; cons last-list and reverse
      (progn (p-05 (cons last-list acc)))))


;; (encode '(a a a a b c c a a d e e e e)) ->
;; ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
(defun p-10 (ls)
  (labels ((

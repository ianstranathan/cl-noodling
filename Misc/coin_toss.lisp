(defun toss-a-coin ()
  (let ((a-coin-toss (random 2)))
    (if (equal a-coin-toss 0)
	"heads"
	"tails")))

(defun guess()
  (print "Guess, heads or tails?")
  (let ((a-guess (read-line )))
    (if (or (equal (string-downcase a-guess) "heads")
	    (equal (string-downcase a-guess) "tails"))
	a-guess
	(progn (print "Sorry, that's an invalid entry?")
	       (guess)))))

(defun coin-toss-game (num-allowable-guesses &optional guesses-left)
  (if guesses-left
      (if (>= guesses-left 0)	  
	  (if (equal (guess)
		     (toss-a-coin))
	      (print "You got it buddy, gratz")
	      (progn (format t "You have ~d guesses left" guesses-left)
		     (setf guesses-left (- guesses-left 1))
		     (coin-toss-game num-allowable-guesses guesses-left)))
	  (print "Sorry pally, better luch next time"))
      (let ((g (- num-allowable-guesses 1))
	(coin-toss-game num-allowable-guesses g))))
				     
		 
      ;; initialize null param and start
      (coin-toss-game num-allowable-guesses num-allowable-guesses)
;; (defun get-guess (allowable-guess-count &optional guesses-left)
;;   ;; is guesses left not nil and not zero
;;   (if (and guesses-left (not (equal guesses-left 0)))
;;       (print "Guess, heads or tails?")
      
      
;;       (let ( (num-guesses 0))
    

  

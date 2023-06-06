
(defun get-player-choice (rock-paper-scissors-options &optional num_trys current_try)  
  (format t "Please choose one: ~{ ~a ~}~%" rock-paper-scissors-options)
  (or (find (read-line) rock-paper-scissors-options :test #' equal)
      (get-player-choice rock-paper-scissors-options)))


(defun get-computer-choice (rock-paper-scissors-options)
  ;; pick random index in rock-paper-scissors-options
  ;; random is exclusive on upper bound
  (nth (random (length rock-paper-scissors-options)) rock-paper-scissors-options))


(defun determine-winner (computer-choice player-choice)
  (cond ((equal computer-choice player-choice)
	 "draw")
	((and (equal player-choice "scissors")
	       (equal computer-choice "paper"))
	 "player")
	((and (equal player-choice "paper")
	      (equal computer-choice "rock"))
	 "player")
	
	((and (equal player-choice "rock")
	      (equal computer-choice "scissors"))
	 "player")
	(t "computer")))


(defun rock-paper-scissors-game (&optional (best-of 3) (num-computer-wins 0) (num-player-wins 0))
  ;; base case, computer or player wins
  ;; => must increment player or computer
  (cond ( (equal num-computer-wins (- best-of 1))
	  "Sorry pally, the computer won")
	( (equal num-player-wins (- best-of 1))
	  "You did it pally, you won")
	( t
	  (progn (format t "score so far: ~% computer: ~a ~% player: ~a ~%" num-computer-wins num-player-wins)
		 (let* ((options '("rock" "paper" "scissors"))
			(computer-choice (get-computer-choice options))
			(player-choice   (get-player-choice options))
			(winner (determine-winner computer-choice player-choice)))
		   (cond ((equal winner "computer")
			  (progn (format t "Too bad, the computer chose ~a~%" computer-choice)
				 (rock-paper-scissors-game best-of
							   (+ num-computer-wins 1)
							   num-player-wins)))
			 ((equal winner "player")
			  (progn (format t "Great, the computer chose ~a~%" computer-choice)
				 (rock-paper-scissors-game best-of
							   num-computer-wins
							   (+ num-player-wins 1))))
			 ((equal winner "draw")
			  (progn (format t "It was a draw~%")
 				 (rock-paper-scissors-game best-of
							   num-computer-wins
							   num-player-wins)))))))))
 

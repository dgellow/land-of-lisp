;;; 1_guess_number.lisp -  A basic "guess my number" game

;; Copyright (C) 2013 Samuel El-Borai

;; Author: Samuel El-Borai <samuel.elborai@gmail.com>
;; Created: 1 Sept 2013

(defun play (high-limit)
  (let ((guess (random high-limit))
	(high high-limit)
	(low 0))
    (next-guess low high guess)))

(defun next-guess (low high guess)
  (cond ((> (guess-number low high) guess)
	 (progn
	   (print "lower")
	   (mapc (lambda (x) (prin1 (format t " ~D " x))) (list low high guess))
	   (next-guess low (1- (guess-number low high)) guess)))
	((< (guess-number low high) guess)
	 (progn
	   (print "higher")
	   (mapc (lambda (x) (prin1 (format t " ~D " x))) (list low high guess))
	   (next-guess (1+ (guess-number low high)) high guess)))
	((= (guess-number low high) guess)
	 (identity guess))))

(defun guess-number (low-limit high-limit)
  (ash (+ low-limit high-limit) -1))

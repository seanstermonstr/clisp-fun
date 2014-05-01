(defparameter *small* 1)
(defparameter *big* 100)


;; function that guesses the number
(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

;; function that handles guessing if the number is smaller
(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

;; function that handles guessing if the number id bigger
(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

(defun start-over ()
  (defparameter *small* 1)
  (defparameter *big* 100)
  (guess-my-number))




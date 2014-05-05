;; Global variables etc for players and monsters

;; Player
(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

;;Monsters
(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

;;------------------------------------------------------------->
;; Main game functions


;; Starts the game loop, and does inits
(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over."))
  (when (monsters-dead)
    (princ "Congratz! You have vanquished all of your foes.")))

;; Defines the Game loop
(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
         (lambda (m)
           (or (monster-dead m) (monster-attack m)))
         *monsters*)
    (game-loop)))


;;------------------------------------------------------------->
;; Player management functions

;; Function that inits the player
(defun init-player ()
  (setf *player-health* 50)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

;; Function that checks if the player is dead
(defun player-dead ()
  (< *player-health* 1))

;; Prints some information about the player
(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with a health of ")
  (princ *player-health*)
  (princ " , an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

;; Function that allows for management of the players attacks
(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read)
    (s (monster-hit (pick-monster)
                    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
         (princ "Your double swing has a strength of ")
         (princ x)
         (fresh-line)
         (monster-hit (pick-monster) x)
         (unless (monsters-dead)
           (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                 (unless (monsters-dead)
                   (monster-hit (random-monster) 1))))))

;; Function that handles generating some random values for the game
(defun randval (n)
  (1+ (random (max 1 n))))





;;----------------------------------------------------------->
;; Some helpful functions dealing with player attacks.

;; Picks a monster at random for the roundhouse attack
;; Endures the monster is not dead
(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
        (random-monster)
      m)))

;; Function that allows the player to pick a monster to target for
;; non-ramdom attacks
(defun pick-monster ()
  (fresh-line)
  (princ "Monster #:")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
        (progn (princ "That is not a valid monster number.")
                (pick-monster))
      (let ((m (aref *monsters* (1- x))))
        (if (monster-dead m)
            (progn (princ "That monster is already dead.")
                    (pick-monster))
          m)))))


;;------------------------------------------------------------>
;; Some functions dealing with monster management

;; Inits the monsters
(defun init-monsters ()
  (setf *monsters*
        (map 'vector
             (lambda (x)
               (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
             (make-array *monster-num*))))

;; Function that checks if a given monster is dead
(defun monster-dead (m)
  (<= (monster-health m) 0))

;; Function that checks  if all the monsters are dead
(defun monsters-dead ()
  (every #'monster-dead *monsters*))

;; Function that prints all the monsters
(defun show-monsters ()
  (fresh-line)
  (princ "Your foes:")
  (let ((x 0))
    (map 'list
         (lambda (m)
           (fresh-line)
           (princ "    ")
           (princ (incf x))
           (princ ". ")
           (if (monster-dead m)
               (princ "**dead**")
             (progn (princ "(Health=")
                    (princ (monster-health m))
                    (princ ") ")
                    (monster-show m))))
         *monsters*)))

;;--------------------------------------------------------->
;; Generic functions that give some life to the monsters

;; Structure declaration defining a generic monster
(defstruct monster (health (randval 10)))

;; Method declaration that will display information about what monster
;; the knight just hit
(defmethod monster-hit (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn (princ "You killed the ")
             (princ (type-of m))
             (princ "! "))
    (progn (princ "You hit the ")
           (princ (type-of m))
           (princ ", knocking off ")
           (princ x)
           (princ "health points! "))))

;; Method that shows the monster and its stats
(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m)))

;; Placeholder method that doesnt actually do anything
(defmethod monster-attack (m))

;;------------------------------------------------------------->
;; Specific monster functions!



;; Dealing with the Wicked Orc!
;;
;;
;;

;; Struct def for the wicked orc
(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)

;; Wicked orc monster show method
(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))

;; Wicked orc monster attack method
(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (princ "An orc swings his club at you and knocks off ")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)))


;; Dealing with the Hydra!
;;
;;
;;

;; Struct def for the Hydra
(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

;; Hydra show method
(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))

;; Hydra hit method
(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
    (progn (princ "You lop off ")
           (princ x)
           (princ " of the hydra's heads! "))))

;; Hydra attack method
(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attacks you with ")
    (princ x)
    (princ " of its heads! It also grows back one more head! ")
    (incf (monster-health m))
    (decf *player-health* x)))


;; Dealing with Slimy Slime Mold!
;;
;;
;;

;; struct def for slime mold.
(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)

;; Slime Mold show method
(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m)))

;; Slime Mold attack method
(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-sliminess m))))
    (princ "A clime mold wraps around your legs and decreases your agility by ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (decf *player-health*))))

;; Dealing with The Cunning Brigand!
;;
;;
;;

;; struct def for the Cunning Brigand
(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

;; Cunning brigand attack method
(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
           (princ "Abrigand hits you with his clingshot, taking off 2 health points! ")
           (decf *player-health* 2))
          ((= x *player-agility*)
           (princ "A brigand catches your leg with his whip, taking off 2 agility points! ")
           (decf *player-agility* 2))
          (( = x *player-strength*)
           (princ "A brigand cuts your arm with his whip, taking off two strength points! ")
           (decf *player-strength* 2)))))





;; Defines the top level text for the game.  
(defparameter *nodes* '((living-room (you are in the living room.
                                          a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                                     There is a well in front of you.))
                        (attic (you are in the attic.
                                    there is a giant welding tourch in the corner.))))

;; discriptions of paths to the locations of the map
(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

;; objects in the game world
(defparameter *objects* '(whiskey bucket frog chain))

;; locations of the objects in our world
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

;; Tracks the players current location
(defparameter *location* 'living-room)

;; returns the information in a node of the alist
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

;; describes the path to a location
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; can generate description for all edges from a given location
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;; describes objects at their given locations
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
                     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

;; Describes objects visable at a given location
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
                         `(you see a ,obj on the floor)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;; Dirty part of the program
;; Not functional

;; Command to allow the player to look around
;; Not a command in the functional style.
;; A dirty part of the program.
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

;; Allows the user to input direction, and walks there
(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
      '(you cannot go that way.))))

;; Allows us to pick up objects in our game world.
(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot pick that up))))

;; Handles checking of the inventory.
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;; games interface functions
;;
;;
;;

;; stores what commands our custom repl will take
(defparameter *allowed-commands* '(look walk pickup inventory))

;; function that only allows certain commands to be called
(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

;; manager function for setting up the games custom REPL
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

;; custom read function to get correctly
;; formated input from the user
(defun game-read ()
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;; custom printing function
(defun game-print (1st)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string 1st))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))

;; helper function for the printing function
(defun tweak-text (1st caps lit)
  (when 1st
    (let ((item (car 1st))
          (rest (cdr 1st)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))




#|
;; some test code written in this file

;; getting input function
(defun say-hello ()
  (print "please type your name:")
  (let ((name (read-line)))
    (print "nice to meet you, ")
    (print name)))

;; add five function
(defun add-five ()
  (print "please enter a number:")
  (let ((num (read)))
    (print "When i add five i get")
    (print (+ 5 num))))

(defparameter *foo* '(+ 1 2))
|#


;; Loads the graphing utility lib
(load "graph-util")
;; Global variables for the game
(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)
(defparameter *player-pos* nil)

;;------------------------------------------------->
;; Functions that handle generating random edges

;; Returns a random node identifier
(defun random-node ()
  (1+ (random *node-num*)))

;; Creates two directed edges between a randomly selected node
(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

;; Generates the actual list of random edges
(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
                       collect (edge-pair (random-node) (random-node)))))




;;------------------------------------------------->
;; Preventing Islands
;; We take the list of edges and find the unconnected nodes
;; then we connect the islands to the rest of the city.

;; Finds all the edges in an edge-list starting from a given node
(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                 edge-list))

;; Builds a list of connections from a given node to as
;; many nodes are connected to that node
(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
                       (unless (member node visited)
                         (push node visited)
                         (mapc (lambda (edge)
                                 (traverse (cdr edge)))
                               (direct-edges node edge-list)))))
            (traverse node))
    visited))

;; Finds all the islands in the edge-list
;; Essentially finds all the land masses
(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
               (let* ((connected (get-connected (car nodes) edge-list))
                      (unconnected (set-difference nodes connected)))
                 (push connected islands)
                 (when unconnected
                   (find-island unconnected)))))
      (find-island nodes))
    islands))

;; Creates bridges between all the islands/land masses
(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

;; Ties all of our island prevention functions together
(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))




;;----------------------------------------------->
;; Building the final edges to Congestion city

;; Creates a list of nodes, then it creates an edge list
(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
                   collect i))
         (edge-list (connect-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (x)
                                (zerop (random *cop-odds*)))
                              edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

;; Takes a list of edges and turns it into an alist of edges
(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge)
                            (list (cdr edge)))
                          (remove-duplicates (direct-edges node1 edge-list)
                                             :test #'equal))))
          (remove-duplicates (mapcar #'car edge-list))))

;; Marks the edges of the alist that have cops on them
(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (node1-edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                (if (intersection (edge-pair node1 node2)
                                                  edges-with-cops
                                                  :test #'equal)
                                    (list node2 'cops)
                                    edge)))
                            node1-edges))))
          edge-alist))


;;------------------------------------------------------------>
;; Building the nodes for Conjestion City

;; Checks a nodes neighbors
(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

;; Checks to see if two given nodes are within one from the other
(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

;; Checks to see if two given nodes are two nodes apart or less
(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x)
              (within-one x b edge-alist))
            (neighbors a edge-alist))))

;; Builds the final node alist or the final map of our city
(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num*
                       collect (random-node))))
    (loop for n from 1 to *node-num*
       collect (append (list n)
                       (cond ((eql n wumpus) '(wumpus))
                             ((within-two n wumpus edge-alist) '(blood!)))
                       (cond ((member n glow-worms)
                              '(glow-worm))
                             ((some (lambda (worm)
                                      (within-one n worm edge-alist))
                                    glow-worms)
                              '(lights!)))
                       (when (some #'cdr (cdr (assoc n edge-alist)))
                         '(sirens!))))))



;;----------------------------------------------------------->
;; Init a game of Grand Theft Wumpus

;; Starts a new game by generating required items
(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

;; Finds an empty node for the player to start at
;; Inefficient like most functions in this program
(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
        (find-empty-node)
        x)))

;;--------------------------------------------------------->
;; Drawing the city map
;; This is the part of the codet that hooks up to the lib

;; function that draws the city
(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))







;;---------------------------------------------------------->
;; Drawing a partial city

;; Building a list of known nodes
(defun known-city-nodes ()
  (mapcar (lambda (node)
            (if (member node *visited-nodes*)
                (let ((n (assoc node *congestion-city-nodes*)))
                  (if (eql node *player-pos*)
                      (append n '(*))
                      n))
                (list node '?)))
          (remove-duplicates
           (append *visited-nodes*
                   (mapcan (lambda (node)
                             (mapcar #'car
                                     (cdr (assoc node
                                                 *congestion-city-edges*))))
                           *visited-nodes*)))))

;; Building a list of known edges
(defun known-city-edges ()
  (mapcar (lambda (node)
            (cons node (mapcar (lambda (x)
                                 (if (member (car x) *visited-nodes*)
                                     x
                                     (list (car x))))
                               (cdr (assoc node *congestion-city-edges*)))))
          *visited-nodes*))

;; Draws a graph of the parts of the city we have visited
(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))



;;----------------------------------------------------------->
;; Functions defuned for walking around the town, and playing the game

;; Allows the user to walk in a direction.
(defun walk (pos)
  (handle-direction pos nil))

;; Handels charging a location when the wumpus is thought to be found
(defun charge (pos)
  (handle-direction pos t))

;; Makes sure that the move is legal by checking the edges of the city.
(defun handle-direction (pos charging)
  (let ((edge (assoc pos
                     (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
        (handle-new-place edge pos charging)
      (princ "That location does not exist!"))))

;; Handles a players actual travel to a new place
(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
         (has-worm (and (member 'glow-worm node)
                        (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into the cops. Game over."))
          ((member 'wumpus node) (if charging
                                     (princ "You found the Wumpus!")
                                   (princ "You ran into the Wumpus!")))
          (charging (princ "You wasted your last bullet. Game Over."))
          (has-worm (let ((new-pos (random-node)))
                       (princ "You ran into a Glow Worm Gang! You are now at ")
                       (princ new-pos)
                       (handle-new-place nil new-pos nil))))))


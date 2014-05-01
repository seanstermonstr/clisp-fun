
;; Loads the graphing utility lib
(load "graph-util")
;; Global variables for the game
(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-offs* 15)

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

;;
(defun get-connected (node edge-list)
  (let ((visited nil))
    (lables ((traverse (node)
                       (unless (member node visited)
                         (push node visited)
                         (mapc (lambda (edge)
                                 (traverse (cdr edge)))
                               (direct-edges node edge-list)))))
            (traverse node))
    visited))

;;
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

;;
(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

;;
(defun connect-all-islands (nodes edge-list)
  (sppend (connect-with-bridges (find-islands nodes edge-list)) edge-list))


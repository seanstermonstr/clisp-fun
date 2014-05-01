(defparameter *drink-order* '((bill . double-espresso)
                              (lisa . small-drip-cofee)
                              (john . medium-latte)))

(defparameter *max-label-length* 30)

;; Defines the top level text for the game.  
(defparameter *wizard-nodes* '((living-room (you are in the living room.
                                          a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                                     There is a well in front of you.))
                        (attic (you are in the attic.
                                    there is a giant welding tourch in the corner.))))


;; Converts node identifiers
;; essentially changes forbidden characters to underscores
(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))


;; Generates the lable that should appear when the node is drawn
(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
          s))
    ""))


;;------------------------------------------->
;; Functions to generate all of the dot data for the lib

;; Calls the nodes->dot func and the edges->dot func
(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

;; Takes a list of nodes and generates dot information that encodes
;; them
(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

;; Generate the dot information for the edges of out graph
(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))


;;------------------------------------>
;; File stuff for the bitmapping etc.

;; Captures the DOT file stuff and stuff
(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                   fname
                   :direction :output
                   :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))

;; Actually creates the graph by calling correct functions
(defun graph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))


;;------------------------------------->
;; expands on the graphing utility to allow for drawing of undirected
;; graphs
;; Undirected version of edges->dot
(defun uedges->dot (edges)
  (maplist (lambda (1st)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr 1st))
                       (fresh-line)
                       (princ (dot-name (caar 1st)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\"];")))
                   (cdar 1st)))
           edges))

;; Undirected version of graph->dot
(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

;; Creates the png of the undirected graph
(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))

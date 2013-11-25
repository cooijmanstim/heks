(in-package :heks)

(declaim (optimize (safety 3) (debug 3)))

;; evaluations v are in [0,1] interpreted as probability of winning.  these are
;; transformed into integers by discretizing the domain into *evaluation-granularity*
;; buckets.  coarser domains allow more pruning.
(defparameter *evaluation-granularity* 40)
(defparameter *evaluation-minimum* 0)
(defparameter *evaluation-maximum* *evaluation-granularity*)

(defun granularize (v)
  (round (* *evaluation-granularity* v)))
(defun ungranularize (v)
  (/ v 1.0 *evaluation-granularity*))
(defun evaluation-inverse (v)
  (assert (= *evaluation-minimum* 0))
  (- *evaluation-maximum* v))

;; evaluations are from the point of view of whoever's turn it is

(defun evaluate-state (state moves)
  (if (null moves)
      0 ;; no moves, player loses
      (material-advantage state)))

;; ratio of pieces that is owned by the current player
(defun material-advantage (state)
  (let ((ours 0) (all 0))
    (iter (for tile at ij of (state-board state))
          (with-slots (object owner) tile
            (when (member object '(:man :king))
              (when (eq owner (state-player state))
                (incf ours))
              (incf all))))
    (/ ours all)))

(defparameter *minimax-maximum-depth* 100)

;; NOTE: we can use (state-hash state) rather than state as the hash-table key,
;; but then collisions can happen, and stored information may be invalid.  if
;; the stored move is invalid (highly likely), conditions will occur.
;; if the stored move is valid, we can still use it for move ordering.
;; TODO: measure collision rate
(defstruct transposition
  (depth 0 :type integer)
  (lower-bound 0 :type integer)
  (upper-bound 0 :type integer)
  (move nil :type list))
(sb-ext:define-hash-table-test state-equal state-hash)
(defparameter *transposition-table* nil)
;; store only deep transpositions to save memory.  shallow transpositions are used much more
;; often, but there are many more of them.  TODO: handle out of memory properly
(defparameter *transposition-minimum-depth* 2)

(defun make-transposition-table ()
  (make-hash-table :test 'state-equal))

(defun lookup-transposition (state &optional depth)
  (when-let ((transposition (gethash state *transposition-table*)))
    (if (and depth (<= depth (transposition-depth transposition)))
        nil
        transposition)))

(defun ensure-transposition (state)
  (if-let ((transposition (gethash state *transposition-table*)))
    transposition
    (setf (gethash (copy-state state) *transposition-table*)
          (make-transposition))))

(defparameter *killers* nil)

(defun make-killers ()
  (make-array (list *minimax-maximum-depth*) :initial-element '()))

(defun lookup-killers (depth valid-moves)
  (let ((killers (aref *killers* depth)))
    (intersection killers valid-moves)))

(defun store-killer (depth move)
  (deletef (aref *killers* depth) move)
  (push move (aref *killers* depth)))

(defun order-moves (depth state moves)
  ;; TODO: need something like this to get some novelty in the search at all when not
  ;; much is about to happen.  maybe do it more lightweight; move a few randomly chosen
  ;; elements to the front.  alternatively, add a bit of noise to the evaluation.
  (shuffle moves)
  (let ((prioritized-moves (lookup-killers depth moves)))
    (when-let ((transposition (lookup-transposition state)))
      (with-slots (move) transposition
        (push move prioritized-moves)))
    (if prioritized-moves
        (nconc prioritized-moves (nset-difference moves prioritized-moves :test #'move-equal))
        moves)))

(defun minimax (state depth max-depth evaluator
                &optional (alpha *evaluation-minimum*) (beta *evaluation-maximum*)
                &aux (original-alpha alpha))
  (when *out-of-time*
    (throw :out-of-time nil))
  (when-let ((transposition (lookup-transposition state depth)))
    ;; use stored values
    (with-slots (lower-bound upper-bound move)
        transposition
      ;; if the intersection between the present window and the previously
      ;; proven interval is empty, don't investigate further.
      (cond ((<= beta lower-bound)
             (return-from minimax (values lower-bound move)))
            ((<= upper-bound alpha)
             (return-from minimax (values upper-bound move))))
      (maxf alpha lower-bound)
      (minf beta upper-bound)))
  ;; investigate the subtree
  (let ((moves (moves state)))
    (when (or (>= depth max-depth) (null moves))
      (return-from minimax (values (granularize (funcall evaluator state moves)) nil)))
    (setf moves (order-moves depth state moves))
    (multiple-value-bind (value best-move)
        (iter (for move in moves)
              (for breadcrumb = (apply-move state move))
              (finding move maximizing (evaluation-inverse
                                        (minimax state (1+ depth) max-depth evaluator
                                                 (evaluation-inverse beta)
                                                 (evaluation-inverse alpha)))
                       into (best-move value))
              (unapply-move state breadcrumb)
              (maxf alpha value)
              (when (>= alpha beta)
                (store-killer depth move)
                (finish))
              (finally (return (values value best-move))))
      ;; store transposition
      (let ((subtree-height (- max-depth depth)))
        (when (>= subtree-height *transposition-minimum-depth*)
          (with-slots (depth lower-bound upper-bound move)
              (ensure-transposition state)
            ;; if the found value is outside or on the border of the search window,
            ;; it only proves a bound.
            (cond ((<= value original-alpha)
                   (setf upper-bound value))
                  ((<= beta value)
                   (setf lower-bound value))
                  ((< alpha value beta)
                   (setf lower-bound value
                         upper-bound value)))
            (setf depth subtree-height
                  move best-move))))
      (values value best-move))))

(defun minimax-decision (state &key (updater #'no-op) (committer #'no-op) (evaluator #'evaluate-state))
  (let (value best-move)
    (catch :out-of-time
      ;; use a fresh table with every top-level search
      (let ((*transposition-table* (make-transposition-table))
            (*killers* (make-killers))
            (state (copy-state state)))
        (iter (for max-depth from 1 below *minimax-maximum-depth*)
              (multiple-value-setq (value best-move) (minimax state 0 max-depth evaluator))
              (print (list max-depth (ungranularize value) best-move))
              (unless *out-of-time* ;; this *could* happen...
                (funcall updater best-move)))))
    (funcall committer)
    best-move))

(defun evaluation-decision (state &key (evaluator #'evaluate-state))
  (iter (for move in (moves state))
        (for breadcrumb = (apply-move state move))
        (finding move maximizing (funcall evaluator state (moves state))
                 into (best-move value))
        (unapply-move state breadcrumb)
        (finally (return best-move))))

(defstruct (mcts-node (:constructor nil))
  (move nil)
  (parent nil :type (or null mcts-node))
  (children '() :type list)
  (nwins 0 :type integer)
  (nvisits 0 :type integer)
  (untried-moves '() :type list)
  (last-player nil :type (or null player)))

(defun make-mcts-node (state &optional move parent)
  (let ((node (make-instance 'mcts-node)))
    (with-slots ((m move) (p parent) untried-moves last-player) node
      (setf m move
            p parent
            untried-moves (moves state)
            last-player (opponent (state-player state))))
    node))

(defun uct-child (node)
  (with-slots (children (parent-nvisits nvisits)) node
    (labels ((uct-score (node)
               (with-slots (nwins nvisits) node
                   (+ (/ nwins nvisits)
                      (sqrt (/ (* 2 (log parent-nvisits))
                               nvisits))))))
      (extremum children #'> :key #'uct-score))))

(defun add-child (parent move state)
  (let ((node (make-mcts-node state move parent)))
    (with-slots (untried-moves children) parent
      (setf untried-moves (delete move untried-moves :test #'move-equal))
      (push node children))
    node))

(defun mcts-update (node winner)
  (with-slots (last-player nvisits nwins) node
    (incf nvisits)
    (incf nwins (if (null winner)
                    1/2
                    (if (eq winner last-player)
                        1
                        0)))))

(defparameter *mcts-maximum-depth* 200)
(defun mcts-decision (root-state &optional (n most-positive-fixnum))
  (assert (not (state-endp root-state)))
  (let ((root-node (make-mcts-node root-state)))
    (labels ((select (node state)
               (iter (while (and (emptyp (mcts-node-untried-moves node))
                                 (not (emptyp (mcts-node-children node)))))
                     (setq node (uct-child node))
                     (apply-move state (mcts-node-move node)))
               node)
             (expand (node state)
               (with-slots (untried-moves) node
                 (unless (emptyp untried-moves)
                   (let ((move (random-elt untried-moves)))
                     (apply-move state move)
                     (setf node (add-child node move state)))))
               node)
             (rollout (state)
               (iter (for moves = (moves state))
                     (for depth from 0)
                     (until (emptyp moves))
                     ;; no winner
                     (when (> depth *mcts-maximum-depth*)
                       (return-from rollout nil))
                     (apply-move state (random-elt moves)))
               (state-winner state))
             (backprop (node winner)
               (iter (while node)
                     (mcts-update node winner)
                     (setq node (mcts-node-parent node)))))
      (iter (repeat n)
            (until *out-of-time*)
            (for node = root-node)
            (for state = (copy-state root-state))
            (setf node (select node state))
            (setf node (expand node state))
            (backprop node (rollout state))))
    (with-slots (children) root-node
      (mcts-node-move (extremum children #'> :key #'mcts-node-nvisits)))))

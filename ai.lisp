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
(defparameter *transposition-minimum-depth* 5)

(defun lookup-transposition (state)
  (gethash state *transposition-table*))

(defun ensure-transposition (state)
  (if-let ((transposition (gethash state *transposition-table*)))
    transposition
    (setf (gethash (copy-state state) *transposition-table*)
          (make-transposition))))

(defparameter *out-of-time* nil)
(defparameter *minimax-decision-time* 10)

(defun order-moves (state moves)
  (if-let ((transposition (lookup-transposition state)))
    (with-slots (move) transposition
      (cons move (delete move moves :test #'move-equal)))
    moves))

(defun minimax (state depth evaluator
                &optional (alpha *evaluation-minimum*) (beta *evaluation-maximum*)
                &aux (original-alpha alpha))
  (when *out-of-time*
    (throw :out-of-time nil))
  (when-let ((transposition (lookup-transposition state)))
    ;; use stored values
    (with-slots ((d depth) lower-bound upper-bound move)
        transposition
      (when (>= d depth)
        (cond ((<= beta lower-bound)
               (return-from minimax (values lower-bound move)))
              ((<= upper-bound alpha)
               (return-from minimax (values upper-bound move))))
        (maxf alpha lower-bound)
        (minf beta upper-bound))))
  ;; investigate the subtree
  (let ((moves (moves state)))
    (when (or (= depth 0) (null moves))
      (return-from minimax (values (granularize (funcall evaluator state moves)) nil)))
    ;; TODO: put moves in a vector for faster reordering?
    (shuffle moves)
    (order-moves state moves)
    (multiple-value-bind (value best-move)
        (iter (for move in moves)
              (for breadcrumb = (apply-move state move))
              (finding move maximizing (evaluation-inverse
                                        (minimax state (1- depth) evaluator
                                                 (evaluation-inverse beta)
                                                 (evaluation-inverse alpha)))
                       into (best-move value))
              (unapply-move state breadcrumb)
              (maxf alpha value)
              (when (>= alpha beta)
                (finish))
              (finally (return (values value best-move))))
      ;; store transposition
      (when (>= depth *transposition-minimum-depth*)
        (with-slots ((d depth) lower-bound upper-bound move)
            (ensure-transposition state)
          (cond ((<= value original-alpha)
                 (setf upper-bound value))
                ((<= beta value)
                 (setf lower-bound value))
                ((< alpha value beta)
                 (setf lower-bound value
                       upper-bound value)))
          (setf d depth move best-move)))
      (values value best-move))))

(defun minimax-decision (state &key (duration 10)
                         (updater #'no-op)
                         (committer #'no-op)
                         (evaluator #'evaluate-state))
  (setq *out-of-time* nil)
  (sb-thread:make-thread
   (lambda ()
     (sleep duration)
     (setq *out-of-time* t)
     (sb-thread:thread-yield))
   :name "timer thread")
  (let (value best-move)
    (catch :out-of-time
      ;; use a fresh table with every top-level search
      (let ((*transposition-table* (make-hash-table :test 'state-equal))
            (state (copy-state state)))
        (iter (for depth from 0)
              (multiple-value-setq (value best-move) (minimax state depth evaluator))
              (print (list depth (ungranularize value) best-move))
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

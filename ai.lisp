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

;; an end-state is a state where no more moves can be made. the player to move loses.
(defun evaluate-end-state (state)
  (declare (ignore state))
  (granularize 0))

;; for now, just ratio of pieces that is player's
(defun evaluate-state (state)
  (granularize
   (let ((ours 0) (all 0))
     (iter (for i from 1 below (- *board-size* 1))
           (iter (for j from 1 below (- *board-size* 1))
                 (with-slots (object owner)
                     (board-tile (state-board state) (v i j))
                   (when (member object '(:man :king))
                     (when (eq owner (state-player state))
                       (incf ours))
                     (incf all)))))
     (/ ours all))))

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

(defun order-moves (state moves)
  (if-let ((transposition (lookup-transposition state)))
    (with-slots (move) transposition
      (cons move (delete move moves :test #'move-equal)))
    moves))

(defun minimax (state depth alpha beta &aux (original-alpha alpha))
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
  (when (= depth 0)
    (return-from minimax (values (evaluate-state state) nil)))
  (let ((moves (moves state)))
    ;; XXX: is there a more efficient way to check for end state?
    (when (null moves)
      (return-from minimax (values (evaluate-end-state state) nil)))
    ;; TODO: put moves in a vector for faster reordering
    (shuffle moves)
    (order-moves state moves)
    (multiple-value-bind (value best-move)
        (iter (for move in moves)
              (for breadcrumb = (apply-move state move))
              (finding move maximizing (evaluation-inverse
                                        (minimax state (1- depth)
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

(defun minimax-decision (state duration update-callback commit-callback)
  (setq *out-of-time* nil)
  (sb-thread:make-thread
   (lambda ()
     (sleep duration)
     (setq *out-of-time* t)
     (sb-thread:thread-yield)
     (funcall commit-callback))
   :name "timer thread")
  (sb-thread:make-thread
   (lambda ()
     (catch :out-of-time
       ;; use a fresh table with every top-level search
       (let ((*transposition-table* (make-hash-table :test 'state-equal)))
         (iter (for depth from 0)
               (with value) (with best-move)
               (multiple-value-setq (value best-move)
                 (minimax state depth *evaluation-minimum* *evaluation-maximum*))
               (print (list depth (ungranularize value) best-move))
               (unless *out-of-time* ;; this *could* happen...
                 (funcall update-callback best-move))))))
   :name "worker-thread"))

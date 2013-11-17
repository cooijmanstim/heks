(in-package :heks)

(declaim (optimize (safety 3) (debug 3)))

;; evaluations v are in [0,1] interpreted as probability of winning.  these are
;; transformed into integers by discretizing the domain into *evaluation-granularity*
;; buckets.  coarser domains allow more pruning.
(defparameter *evaluation-granularity* 10)
(defun granularize (v)
  (round (* *evaluation-granularity* v)))

;; an end-state is a state where no more moves can be made. the player to move loses.
(defun evaluate-end-state (state player)
  (granularize
   (if (eq (state-player state) player)
       0
       1)))

;; for now, just ratio of pieces that is player's
(defun evaluate-state (state player)
  (granularize
   (let ((ours 0) (all 0))
     (iter (for i from 1 below (- *board-size* 1))
           (iter (for j from 1 below (- *board-size* 1))
                 (for owner = (tile-owner (board-tile (state-board state) (v i j))))
                 (when (eq owner player)
                   (incf ours))
                 (incf all)))
     (/ ours all))))

;; NOTE: we can use (state-hash state) rather than state as the hash-table key,
;; but then collisions can happen, and stored information may be invalid.  if
;; the stored move is invalid (highly likely), conditions will occur.
;; if the stored move is valid, we can still use it for move ordering.
(defstruct transposition
  (depth 0 :type integer)
  (lower-bound 0 :type integer)
  (upper-bound 0 :type integer)
  (move nil :type list))
(sb-ext:define-hash-table-test state-equal state-hash)
(defparameter *transposition-table* nil)

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

(defun minimax (state depth alpha beta toplevel-player &aux (original-alpha alpha))
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
    (return-from minimax (values (evaluate-state state toplevel-player) nil)))
  (let ((moves (moves state)))
    ;; XXX: is there a more efficient way to check for end state?
    (when (null moves)
      (return-from minimax (values (evaluate-end-state state toplevel-player) nil)))
    (order-moves state moves)
    (multiple-value-bind (value best-move)
        (iter (for move in moves)
              (for breadcrumb = (apply-move state move))
              (finding move
                       maximizing (- (minimax state (1- depth) (- beta) (- alpha) toplevel-player))
                       into (best-move value))
              (unapply-move state breadcrumb)
              (maxf alpha value)
              (when (>= alpha beta)
                (finish))
              (finally (return (values value best-move))))
      ;; store transposition
      (with-slots ((d depth) lower-bound upper-bound move)
          (ensure-transposition state)
        (cond ((<= value original-alpha)
               (setf upper-bound value))
              ((<= beta value)
               (setf lower-bound value))
              ((< alpha value beta)
               (setf lower-bound value
                     upper-bound value)))
        (setf d depth move best-move))
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
                 (minimax state depth
                          most-negative-fixnum most-positive-fixnum
                          (state-player state)))
               (print (list depth value best-move))
               (unless *out-of-time* ;; this *could* happen...
                 (funcall update-callback best-move))))))
   :name "worker-thread"))


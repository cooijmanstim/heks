(in-package :heks)

(declaim (optimize (debug 3)))


;; TODO: let evaluations be in [0,1] interpreted as probability of winning.
;; then introduce a parameter *granularity* and transform the evaluations as
;; (round (* *granularity* evaluation)). this gives control over the granularity
;; of the evaluations; a coarser domain allows more pruning.


;; evaluation is from the perspective of player-to-move

;; an end-state is a state where no more moves can be made. the
;; player to move loses.
(defun evaluate-end-state (state)
  (declare (ignore state))
  -1)

;; just count advantage in number of pieces
(defun evaluate-state (state)
  (iter (for i from 1 below (- *board-size* 1))
        (summing
         (iter (for j from 1 below (- *board-size* 1))
               (for owner = (tile-owner (board-tile (state-board state) (v i j))))
               (summing (cond ((eq owner (state-player state)) 1)
                              ((eq owner (opponent (state-player state))) -1)
                              (t 0)))))))

(defparameter *out-of-time* nil)

(defun minimax (state depth alpha beta)
  (when *out-of-time*
    (throw :out-of-time nil))
  (if (= depth 0)
      (evaluate-state state)
      (let ((moves (moves state)))
        (if (null moves)
            (evaluate-end-state state)
            (iter (for move in moves)
                  (for breadcrumb = (apply-move state move))
                  (finding move
                           maximizing (- (minimax state (1- depth) (- beta) (- alpha)))
                           into (best-move value))
                  (unapply-move state breadcrumb)
                  (setq alpha (max alpha value))
                  (when (>= alpha beta)
                    (finish))
                  (finally (return (values value best-move))))))))

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
       (iter (for depth from 0)
             (with value) (with best-move)
             (multiple-value-setq (value best-move)
               (minimax state depth most-negative-fixnum most-positive-fixnum))
             (print (list depth value best-move))
             (unless *out-of-time* ;; this *could* happen...
               (funcall update-callback best-move)))))
   :name "worker-thread"))


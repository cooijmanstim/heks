(in-package :heks)

(declaim (optimize (debug 3)))

;; the vector encompasses four bitboards: one for each combination of white/black
;; and man/king.
;; these are used as inputs to a neural network later.  the representation is redundant,
;; but expected to be easier to deal with for a neural net.
;; NOTE: in the end, we want the evaluation to be symmetric, that is:
;;   (= v-from-whites-perspective (evaluation-inverse v-from-blacks-perspective))
;; and vice versa.  this is a constraint that we can use to guide training of the net.
(defun state->vector (state)
  (with-slots (board (self player)) state
    ;; FIXME: magic numbers
    (let* ((vector (make-array (* 4 61) :element-type 'single-float :fill-pointer 0 :adjustable t))
           (opponent (opponent self)))
      (iter (for signified-object in '(:man :king))
            (iter (for signified-owner in (list self opponent))
                  (iter (for tile at ij of board from self)
                        (with-slots (object owner) tile
                          (vector-push (if (and (eq object signified-object)
                                                (eq owner  signified-owner))
                                           1.0
                                           0.0)
                                       vector)))))
      vector)))

(defun measure-performance (player opponent n)
  (let ((sample (iter (repeat (/ n 2))
                      (collect (if (match-players player opponent) 1 0))
                      (collect (if (match-players opponent player) 0 1)))))
    (values (mean sample) (standard-deviation sample :biased nil))))

;; let player1 (white) play a game against player2 (black)
;; return t if white won, nil otherwise
(defun match-players (player1 player2)
  (declare (optimize debug))
  ;; players is a circular list of the two players
  (let ((players (list player1 player2)))
    (rplacd (last players) players)
    (iter (with state = (make-initial-state))
          (with breadcrumbs = '()) ;; debug info
          (for player in players)
          (for moves = (moves state))
          (for n from 0)
          (until (null moves))
          ;; punish taking too long
          (when (> n 1000)
            (return nil))
          (let ((move (funcall player state)))
            (assert (member move moves :test #'move-equal))
            (push (apply-move state move) breadcrumbs))
          (finally
           (return (eq (state-player state) *black-player*))))))

(defun generate-states (n fn)
  (let ((observation-rate 0.05))
    (iter (with state = (make-initial-state))
          (for moves = (moves state))
          (while (> n 0))
          (when (null moves)
            (setf state (make-initial-state))
            (next-iteration))
          (when (< (random 1.0) observation-rate)
            (funcall fn state)
            (decf n))
          (apply-move state (random-elt moves)))))

(defun generate-mcts-evaluated-state-vectors (n mcts-sample-budget fn)
  (generate-states n
                   (lambda (state)
                     (multiple-value-bind (move move-value state-value)
                         (mcts-decision state :max-sample-size mcts-sample-budget)
                       (declare (ignore move state-value))
                       (let ((state-vector (state->vector state)))
                         (vector-push-extend move-value state-vector)
                         (funcall fn state-vector))))))

(defun dump-mcts-evaluated-state-vectors (n mcts-sample-budget file-name)
  (with-output-to-file (stream file-name :if-exists :overwrite :if-does-not-exist :create)
      (generate-mcts-evaluated-state-vectors
       n mcts-sample-budget
       (lambda (state-vector)
         (format-vector stream state-vector)))))


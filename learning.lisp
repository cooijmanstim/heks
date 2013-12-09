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
           (return (eq (state-player state) :black))))))

(defun format-vector (stream v)
  (fresh-line stream)
  (iter (for elt in-vector v)
        (format stream "~A " elt)))

(defun load-array-from-file (file-name)
  (with-open-file (stream file-name)
    (iter (for line = (read-line stream nil nil))
          (for row = (make-array 100 :element-type 'single-float :fill-pointer 0))
          (while line)
          (collecting (mapcar (lambda (string)
                                ;; sigh.
                                (coerce (read-from-string string) 'single-float))
                              (cl-ppcre:all-matches-as-strings "\\S+" line))

                      into rows)
          (finally
           (return (make-array (list (length rows) (length (first rows)))
                               :element-type 'single-float
                               :adjustable nil
                               :initial-contents rows))))))

;; minimization through simultaneous perturbation stochastic approximation
;; returns final theta and a list of previous thetas, most recent first
;; TODO: integrate this with the granularity thing, in which case we should
;; ensure thetas always sum to 1. some may be negative, but as long as
;; they sum to 1 the linear combination of features will be in [0,1]
;; choose c approximately equal to the standard deviation of the measurements fn
(defun spsa (n fn theta &key (c 0.5))
  ;; if initial theta is zero, it won't move
  (assert (not (every #'zerop theta)))
  (let ((a 1) (bigA (/ n 10)) (alpha 0.602) (gamma 0.101)
        (thetas (list theta)))
    (iter (for k from 1 to n)
          (for ak = (/ a (expt (+ k bigA) alpha)))
          (for ck = (/ c (expt k gamma)))
          (for delta = (iter (for x in-vector theta)
                             (collect (- (* (random 2) 2) 1))))
          (for theta- = (vector-plus theta (scale-vector (- ck) delta)))
          (for theta+ = (vector-plus theta (scale-vector (+ ck) delta)))
          (for y- = (funcall fn theta-))
          (for y+ = (funcall fn theta+))
          (for g^ = (map '(vector single-float)
                         (lambda (d)
                           (/ (- y+ y-) 2 ck d))
                         delta))
          (for dtheta = (scale-vector (* -1 ak) g^))
          ;; constrain dtheta to have norm at most twice that of theta
          ;; slows convergence at worst, enables convergence at best
          (let ((dtheta-norm (vector-norm dtheta))
                (dtheta-norm-max (* 2 (vector-norm theta))))
            (when (> dtheta-norm dtheta-norm-max)
              (setf dtheta (scale-vector (/ dtheta-norm-max dtheta-norm) dtheta))))
          (setf theta (vector-plus theta dtheta))
          (push theta thetas)
          (print (list k theta)))
    (values theta thetas)))

;; perform moves according to a fast mcts decision, calling fn with the state every now and then
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


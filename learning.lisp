(in-package :heks)

;; each state is written on a separate line, as a space-separated vector of bits.
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
    (let* ((vector (make-array (* 4 61) :element-type 'bit :fill-pointer 0))
           (opponent (opponent self)))
      (iter (for signified-object in '(:man :king))
            (iter (for signified-owner in (list self opponent))
                  (iter (for tile at ij of board from self)
                        (with-slots (object owner) tile
                          (vector-push (if (and (eq object signified-object)
                                                (eq owner  signified-owner))
                                           1
                                           0)
                                       vector)))))
      vector)))

(defun format-vector (stream v)
  (fresh-line stream)
  (iter (for elt in-vector v)
        (format stream "~A " elt)))

(defun measure-performance (player opponent n)
  (/ (iter (repeat n)
           (counting (match-players player opponent)))
     n))

;; return t if player1 won, nil otherwise
(defun match-players (player1 player2)
  ;; players is a circular list of the two players
  (let ((players (list player1 player2)))
    (rplacd (last players) players)
    (iter (with state = (make-initial-state))
          (with breadcrumbs = '()) ;; debug info
          (for player in players)
          (until (null moves))
          (let ((moves (moves state))
                (move (funcall player state)))
            (assert (member move moves :test #'move-equal))
            (push (apply-move state move) breadcrumbs))
          (finally (return (eq player player2))))))

(defun no-op (&rest args)
  (declare (ignore args)))

(defun make-minimax-player (evaluation-fn)
  (lambda (state)
    (let ((*minimax-evaluator* evaluation-fn))
      (minimax-decision state #'no-op #'no-op))))

(defun make-learned-evaluator (weights)
  (lambda (state)
    (reduce #'+ (map '(vector single-float) #'*
                     weights (state->vector state)))))

;; minimization through simultaneous perturbation stochastic approximation
;; TODO: integrate this with the granularity thing, in which case we should
;; ensure thetas always sum to 1. some may be negative, but as long as
;; they sum to 1 the linear combination of features will be in [0,1]
(defun spsa (n fn theta)
  (labels ((apply-delta (xs dxs scale)
             (map '(vector single-float)
                  (lambda (x dx)
                    (+ x (* scale dx)))
                  xs dxs)))
    ;; these are taken from "Multivariate stochastic approximation using a simultaneous
    ;; perturbation gradient approximation" by Spall. there is no literature on how to
    ;; pick these, it seems.
    (let ((a 300) (c 100) (bigA 2) (alpha 1) (gamma 0.25))
      (iter (for k from 1 to n)
            (for ak = (/ a (expt (+ k bigA) alpha)))
            (for ck = (/ c (expt k gamma)))
            (for delta = (iter (for x in theta)
                               (collect (- (* (random 2) 2) 1))))
            (for theta- = (apply-delta theta delta (- ck)))
            (for theta+ = (apply-delta theta delta ck))
            (for y- = (funcall fn theta-))
            (for y+ = (funcall fn theta+))
            (for g^ = (map '(vector single-float)
                           (lambda (d)
                             (/ (- y+ y-) 2 ck d))
                           delta))
            (iter (for i index-of-vector theta)
                  (decf (aref theta i) (* ak (aref g^ i)))))))
  theta)


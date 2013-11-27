(in-package :heks)

(declaim (optimize (debug 3)))

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

(defun make-learned-evaluator (weights)
  (lambda (state moves)
    (declare (ignore moves))
    (dot-product weights (features (state->vector state)))))

(defun dot-product (us vs)
  (assert (length= us vs))
  (iter (for u in-vector us)
        (for v in-vector vs)
        (summing (* u v))))

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

(defparameter *featuremap-mean* (load-array-from-file "/home/tim/school/isg/pca_mean"))
(defparameter *featuremap-map* (load-array-from-file "/home/tim/school/isg/pca_map"))

(defun features (x)
  (destructuring-bind (m n) (array-dimensions *featuremap-map*)
    (iter (for j from 0 below n)
          (collecting (iter (for i from 0 below m)
                            (summing (* (aref *featuremap-map* i j)
                                        (- (aref x i) (aref *featuremap-mean* 0 i)))))
                      result-type (vector single-float)))))

;; minimization through simultaneous perturbation stochastic approximation
;; returns final theta and a list of previous thetas, most recent first
;; TODO: integrate this with the granularity thing, in which case we should
;; ensure thetas always sum to 1. some may be negative, but as long as
;; they sum to 1 the linear combination of features will be in [0,1]
;; choose c approximately equal to the standard deviation of the measurements fn
(defun spsa (n fn theta &key (c 0.5))
  ;; if initial theta is zero, all dthetas will be too large
  (assert (not (every #'zerop theta)))
  (labels ((apply-delta (xs dxs scale)
             (map '(vector single-float)
                  (lambda (x dx)
                    (+ x (* scale dx)))
                  xs dxs)))
    (let ((a 1) (bigA (/ n 10)) (alpha 0.602) (gamma 0.101)
          (thetas (list theta)))
      (iter (for k from 1 to n)
            (for ak = (/ a (expt (+ k bigA) alpha)))
            (for ck = (/ c (expt k gamma)))
            (for delta = (iter (for x in-vector theta)
                               (collect (- (* (random 2) 2) 1))))
            (for theta- = (apply-delta theta delta (- ck)))
            (for theta+ = (apply-delta theta delta ck))
            (for y- = (funcall fn theta-))
            (for y+ = (funcall fn theta+))
            (for g^ = (map '(vector single-float)
                           (lambda (d)
                             (/ (- y+ y-) 2 ck d))
                           delta))
            (for dtheta = (map '(vector single-float)
                               (lambda (g^)
                                 (* -1 ak g^))
                               g^))
            ;; repeatedly halve dtheta until ||dtheta|| is not stupidly large
            ;; slows convergence at worst, enables convergence at best
            (iter (for dtheta-norm = (vector-norm dtheta))
                  (with theta-norm = (vector-norm theta))
              (while (> dtheta-norm (* 2 theta-norm)))
              (iter (for i index-of-vector dtheta)
                    (setf (aref dtheta i) (/ (aref dtheta i) 2))))
            (setf theta (map '(vector single-float) #'+ theta dtheta))
            (push theta thetas)
            (print theta))
      (values theta thetas))))

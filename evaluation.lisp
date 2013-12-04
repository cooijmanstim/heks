(in-package :heks)

(declaim (optimize (debug 3) (safety 3)))

(deftype evaluation () 'single-float)

(defparameter *evaluation-maximum* (min (abs most-negative-single-float) (abs most-positive-single-float)))
(defparameter *evaluation-minimum* (- *evaluation-maximum*))
(declaim (evaluation *evaluation-maximum* *evaluation-minimum*))

(defun evaluation-inverse (v)
  (- v))

(declaim (ftype (function (*) fixnum) piece-value))

(defun heuristic-evaluation (state)
  (let ((king-value    3)
        (men-ours      0)
        (kings-ours    0)
        (men-theirs    0)
        (kings-theirs  0)
        (capturability-ours   0)
        (capturability-theirs 0))
    (with-slots ((us player) board) state
      (declare (optimize (speed 3) (safety 0))
               (fixnum
                men-ours kings-ours
                men-theirs kings-theirs
                capturability-ours capturability-theirs)
               (board board))
      (macrolet ((count* (men kings capturability)
                   `(case object
                      (:man
                       (incf ,men)
                       (incf ,capturability
                             ;; number of axes along which the piece has empties on both sides
                             (iter (for direction in half-of-directions)
                                   (counting (and (tile-empty-p board (v+v ij direction))
                                                  (tile-empty-p board (v-v ij direction)))))))
                      (:king (incf ,kings)))))
        (iter (for tile at ij of board)
              (with them = (opponent us))
              (with half-of-directions = (player-forward-directions :white))
              (with-slots (object owner) tile
                (unless (eq object :empty)
                  (cond ((eq owner us)
                         (count* men-ours kings-ours capturability-ours))
                        ((eq owner them)
                         (count* men-theirs kings-theirs capturability-theirs))))))))
    (+
     ;; material advantage
     (- (+ men-ours   (* king-value kings-ours))
        (+ men-theirs (* king-value kings-theirs)))
     ;; difference in average capturability of men
     (* 0.25 (- (if (zerop men-theirs)
                    0
                    (/ capturability-theirs men-theirs))
                (if (zerop men-ours)
                    0
                    (/ capturability-ours   men-ours)))))))

(declaim (ftype (function * evaluation) heuristic-evaluation evaluate-state))

(defun evaluate-state (state moves)
  (if (null moves)
      *evaluation-minimum* ;; no moves, player loses
      (heuristic-evaluation state)))

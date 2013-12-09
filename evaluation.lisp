(in-package :heks)

(declaim (optimize (debug 3) (safety 3)))

(deftype evaluation () 'fixnum)
(defparameter *evaluation-granularity* 16) ; on both sides of zero
(defparameter *evaluation-minimum* (- *evaluation-granularity*))
(defparameter *evaluation-maximum* *evaluation-granularity*)
(declaim (evaluation *evaluation-minimum* *evaluation-maximum*)
         (fixnum *evaluation-granularity*))

(defun evaluation-inverse (v)
  (+ *evaluation-minimum*
     (- *evaluation-maximum*
        v)))

(defun granularize (x)
  (declare (optimize (speed 3) (safety 1))
           (single-float x))
  ;; scale x because x/(1+|x|) saturates too early
  (let ((x (/ x 8)))
    (round (* *evaluation-granularity*
              (/ x (1+ (abs x)))))))

(declaim (ftype (function (state) single-float) heuristic-evaluation))
(defun heuristic-evaluation (state)
  (let* ((king-value    5)
         (men           (vector 0 0))
         (kings         (vector 0 0))
         (capturability (vector 0 0))
         (us (state-player state))
         (them (opponent us))
         (board (state-board state)))
    (declare (optimize (speed 3) (safety 1))
             (board board))
    (iter (for tile at ij of board)
          (with-slots (object owner) tile
            (case object
              (:man (incf (svref men owner))
                    (incf (svref capturability owner)
                          ;; number of axes along which the piece has empties on both sides
                          (the fixnum (iter (for direction in (player-forward-directions *white-player*))
                                            (counting (and (tile-empty-p board (v+v ij direction))
                                                           (tile-empty-p board (v-v ij direction))))))))
              (:king (incf (svref kings owner))))))
    (labels ((material-score (player)
               (+ (svref men player) (* king-value (svref kings player))))
             (average-capturability (player)
               (if (zerop (svref men player))
                   0
                   (/ (svref capturability player) (svref men player)))))
      (+
       (* 1.0 (- (material-score us)
                 (material-score them)))
       (* 0.5 (- (average-capturability them)
                 (average-capturability us)))))))

(declaim (ftype (function (state list) evaluation) evaluate-state))
(defun evaluate-state (state moves)
  (if (null moves)
      *evaluation-minimum* ;; no moves, player loses
      (granularize (heuristic-evaluation state))))



;; TODO
;(defstruct running-evaluation
;  (men   #(0 0) :type simple-vector)
;  (kings #(0 0) :type simple-vector))
;
;(defun update-running-evaluation (evaluation state breadcrumb)
;  ;; NOTE: after last move, when state-player was opponent
;  (with-slots (opponent) state
;    (let ((player (opponent opponent)))
;      (with-slots (men kings) evaluation
;        (with-slots (capture-tiles crowned) breadcrumb
;          (dolist (tile capture-tiles)
;            (with-slots (object) tile
;              (case object
;                (:man  (decf (svref men   opponent)))
;                (:king (decf (svref kings opponent))))))
;          (when crowned
;            (decf (svref men   player))
;            (incf (svref kings player))))))))


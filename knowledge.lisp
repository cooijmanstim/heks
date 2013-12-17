(in-package :heks)

(declaim (optimize (debug 3) (safety 3)))

(deftype evaluation () 'fixnum)
(defparameter *evaluation-maximum* (min (abs most-negative-fixnum) (abs most-positive-fixnum)))
(defparameter *evaluation-minimum* (- *evaluation-maximum*))
(declaim (evaluation *evaluation-minimum* *evaluation-maximum*))

(defun evaluation-inverse (v)
  (+ *evaluation-minimum*
     (- *evaluation-maximum*
        v)))

(declaim (ftype (function (single-float &optional fixnum) evaluation) granularize))
(defun granularize (x &optional (granularity 16))
  (declare (optimize (speed 3) (safety 0)))
  (round (* (/ granularity 2)
            (/ x (1+ (abs x))))))

(defparameter *king-value* 10)
(declaim (fixnum *king-value*))

(defun count-material (state)
  (let* ((men   (vector 0 0))
         (kings (vector 0 0))
         (board (state-board state)))
    (declare (optimize (speed 3) (safety 0))
             (board board))
    (iter (for tile at ij of board)
          (with-slots (object owner) tile
            (case object
              (:man  (incf (the fixnum (svref men   owner))))
              (:king (incf (the fixnum (svref kings owner)))))))
    (values men kings)))

(declaim (ftype (function (state list) evaluation) material-evaluation))
(defun material-evaluation (state moves)
  (declare (optimize (speed 3) (safety 0))
           (ignore moves))
  (multiple-value-bind (men kings) (count-material state)
    (let* ((us (state-player state))
           (them (opponent us)))
      (labels ((material-score (player)
                 (+ (the fixnum (svref men player)) (the fixnum (* *king-value* (the fixnum (svref kings player)))))))
        (- (material-score us) (material-score them))))))

(declaim (ftype (function (v player) single-float) tile-weight))
(defun tile-weight (ij owner)
  (declare (v ij)
           (optimize (speed 3) (safety 1))
           (player owner))
  (let ((end (- *board-size* 1)))
    (declare (fixnum end))
    (when (= owner *black-player*)
      (setf ij (s-v end ij)))
    (coerce (/ (- (vnorm (v-v ij (v 1 1))) ;; get away from home
                  (vnorm (v-v ij (v end end))) ;; toward opponent
                  (min (vnorm (v-v ij (v end 1))) ;; into corners/sides
                       (vnorm (v-v ij (v 1 end)))))
               end)
            'single-float)))

(declaim (ftype (function (state list) evaluation) heuristic-evaluation))
(defun heuristic-evaluation (state moves)
  (declare (optimize (speed 3) (safety 1))
           (ignore moves))
  (let* ((men           (vector 0 0))
         (kings         (vector 0 0))
         (capturability (vector 0 0))
         (positional-score (vector 0.0 0.0))
         (us (state-player state))
         (them (opponent us))
         (board (state-board state)))
    (declare (board board))
    (iter (for tile at ij of board)
          (with-slots (object owner) tile
            (case object
              (:man (incf (the fixnum (svref men owner)))
                    (incf (the single-float (svref positional-score owner)) (tile-weight ij owner))
                    (incf (the fixnum (svref capturability owner))
                          ;; number of axes along which the piece has empties on both sides
                          (the fixnum (iter (for direction in (player-forward-directions *white-player*))
                                            (counting (and (tile-empty-p board (v+v ij direction))
                                                           (tile-empty-p board (v-v ij direction))))))))
              (:king (incf (the fixnum (svref kings owner)))))))
    (labels ((material-score (player)
               (+ (the fixnum (svref men player)) (the fixnum (* *king-value* (the fixnum (svref kings player))))))
             (average-capturability (player)
               (if (zerop (the fixnum (svref men player)))
                   0.0
                   (/ (the fixnum (svref capturability player)) (the fixnum (svref men player)) 1.0)))
             (average-positional-score (player)
               (if (zerop (the fixnum (svref men player)))
                   0.0
                   (/ (the single-float (svref positional-score player)) (the fixnum (svref men player))))))
      (declare (ftype (function (player) fixnum) material-score)
               (ftype (function (player) single-float) average-capturability))
      (granularize (* 1/8
                      (+ (coerce (- (material-score us)
                                    (material-score them))
                                 'single-float)
                         (* 0.5 (- (average-capturability them)
                                   (average-capturability us)))
                         (* 4 (the single-float (symmetric-sigmoid (/ (- (average-positional-score them)
                                                                         (average-positional-score us))
                                                                      8))))))
                   16))))

(defstruct running-material
  (men   (vector 0 0) :type simple-vector)
  (kings (vector 0 0) :type simple-vector))

(defun initialize-running-material (material state)
  (unless material
    (setf material (make-running-material)))
  (with-slots (men kings) material
    (multiple-value-setq (men kings) (count-material state)))
  material)

(defun date-running-material (valence material state breadcrumb)
  ;; called after the move has been applied and before it is unapplied,
  ;; so the player to move is not the player that made the move.
  (with-slots ((opponent player)) state
    (let ((player (opponent opponent)))
      (with-slots (men kings) material
        (with-slots (capture-tiles crowned) breadcrumb
          (dolist (tile capture-tiles)
            (with-slots (object) tile
              (case object
                (:man  (decf (svref men   opponent) valence))
                (:king (decf (svref kings opponent) valence)))))
          (when crowned
            (decf (svref men   player) valence)
            (incf (svref kings player) valence))))))
  material)

(defun update-running-material (material state move breadcrumb)
  (declare (ignore move))
  (date-running-material  1 material state breadcrumb))
(defun downdate-running-material (material state move breadcrumb)
  (declare (ignore move))
  (date-running-material -1 material state breadcrumb))

(defun get-material-advantage (material player)
  (with-slots (men kings) material
    (labels ((material-score (player)
               (+ (svref men player)
                  (* *king-value* (svref kings player)))))
      (- (material-score player) (material-score (opponent player))))))

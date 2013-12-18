(in-package :heks)

(declaim (optimize (debug 3) (safety 3)))

(deftype evaluation () 'fixnum)
(defparameter *evaluation-maximum* (min (abs most-negative-fixnum) (abs most-positive-fixnum)))
(defparameter *evaluation-minimum* (- *evaluation-maximum*))
(declaim (evaluation *evaluation-minimum* *evaluation-maximum*))

(declaim (inline evaluation-inverse))
(defun evaluation-inverse (v)
  (- v))

(defparameter *tile-weights*
  (let* ((board (make-empty-board))
         (weights (make-array (append '(2) (array-dimensions board)))))
    (labels ((proximity (u v)
               (exp (- (vnorm (v-v u v))))))
      (iter (for tile at ij of board)
            (iter (for player in (list *white-player* *black-player*))
                  (for ij2 in (list ij (s+v -1 (v-v *board-dimensions* ij))))
                  (setf (aref weights player (s1 ij) (s2 ij))
                        (+ 1
                           ;; mix a couple of gaussians in there
                           (* 1 (proximity ij2 (v 9 9)))  ;; go toward opponent
                           (* 2 (proximity ij2 (v 1 5)))  ;; go toward corners
                           (* 2 (proximity ij2 (v 5 1)))
                           (* 2 (proximity ij2 (v 9 5)))
                           (* 2 (proximity ij2 (v 5 9))))))))
    weights))

(declaim (ftype (function (v player) single-float) tile-weight))
(defun tile-weight (ij owner)
  (aref *tile-weights* owner (s1 ij) (s2 ij)))

(defparameter *feature-weights*
  #(
   1.083451156943683
  -0.062543846281054
   0.055784069291807
  -0.675006007019073
   0.409534634004255
  -0.063101550011102
   0.043101573338307
   0.261765181484584
  -0.205680376270151
  ))

(defun heuristic-features (state)
  (declare (optimize (speed 3) (safety 1)))
  (let* ((men           (vector 0 0))
         (kings         (vector 0 0))
         (weighted-men  (vector 0.0 0.0))
         (capturability (vector 0 0))
         (us (state-player state))
         (them (opponent us))
         (board (state-board state)))
    (declare (board board)
             (dynamic-extent men kings weighted-men capturability))
    (iter (for tile at ij of board)
          (with-slots (object owner) tile
            (case object
              (:man (incf (the fixnum (svref men owner)))
                    (incf (the single-float (svref weighted-men owner)) (tile-weight ij owner))
                    (incf (the fixnum (svref capturability owner))
                          ;; number of axes along which the piece has empties on both sides
                          (the fixnum (iter (for direction in (player-forward-directions *white-player*))
                                            (counting (and (tile-empty-p board (v+v ij direction))
                                                           (tile-empty-p board (v-v ij direction))))))))
              (:king (incf (the fixnum (svref kings owner)))))))
    (labels ((average-capturability (player)
               (if (zerop (svref men player))
                   0.0
                   (/ (svref capturability player)
                      1.0 (svref men player)))))
      (vector 1
              (svref men   us) (svref men   them)
              (svref kings us) (svref kings them)
              (svref weighted-men us) (svref weighted-men them)
              (average-capturability us) (average-capturability them)))))

(declaim (ftype (function (state list) evaluation) heuristic-evaluation))
(defun heuristic-evaluation (state moves)
  (declare (ignore moves))
  (let ((probability (exp (- (dot-product *feature-weights* (heuristic-features state))))))
    (values
     ;; center and discretize
     (round (* (- probability 0.5)
               100))
     probability)))


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

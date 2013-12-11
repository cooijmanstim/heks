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
  (declare (optimize (speed 3) (safety 1)))
  (round (* (/ granularity 2)
            (/ x (1+ (abs x))))))

(defparameter *king-value* 5)
(declaim (fixnum *king-value*))

(defun count-material (state)
  (let* ((men   (vector 0 0))
         (kings (vector 0 0))
         (board (state-board state)))
    (declare (optimize (speed 3) (safety 1))
             (board board))
    (iter (for tile at ij of board)
          (with-slots (object owner) tile
            (case object
              (:man  (incf (the fixnum (svref men   owner))))
              (:king (incf (the fixnum (svref kings owner)))))))
    (values men kings)))

(declaim (ftype (function (t state) evaluation) material-evaluation))
(defun material-evaluation (data state)
  (declare (optimize (speed 3) (safety 0))
           (ignore data))
  (multiple-value-bind (men kings) (count-material state)
    (let* ((us (state-player state))
           (them (opponent us)))
      (labels ((material-score (player)
                 (+ (the fixnum (svref men player)) (the fixnum (* *king-value* (the fixnum (svref kings player)))))))
        (- (material-score us) (material-score them))))))

(declaim (ftype (function (t state) evaluation) heuristic-evaluation))
(defun heuristic-evaluation (data state)
  (declare (optimize (speed 3) (safety 0))
           (ignore data))
  (let* ((men           (vector 0 0))
         (kings         (vector 0 0))
         (capturability (vector 0 0))
         (us (state-player state))
         (them (opponent us))
         (board (state-board state)))
    (declare (board board))
    (iter (for tile at ij of board)
          (with-slots (object owner) tile
            (case object
              (:man (incf (the fixnum (svref men owner)))
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
                   (/ (the fixnum (svref capturability player)) (the fixnum (svref men player)) 1.0))))
      (declare (ftype (function (player) fixnum) material-score)
               (ftype (function (player) single-float) average-capturability))
      (granularize (* 1/8
                      (+ (coerce (- (material-score us)
                                    (material-score them))
                                 'single-float)
                         (* 0.5 (- (average-capturability them)
                                   (average-capturability us)))))
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

(defun get-running-material (material state moves)
  (declare (ignore moves))
  (with-slots (men kings) material
    (with-slots (player) state
      (labels ((material-score (player)
                 (+ (svref men player)
                    (* *king-value* (svref kings player)))))
        (- (material-score player) (material-score (opponent player)))))))

;; updater/downdater are called with data as first argument and return the new value
(defstruct evaluator
  data
  ;; called with the initial state
  (initializer nil :type (or null (function (t state) t)))
  ;; called after a move is applied to state
  (updater nil :type (or null (function (t state list breadcrumb) t)))
  ;; called before a move is unapplied to state
  (downdater nil :type (or null (function (t state list breadcrumb) t)))
  ;; called to evaluate a state
  (getter (constantly 0) :type (function (t state list) evaluation)))

(defun evaluation* (evaluator state)
  (with-slots (initializer data) evaluator
    (when initializer
      (setf data (funcall initializer data state)))))

(defun evaluation+ (evaluator state move breadcrumb)
  (with-slots (updater data) evaluator
    (when updater
      (setf data (funcall updater data state move breadcrumb)))))

(defun evaluation- (evaluator state move breadcrumb)
  (with-slots (downdater data) evaluator
    (when downdater
      (setf data (funcall downdater data state move breadcrumb)))))

(defun evaluation? (evaluator state moves)
  (if (null moves)
      *evaluation-minimum*
      (with-slots (getter data) evaluator
        (funcall getter data state moves))))

(defun make-material-evaluator ()
  (make-evaluator :initializer #'initialize-running-material
                  :updater #'update-running-material
                  :downdater #'downdate-running-material
                  :getter #'get-running-material))

(defun make-slow-material-evaluator ()
  (make-evaluator :getter #'material-evaluation))

(defun make-heuristic-evaluator ()
  (make-evaluator :getter #'heuristic-evaluation))

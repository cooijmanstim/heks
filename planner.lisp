(in-package :heks)

(declaim (optimize debug safety))

(defparameter *tb-ma-sample-budget* 1000)
(defparameter *tb-ma-sample-depth* 8)
(defparameter *tb-ml-sample-budget* 500)

;; collect *tb-ma-sample-budget* samples of material advantage after
;; *tb-ma-sample-depth* random moves
(defun sample-material-advantages (state)
  (let ((material (make-running-material))
        (sample (make-array *tb-ma-sample-budget* :fill-pointer 0))
        (state (copy-state state))
        (player (state-player state)))
    (initialize-running-material material state)
    (iter (repeat *tb-ma-sample-budget*)
          (iter (with breadcrumbs = '())
                (with move-history = '())
                (repeat *tb-ma-sample-depth*)
                (for moves = (moves state))
                (when (state-endp state)
                  (finish))
                (let* ((move (random-elt moves))
                       (breadcrumb (apply-move state move)))
                  (push breadcrumb breadcrumbs)
                  (push move move-history)
                  (update-running-material material state move breadcrumb))
                (finally
                 (vector-push (get-material-advantage material player) sample)
                 (iter (for breadcrumb in breadcrumbs)
                       (for move in move-history)
                       (downdate-running-material material state move breadcrumb)
                       (unapply-move state breadcrumb)))))
    sample))

;; keep a decaying average of playout lengths.  might overestimate due to inertia,
;; but without it we drown in noise.
(defun estimate-moves-left (agent state)
  (with-slots (moves-left-mean) agent
    (iter (repeat *tb-ml-sample-budget*)
          (for move-count = (iter (with state = (copy-state state))
                                  (for moves = (moves state))
                                  (until (emptyp moves))
                                  (count t)
                                  (apply-move state (random-elt moves))))
          (setf moves-left-mean (+ (* 0.8 moves-left-mean)
                                   (* 0.2 move-count))))
    moves-left-mean))

(defun time-budget (planner state)
  (let* ((sample (sample-material-advantages state))
         (variance (coerce (variance sample) 'single-float)))
    (with-slots (variance-mean variance-sample-size time-left) planner
      (update-running-average variance-mean variance-sample-size variance)
      (let* ((moves-left (estimate-moves-left planner state))
             (mean-move-time (/ time-left moves-left))
             (extra-variance (- variance variance-mean))
             ;; when moves-left >= 2, importance ranges from 1/2 (low variance) to 2 (high variance)
             (importance (expt (min 2 moves-left) (symmetric-sigmoid (/ extra-variance 4))))
             (time-budget (* mean-move-time importance)))
        (fresh-line)
        (print (list :time-left time-left
                     :moves-left moves-left
                     :mean-move-time mean-move-time
                     :extra-variance extra-variance
                     :importance importance
                     :time-budget time-budget))
        time-budget))))

(defclass planner ()
  ((time-left :type single-float :initarg :total-time :initform most-positive-single-float)
   (variance-mean :initform 0.0 :type single-float)
   (variance-sample-size :initform 0 :type integer)
   (moves-left-mean :initform 0.0 :type single-float)))

(defmethod initialize ((planner planner) state)
  ;; initialize mean variance estimate
  (dotimes (i 10)
    (time-budget planner state)))

(defun plan-and-decide (planner state decision-fn)
  (with-slots (time-left) planner
    (labels ((duration (t0 t1)
               (coerce (/ (- t1 t0)
                          internal-time-units-per-second)
                       'single-float)))
      (let (time t0 t1 t2 values)
        (setf t0 (get-internal-real-time))
        (unwind-protect
             (progn
               (setf time (time-budget planner state))
               (setf t1 (get-internal-real-time))
               (setf values (multiple-value-list (funcall decision-fn time))))
          (setf t2 (get-internal-real-time))
          (print (list :planning-time (duration t0 t1)
                       :decision-time (duration t1 t2)))
          (decf time-left (duration t0 t2)))
        (values-list values)))))

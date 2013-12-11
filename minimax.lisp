(in-package :heks)

(declaim (optimize (safety 3) (debug 3)))

(defparameter *minimax-maximum-depth* 30)

(defun make-table (&key (size 100))
  (make-hash-table :size size))
(defun table-size (table)
  (hash-table-size table))
(declaim (inline lookup (setf lookup)))
(defun lookup (table key)
  (multiple-value-bind (data presentp) (gethash key table)
    (if presentp
        (values data t)
        (values nil nil))))
(defun (setf lookup) (data table key)
  (setf (gethash key table) data))

(defparameter *moves-cache* nil)
(defparameter *moves-cache-size-estimate* 1000000)

(defun make-moves-cache ()
  (make-table :size *moves-cache-size-estimate*))

(defparameter *moves-cache-statistics* (vector 0 0)) ;; hits, misses

(defun lookup-moves (state)
  (multiple-value-bind (moves presentp) (lookup *moves-cache* (state-hash state))
    (cond (presentp
           (incf (svref *moves-cache-statistics* 0))
           moves)
          (t
           (incf (svref *moves-cache-statistics* 1))
           (setf (lookup *moves-cache* (state-hash state)) (moves state))))))

;; NOTE: we can use (state-hash state) rather than state as the hash-table key,
;; but then collisions can happen, and stored information may be invalid.  if
;; the stored move is invalid (highly likely), conditions will occur.
;; if the stored move is valid, we can still use it for move ordering.
(defstruct transposition
  (depth 0 :type fixnum)
  (value 0 :type evaluation)
  (type nil :type (or null (member :exact :lower :upper)))
  (move nil :type list))
(sb-ext:define-hash-table-test state-equal state-hash)
(defparameter *transposition-table* nil)
(defparameter *transposition-table-size-estimate* 1000000)
;; store only deep transpositions to save memory.  shallow transpositions are used much more
;; often, but there are many more of them.
(defparameter *transposition-minimum-depth* 2)
(declaim (fixnum *transposition-minimum-depth*))

(defun make-transposition-table ()
  (make-table :size *transposition-table-size-estimate*))

(defun lookup-transposition (state &optional depth)
  (when-let ((transposition (lookup *transposition-table* (state-hash state))))
    (if (and depth (>= depth (transposition-depth transposition)))
        nil
        transposition)))

(defun ensure-transposition (state)
  (if-let ((transposition (lookup *transposition-table* (state-hash state))))
    transposition
    (setf (lookup *transposition-table* (state-hash state)) (make-transposition))))

(defparameter *killer-table* nil)
(defparameter *max-killers* 2)

(defun make-killers ()
  (make-array (list *minimax-maximum-depth*) :initial-element '()))

(defun lookup-killers (ply valid-moves)
  (let ((killers (aref *killer-table* ply)))
    (intersection killers valid-moves)))

(defun store-killer (ply move)
  (symbol-macrolet ((killers (aref *killer-table* ply)))
    (deletef killers move :test #'move-equal)
    (push move killers)
    (when-let ((max-cdr (nthcdr (1- *max-killers*) killers)))
      (rplacd max-cdr nil))))

(defun order-moves (depth ply state moves)
  ;; use transposition move regardless of depth
  (declare (ignore depth))
  (append (when-let* ((transposition (lookup-transposition state))
                      (move (transposition-move transposition)))
            (list move))
          (lookup-killers ply moves)
          ;; move a few random moves forward to get nondeterminism
          ;; alternatively, add a bit of noise to the evaluation function
          (list (random-elt moves)
                (random-elt moves)
                (random-elt moves))
          moves))

(defstruct minimax-statistics
  (mean-branching-factor 0.0 :type single-float)
  (branching-factor-sample-size 0 :type fixnum)
  (mean-move-count 0.0 :type single-float)
  (move-count-sample-size 0 :type fixnum)
  (node-count 0 :type fixnum))
(defparameter *minimax-statistics* nil)

;; XXX: mean and n are places that are evaluated more than once
(defmacro update-running-average (mean n newvalue)
  `(progn
     (setf ,mean (+ (* (/ ,n (+ 1.0 ,n)) ,mean)
                    (* (/  1 (+ 1.0 ,n)) ,newvalue)))
     (incf ,n)))

(declaim (inline measure-branching-factor
                 measure-moves
                 measure-node))
(defun measure-branching-factor (branching-factor)
  (when *minimax-statistics*
    (with-slots ((mean mean-branching-factor) (n branching-factor-sample-size)) *minimax-statistics*
      (declare (fixnum n branching-factor)
               (single-float mean))
      (update-running-average mean n branching-factor))))

(defun measure-moves (moves)
  (when *minimax-statistics*
    (with-slots ((mean mean-move-count) (n move-count-sample-size)) *minimax-statistics*
      (declare (fixnum n)
               (single-float mean)
               (list moves))
      (update-running-average mean n (length moves)))))

(defun measure-node ()
  (when *minimax-statistics*
    (incf (minimax-statistics-node-count *minimax-statistics*))))

;; depth counts down as we recurse, ply counts up. transposition lookup depends on depth
;; (of the subtree it summarizes) whereas killer move lookup depends on ply.
(defun minimax (state depth ply evaluator
                &optional (alpha *evaluation-minimum*) (beta *evaluation-maximum*)
                &aux (original-alpha alpha))
  (declare (optimize (speed 3) (safety 0))
           (fixnum depth ply)
           (evaluator evaluator)
           (evaluation alpha beta))
  (when *out-of-time*
    (throw :out-of-time nil))
  (labels ((maybe-use-transposition ()
             (when-let ((transposition (lookup-transposition state depth)))
               ;; use stored values
               ;; NOTE: in the case of a state hash collision it is possible that the
               ;; transposition does not apply to the current state.  hence the conclusion
               ;; drawn here may be entirely incorrect.
               ;; furthermore, an illegal move may be returned from here, but this never
               ;; happens at the toplevel so it should not cause a crash.  at worst the
               ;; user is presented with a principal variation that ends in an illegal move.
               (with-slots (value type move)
                   transposition
                 (declare (evaluation value))
                 (ccase type
                   (:exact (return-from minimax (values value (list move))))
                   (:lower (maxf alpha value))
                   (:upper (minf beta value)))
                 (when (>= alpha beta)
                   (return-from minimax (values value (list move)))))))
           (maybe-store-transposition (state alpha beta value depth variation)
             (declare (evaluation alpha beta value)
                      (fixnum depth))
             ;; store transposition
             (when (>= depth *transposition-minimum-depth*)
               (with-slots ((stored-depth depth)
                            (stored-value value)
                            (stored-type type)
                            (stored-move move))
                   (ensure-transposition state)
                 ;; if the found value is outside or on the border of the search window,
                 ;; it only proves a bound
                 (cond ((<= value alpha)
                        (setf stored-type :upper))
                       ((<= beta value)
                        (setf stored-type :lower))
                       (t
                        (setf stored-type :exact)))
                 (setf stored-value value
                       stored-depth depth
                       stored-move (first variation))))))
    (maybe-use-transposition)
    ;; investigate the subtree
    ;; NOTE: moves are cached in *moves-cache* and collisions between states are possible.
    (let ((moves (lookup-moves state)))
      (when (or (<= depth 0) (null moves))
        (return-from minimax (values (evaluation? evaluator state moves) '())))
      (measure-node)
      (measure-moves moves)
      (setf moves (order-moves depth ply state moves))
      (macrolet ((with-tentative-move (&body body)
                   `(let ((breadcrumb (apply-move state move)))
                      (when (null breadcrumb)
                        ;; apply-move returns nil (or doesn't return at all) when an illegal
                        ;; move is applied.  we only get illegal moves if the state hash
                        ;; collides with the hash of another state.
                        (warn "hash collision detected")
                        (next-iteration))
                      ;; the move has been applied, make sure it is unapplied on exit
                      (unwind-protect
                           (progn
                             (evaluation+ evaluator state move breadcrumb)
                             ;; the evaluator has been updated, make sure it is downdated
                             (unwind-protect
                                  (progn ,@body)
                               (evaluation- evaluator state move breadcrumb)))
                        (unapply-move state breadcrumb)))))
        (iter (for move in moves)
              (with branching-factor = 0)
              (with principal-variation = (list (first moves)))
              (with-tentative-move
                  (let ((current-variation '())
                        (current-value *evaluation-minimum*))
                    (declare (fixnum branching-factor)
                             (evaluation current-value))
                    (labels ((update-current (alpha beta)
                               (declare (evaluation alpha beta))
                               (multiple-value-bind (subvalue subvariation)
                                   (minimax state
                                            (the fixnum (1- depth))
                                            (the fixnum (1+ ply))
                                            evaluator
                                            (evaluation-inverse beta)
                                            (evaluation-inverse alpha))
                                 (declare (evaluation subvalue))
                                 (setf current-value (evaluation-inverse subvalue)
                                       current-variation (cons move subvariation))))
                             (expand ()
                               (incf branching-factor)
                               (update-current alpha beta)))
                      (if-first-time
                       (expand)
                       ;; PVS/Negascout
                       (let* ((pvs-alpha alpha) (pvs-beta (1+ pvs-alpha))
                              (*minimax-statistics* nil))
                         (declare (evaluation pvs-alpha pvs-beta))
                         (update-current pvs-alpha pvs-beta)
                         ;; search properly if current value inaccurate
                         (when (and (<= pvs-beta current-value) (< current-value beta))
                           (expand)))))
                    (when (< alpha current-value)
                      (setf alpha current-value
                            principal-variation current-variation))))
              (when (>= alpha beta)
                (store-killer ply move)
                (finish))
              (finally
               (measure-branching-factor branching-factor)
               (maybe-store-transposition state original-alpha beta alpha depth principal-variation)
               (when (null (first principal-variation))
                 (break))
               (return (values alpha principal-variation))))))))
  
(defun minimax-decision (state evaluator &key (verbose t))
  (let ((moves (moves (copy-state state))))
    (when (= (length moves) 1)
      (return-from minimax-decision (first moves))))
  (sb-ext:gc :full t)
  (unwind-protect
       (let (value
             variation
             (*transposition-table* (make-transposition-table))
             (*killer-table* (make-killers))
             (*minimax-statistics* (make-minimax-statistics))
             (*moves-cache* (make-moves-cache))
             (state (copy-state state)))
         (catch :out-of-time
           (iter (for max-depth from 0 below *minimax-maximum-depth*)
                 (multiple-value-setq (value variation) (minimax state max-depth 0 evaluator))
                 (print (list max-depth value variation))))
         (let ((table-statistics
                (list :ply-nkillers (map 'vector #'length *killer-table*))))
           (when verbose
             (print (list variation *minimax-statistics* table-statistics)))
           (maxf *transposition-table-size-estimate* (table-size *transposition-table*))
           (maxf *moves-cache-size-estimate*         (table-size *moves-cache*))
           (values (first variation) value)))
    (sb-ext:gc :full t)))


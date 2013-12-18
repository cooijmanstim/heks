(in-package :heks)

(declaim (optimize (safety 3) (debug 3)))

(defparameter *minimax-maximum-depth* 30)

(defstruct minimax-statistics
  (mean-branching-factor 0.0)
  (branching-factor-sample-size 0)
  (mean-move-count 0.0)
  (move-count-sample-size 0)
  (node-count 0))

(declaim (inline measure-branching-factor
                 measure-moves
                 measure-node))
(defun measure-branching-factor (statistics branching-factor)
  (with-slots ((mean mean-branching-factor) (n branching-factor-sample-size)) statistics
    (declare (fixnum n branching-factor)
             (single-float mean))
    (update-running-average mean n branching-factor)))

(defun measure-moves (statistics moves)
  (with-slots ((mean mean-move-count) (n move-count-sample-size)) statistics
    (declare (fixnum n)
             (single-float mean)
             (list moves))
    (update-running-average mean n (length moves))))

(defun measure-node (statistics)
  (incf (minimax-statistics-node-count statistics)))

(defparameter *minimax-statistics* nil)
(defparameter *transposition-table* nil)
(defparameter *killer-table* nil)

;; depth counts down as we recurse, ply counts up. transposition lookup depends on depth
;; (of the subtree it summarizes) whereas killer move lookup depends on ply.
(defun minimax (state depth ply evaluator
                &optional (alpha *evaluation-minimum*) (beta *evaluation-maximum*)
                &aux (original-alpha alpha))
  (declare (optimize (speed 3) (safety 1))
           (fixnum depth ply)
           (evaluator evaluator)
           (evaluation alpha beta))
  (when *out-of-time*
    (throw :out-of-time nil))
  (labels ((maybe-use-transposition (state depth)
             (when-let* ((table *transposition-table*)
                         (transposition (lookup-transposition table state depth)))
               ;; use stored values
               ;; NOTE: in the case of a state hash collision it is possible that the
               ;; transposition does not apply to the current state.  hence the conclusion
               ;; drawn here may be entirely incorrect.
               ;; furthermore, an illegal move may be returned from here, but this never
               ;; happens at the toplevel so it should not cause a crash.  at worst the
               ;; user is presented with a principal variation that ends in an illegal move.
               ;; furtherfurthermore, the set of moves stored with the transposition may
               ;; contain illegal moves.
               ;; most of the time an illegal move is tried, it will be caught by apply-move.
               (with-slots (value type move moves)
                   transposition
                 (declare (evaluation value))
                 (ccase type
                   (:exact (return-from minimax (values value (list move))))
                   (:lower (maxf alpha value))
                   (:upper (minf beta value)))
                 (when (>= alpha beta)
                   (return-from minimax (values value (list move))))
                 (values move moves))))
           (maybe-store-transposition (state alpha beta value depth variation moves)
             (declare (evaluation alpha beta value)
                      (fixnum depth))
             (when *transposition-table*
               (with-slots ((stored-depth depth)
                            (stored-value value)
                            (stored-type type)
                            (stored-move move)
                            (stored-moves moves))
                   (ensure-transposition *transposition-table* state)
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
                       stored-move (first variation)
                       stored-moves moves))))
           (order-moves (depth ply state moves transposition-move)
             (declare (ignore depth state))
             (append (when transposition-move
                       (list transposition-move))
                     (when *killer-table* (lookup-killers *killer-table* ply moves))
                     ;; move a few random moves forward to get nondeterminism
                     ;; alternatively, add a bit of noise to the evaluation function
                     (list (random-elt moves)
                           (random-elt moves)
                           (random-elt moves))
                     moves)))
    (multiple-value-bind (transposition-move transposition-moves) (maybe-use-transposition state depth)
      ;; investigate the subtree
      (let ((moves (or transposition-moves (moves state))))
        (when (or (<= depth 0) (null moves))
          (return-from minimax (values (evaluation? evaluator state moves) '())))
        (when *minimax-statistics*
          (measure-node *minimax-statistics*)
          (measure-moves *minimax-statistics* moves))
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
          (iter (for move in (order-moves depth ply state moves transposition-move))
                (with branching-factor = 0)
                (with principal-variation = (list (first moves))) ; initialize with any move
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
                  (when *killer-table* (store-killer *killer-table* ply move))
                  (finish))
                (finally
                 (when *minimax-statistics*
                   (measure-branching-factor *minimax-statistics* branching-factor))
                 (maybe-store-transposition state original-alpha beta alpha depth principal-variation moves)
                 (when (null (first principal-variation))
                   (break))
                 (return (values alpha principal-variation)))))))))

(defun minimax-decision (state evaluator &key (verbose t))
  (let ((moves (moves (copy-state state))))
    (when (= (length moves) 1)
      (return-from minimax-decision (first moves))))
  (let (value
        variation
        (*minimax-statistics* (when verbose (make-minimax-statistics)))
        (state (copy-state state)))
    (catch :out-of-time
      (iter (for max-depth from 0 below *minimax-maximum-depth*)
            (multiple-value-setq (value variation) (minimax state max-depth 0 evaluator))
            (print (list max-depth value variation))))
    (when verbose
      (print (list variation *minimax-statistics*)))
    (values (first variation) value)))

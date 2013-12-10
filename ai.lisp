(in-package :heks)

(declaim (optimize (safety 3) (debug 3)))

(defparameter *minimax-maximum-depth* 30)

(defun make-table ()
  (hh-redblack:make-red-black-tree))

;; have to wrap it in a cons because hh-redblack can't store nil -_-
(declaim (inline lookup (setf lookup)))
(defun lookup (table key)
  (multiple-value-bind (data presentp) (hh-redblack:rb-get table key)
    (if presentp
        (values (first data) t)
        (values nil nil))))
(defun (setf lookup) (data table key)
  (hh-redblack:rb-put table key (list data))
  data)

(defparameter *moves-cache* nil)

(defun make-moves-cache ()
  (make-table))

(defun lookup-moves (state)
  (multiple-value-bind (moves presentp) (lookup *moves-cache* (state-hash state))
    (if presentp
      moves
      (setf (lookup *moves-cache* (state-hash state)) (moves state)))))

;; NOTE: we can use (state-hash state) rather than state as the hash-table key,
;; but then collisions can happen, and stored information may be invalid.  if
;; the stored move is invalid (highly likely), conditions will occur.
;; if the stored move is valid, we can still use it for move ordering.
;; TODO: measure collision rate
(defstruct transposition
  (depth 0 :type fixnum)
  (value 0 :type evaluation)
  (type nil :type (or null (member :exact :lower :upper)))
  (move nil :type list))
(sb-ext:define-hash-table-test state-equal state-hash)
(defparameter *transposition-table* nil)
;; store only deep transpositions to save memory.  shallow transpositions are used much more
;; often, but there are many more of them.  TODO: handle out of memory properly
(defparameter *transposition-minimum-depth* 2)
(declaim (fixnum *transposition-minimum-depth*))

(defun make-transposition-table ()
  (make-table))

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
      (iter (for move in moves)
            (with branching-factor = 0)
            (for breadcrumb = (apply-move state move))
            (with principal-variation = (list (first moves)))
            (when (null breadcrumb)
              ;; apply-move returns nil (or doesn't return at all) when an illegal
              ;; move is applied.  we only get illegal moves if the state hash
              ;; collides with the hash of another state.
              (warn "hash collision detected")
              (next-iteration))
            (evaluation+ evaluator state breadcrumb)
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
                 (let* ((pvs-alpha alpha)
                        (pvs-beta (1+ pvs-alpha))
                        (*minimax-statistics* nil))
                   (declare (evaluation pvs-alpha pvs-beta))
                   (update-current pvs-alpha pvs-beta)
                   ;; search properly if current value inaccurate
                   (when (and (<= pvs-beta current-value) (< current-value beta))
                     (expand)))))
              (evaluation- evaluator state breadcrumb)
              (unapply-move state breadcrumb)
              (when (< alpha current-value)
                (setf alpha current-value
                      principal-variation current-variation))
              (when (>= alpha beta)
                (store-killer ply move)
                (finish)))
            (finally
             (measure-branching-factor branching-factor)
             (maybe-store-transposition state original-alpha beta alpha depth principal-variation)
             (when (null (first principal-variation))
               (break))
             (return (values alpha principal-variation)))))))

(defun minimax-decision (state &key (updater #'no-op) (committer #'no-op) (evaluator (make-material-evaluator)) (verbose t))
  (let ((moves (moves (copy-state state))))
    (when (= (length moves) 1)
      (funcall updater (first moves))
      (funcall committer)
      (return-from minimax-decision (first moves))))
  (let (value
        variation
        (*transposition-table* (make-transposition-table))
        (*killer-table* (make-killers))
        (*minimax-statistics* (make-minimax-statistics))
        (*moves-cache* (make-moves-cache))
        (state (copy-state state)))
    (catch :out-of-time
      (iter (for max-depth from 0 below *minimax-maximum-depth*)
            (unwind-protect
                 (progn
                   (evaluation* evaluator state)
                   (multiple-value-setq (value variation) (minimax state max-depth 0 evaluator))
                   (print (list max-depth value variation)))
              (unless *out-of-time*
                (funcall updater (first variation))))))
    (funcall committer)
    (let ((table-statistics
           (list :ply-nkillers (map 'vector #'length *killer-table*))))
      (when verbose
        (print (list variation *minimax-statistics* table-statistics)))
      (values (first variation) value))))

(defun evaluation-decision (state &key (evaluator (make-material-evaluator)) (verbose t))
  (evaluation* evaluator state)
  (iter (for move in (moves state))
        (for breadcrumb = (apply-move state move))
        (evaluation+ evaluator state breadcrumb)
        (finding move maximizing (evaluation-inverse (evaluation? evaluator state (moves state)))
                 into (best-move value))
        (evaluation- evaluator state breadcrumb)
        (unapply-move state breadcrumb)
        (finally
         (when verbose
           (print (list value best-move)))
         (return best-move))))

(defstruct (mcts-node (:constructor nil))
  (move nil)
  (parent nil :type (or null mcts-node))
  (children '() :type list)
  (nwins 0 :type integer)
  (nvisits 0 :type integer)
  (untried-moves '() :type list)
  (last-player nil :type (or null player)))

(defun make-mcts-node (state &optional move parent)
  (let ((node (make-instance 'mcts-node)))
    (with-slots ((m move) (p parent) untried-moves last-player) node
      (setf m move
            p parent
            untried-moves (moves state)
            last-player (opponent (state-player state))))
    node))

(defun uct-child (node)
  (with-slots (children (parent-nvisits nvisits)) node
    (labels ((uct-score (node)
               (with-slots (nwins nvisits) node
                   (+ (/ nwins nvisits)
                      (sqrt (/ (* 2 (log parent-nvisits))
                               nvisits))))))
      (extremum children #'> :key #'uct-score))))

(defun add-child (parent move state)
  (let ((node (make-mcts-node state move parent)))
    (with-slots (untried-moves children) parent
      (setf untried-moves (delete move untried-moves :test #'move-equal))
      (push node children))
    node))

(defun mcts-update (node winner)
  (with-slots (last-player nvisits nwins) node
    (incf nvisits)
    (incf nwins (if (eq winner last-player)
                    1
                    0))))

(defparameter *mcts-maximum-depth* 200)
(defun mcts-decision (root-state &key (max-sample-size most-positive-fixnum) (verbose t))
  (assert (not (state-endp root-state)))
  (let ((root-node (make-mcts-node root-state)))
    (labels ((select (node state)
               (iter (while (and (emptyp (mcts-node-untried-moves node))
                                 (not (emptyp (mcts-node-children node)))))
                     (setq node (uct-child node))
                     (apply-move state (mcts-node-move node)))
               node)
             (expand (node state)
               (with-slots (untried-moves) node
                 (unless (emptyp untried-moves)
                   (let ((move (random-elt untried-moves)))
                     (apply-move state move)
                     (setf node (add-child node move state)))))
               node)
             (rollout (state)
               (iter (for moves = (moves state))
                     (for depth from 0)
                     (until (emptyp moves))
                     (apply-move state (random-elt moves)))
               (state-winner state))
             (backprop (node winner)
               (iter (while node)
                     (mcts-update node winner)
                     (setq node (mcts-node-parent node)))))
      (iter (repeat max-sample-size)
            (until *out-of-time*)
            (for node = root-node)
            (for state = (copy-state root-state))
            (setf node (select node state))
            (setf node (expand node state))
            (backprop node (rollout state))))
    (with-slots (children (root-nvisits nvisits) (root-nwins nwins)) root-node
      (let ((best-child (extremum children #'> :key #'mcts-node-nvisits)))
        (with-slots ((move-nvisits nvisits) (move-nwins nwins) move) best-child
          (let ((state-value (coerce (- 1 (/ root-nwins root-nvisits)) 'single-float))
                (move-value  (coerce      (/ move-nwins move-nvisits)  'single-float)))
            (when verbose
              (print (list :nsamples root-nvisits :state-value state-value :move-value move-value :move move)))
            (values move move-value state-value)))))))

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
  
(defun minimax-decision (state &key (evaluator (make-material-evaluator)) (verbose t))
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
                 (evaluation* evaluator state)
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

(defun evaluation-decision (state &key (evaluator (make-material-evaluator)) (verbose t))
  (evaluation* evaluator state)
  (iter (for move in (moves state))
        (for breadcrumb = (apply-move state move))
        (evaluation+ evaluator state move breadcrumb)
        (finding move maximizing (evaluation-inverse (evaluation? evaluator state (moves state)))
                 into (best-move value))
        (evaluation- evaluator state move breadcrumb)
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
  (last-player nil :type (or null player))
  (state-hash 0 :type zobrist-hash))

(defun make-mcts-node (state &optional move parent)
  (let ((node (make-instance 'mcts-node)))
    (with-slots ((m move) (p parent) untried-moves last-player state-hash) node
      (setf m move
            p parent
            untried-moves (if *moves-cache*
                              (lookup-moves state)
                              (moves state))
            last-player (opponent (state-player state))
            state-hash (state-hash state)))
    node))

(defun mcts-node-uct-child (parent)
  (with-slots (children (parent-nvisits nvisits)) parent
    (if (zerop parent-nvisits)
      (random-elt children)
      (labels ((uct-score (child)
                 (+ (mcts-node-win-rate child)
                    (sqrt (/ (* 2 (log parent-nvisits)
                                (mcts-node-nvisits child)))))))
        (extremum children #'> :key #'uct-score)))))

(defun mcts-node-best-child (node)
  ;; TODO: optimize
  (extremum (mcts-node-children node) #'> :key #'mcts-node-nvisits))

(declaim (ftype (function (mcts-node) single-float) mcts-node-win-rate))
(defun mcts-node-win-rate (node)
  (with-slots (nwins nvisits) node
    (if (zerop nvisits)
        0.5
        (coerce (/ nwins nvisits) 'single-float))))

(defun mcts-add-child (parent move state)
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

(defun mcts-sample (node state)
  (let ((state (copy-state state)))
    (setf node (mcts-select node state))
    (setf node (mcts-expand node state))
    (mcts-backprop node (mcts-rollout state))))

(defun mcts-select (node state)
  (iter (while (and (emptyp (mcts-node-untried-moves node))
                    (not (emptyp (mcts-node-children node)))))
        (setf node (mcts-node-uct-child node))
        (apply-move state (mcts-node-move node)))
  node)

(defparameter *mcts-expansion-rate* 2)

(defun mcts-expand (node state)
  (dotimes (i *mcts-expansion-rate*)
    (with-slots (untried-moves) node
      (unless (emptyp untried-moves)
        (let ((move (random-elt untried-moves)))
          (apply-move state move)
          (setf node (mcts-add-child node move state)))))
    node))

(defun mcts-rollout (state)
  (iter (for moves = (moves state))
        (until (emptyp moves))
        (apply-move state (random-elt moves)))
  (state-winner state))

(defun mcts-backprop (node winner)
  (iter (while node)
        (mcts-update node winner)
        (setf node (mcts-node-parent node))))

(defparameter *mcts-maximum-depth* 200)
(defun mcts-decision (root-state &key (max-sample-size most-positive-fixnum) (verbose t))
  (assert (not (state-endp root-state)))
  (let ((root-node (make-mcts-node root-state)))
    (iter (repeat max-sample-size)
          (until *out-of-time*)
          (mcts-sample root-node root-state))
    (let* ((best-child (mcts-node-best-child root-node))
           (move (mcts-node-move best-child))
           (value (mcts-node-win-rate best-child)))
      (when verbose
        (print (list :nsamples (mcts-node-nvisits root-node) :value value :move move)))
      (values move value))))

;; an evaluator that can be reused and will get better as it is reused.
;; uses evaluation and pondering time to build an mcts tree.

(defstruct pmcts-tree
  (root-node    nil :type (or null mcts-node))
  (current-node nil :type (or null mcts-node)))

(defun make-pmcts-tree-for-state (state)
  (let ((root-node (make-mcts-node state)))
    (make-pmcts-tree :root-node root-node :current-node root-node)))

(defun initialize-mcts-evaluation (tree state)
  ;; no-op, updater and downdater should make sure we are in sync
  (assert (= (state-hash state) (mcts-node-state-hash (pmcts-tree-current-node tree))))
  tree)

(defun update-pmcts-tree (tree state move breadcrumb)
  (declare (ignore breadcrumb))
  (with-slots (current-node) tree
    ;; traverse the list of children to find the node with the appropriate state-hash
    ;; (can't really do this more cheaply without keeping a map of some sort. not sure
    ;; if that is cheaper; there are fewer than twenty moves on average.)
    (dolist (child (mcts-node-children current-node))
      (when (= (state-hash state) (mcts-node-state-hash child))
        (setf current-node child)
        (mcts-sample current-node state)
        (return-from update-pmcts-tree tree)))
    ;; if we get here, the node hasn't been expanded yet.  do so.
    (setf current-node (mcts-add-child current-node move state))
    (mcts-sample current-node state))
  tree)

(defun downdate-pmcts-tree (tree state move breadcrumb)
  (declare (ignore state move breadcrumb))
  (with-slots (current-node) tree
    (setf current-node (mcts-node-parent current-node)))
  tree)

(defparameter *mcts-evaluation-support-mean*        0.0)
(defparameter *mcts-evaluation-support-sample-size* 0)
(declaim (single-float *mcts-evaluation-support-mean*)
         (fixnum *mcts-evaluation-support-sample-size*))

(declaim (ftype (function * evaluation) get-mcts-evaluation))
(defun get-mcts-evaluation (tree state moves)
  (declare (ignore moves)
           (optimize (speed 3) (safety 1)))
  (with-slots (current-node) tree
    (assert (= (state-hash state) (mcts-node-state-hash current-node)))
    ;; take one sample -- with results from previous turns this should be enough
    (mcts-sample current-node state)
    (update-running-average *mcts-evaluation-support-mean*
                            *mcts-evaluation-support-sample-size*
                            (mcts-node-nvisits current-node))
    (round (* 100 (- (mcts-node-win-rate (mcts-node-best-child current-node)) 0.5)))))

(defun make-mcts-evaluator (state)
  (make-evaluator :data (make-pmcts-tree-for-state state)
                  :initializer #'initialize-mcts-evaluation
                  :updater #'update-pmcts-tree
                  :downdater #'downdate-pmcts-tree
                  :getter #'get-mcts-evaluation))

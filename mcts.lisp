(in-package :heks)

(declaim (optimize (speed 3) (safety 1) (space 2)))

;; threads adding children simultaneously causes no end of trouble.  but don't store
;; a lock on each node; that would eat too much memory.
(defparameter *mcts-add-child-lock* (sb-thread:make-mutex :name "mcts-add-child-lock"))

(defstruct (mcts-node (:constructor nil))
  (move nil)
  (parent nil :type (or null mcts-node))
  (children '() :type list)
  (nwins 0 :type (integer 0))
  (nvisits 0 :type (integer 0))
  (untried-moves '() :type list)
  (last-player nil :type (or null player))
  (state-hash 0 :type zobrist-hash))

(defun make-mcts-node (state &optional move parent)
  (let ((node (make-instance 'mcts-node)))
    (with-slots ((m move) (p parent) untried-moves last-player state-hash) node
      (setf m move
            p parent
            untried-moves (moves state)
            last-player (opponent (state-player state))
            state-hash (state-hash state)))
    node))

(defparameter *prior-nvisits* 10)

(declaim (ftype (function (mcts-node &optional single-float) single-float) mcts-node-win-probability))
(defun mcts-node-win-probability (node &optional (prior 0.5))
  (let ((nwins (mcts-node-nwins node))
        (nvisits (mcts-node-nvisits node)))
    (/ (+ nwins   (* prior *prior-nvisits*))
       (+ nvisits *prior-nvisits*))))

;; use the plain win-rate in uct computations
(declaim (ftype (function (mcts-node) single-float) mcts-node-win-rate))
(defun mcts-node-win-rate (node)
  (let ((nwins (mcts-node-nwins node))
        (nvisits (mcts-node-nvisits node)))
    (if (zerop nvisits)
        0.5
        (coerce (/ (+ nwins) (+ nvisits)) 'single-float))))

(defparameter *uct-exploitation-weight* 1)
(defun mcts-node-uct (child parent-nvisits)
  (if (= parent-nvisits 0)
      ;; this case happens when the node is added by another thread but
      ;; that thread is not yet done with the first rollout. the children
      ;; will be in place though.
      0
      (+ (* *uct-exploitation-weight* (mcts-node-win-rate child))
         (sqrt (/ (* 1/4 (log (the (integer 1) parent-nvisits)))
                  ;; race conditions make it possible for child-nvisits to be 0 here
                  (+ 1e-4 (mcts-node-nvisits child)))))))

(defun mcts-node-select-child (parent)
  (declare (optimize (speed 3) (safety 1)))
  (let ((children (mcts-node-children parent))
        (parent-nvisits (mcts-node-nvisits parent)))
    (declare (type (integer 0) parent-nvisits))
    (if (= parent-nvisits 0)
        (random-list-elt children)
        (iter (for child in children)
              (for score = (mcts-node-uct child parent-nvisits))
              (finding child maximizing score)))))

(defun mcts-node-best-child (node)
  (declare (optimize (speed 3) (safety 1)))
  (iter (for child in (mcts-node-children node))
        (finding child maximizing (mcts-node-nvisits child))))

(defun mcts-add-child (parent move state)
  (let ((node (make-mcts-node state move parent)))
    (with-slots (untried-moves children) parent
      (deletef untried-moves move :test #'move-equal)
      (push node children))
    node))

(defun mcts-update (node winner)
  (with-slots (nwins nvisits last-player) node
    (incf nvisits)
    (incf nwins (if (eq winner last-player)
                    1
                    0))))

(define-condition mcts-node-state-mismatch-error (error)
  ((state :initarg :state)
   (node :initarg :node)))

(defun mcts-sample (node state)
  (unless (= (state-hash state) (mcts-node-state-hash node))
    (error 'mcts-node-state-mismatch-error :state state :node node))
  (let ((state (copy-state state)))
    (setf node (mcts-select node state))
    (setf node (mcts-expand node state))
    (mcts-backprop node (mcts-rollout state))))

(defun mcts-select (node state)
  (iter (while (and (emptyp (mcts-node-untried-moves node))
                    (not (emptyp (mcts-node-children node)))))
        (setf node (mcts-node-select-child node))
        (apply-move state (mcts-node-move node)))
  node)

(defparameter *mcts-expansion-rate* 5)

(defun mcts-expand (node state)
  (sb-thread:with-mutex (*mcts-add-child-lock*)
    (dotimes (i *mcts-expansion-rate* node)
      (let ((untried-moves (mcts-node-untried-moves node)))
        (unless (emptyp untried-moves)
          (let ((move (random-elt untried-moves)))
            (apply-move state move)
            (setf node (mcts-add-child node move state))))))))

(defun mcts-rollout (state)
  (iter (for moves = (moves state))
        (until (emptyp moves))
        (apply-move state (random-elt moves)))
  (state-winner state))

(defun mcts-backprop (node winner)
  (iter (while node)
        (mcts-update node winner)
        (setf node (mcts-node-parent node))))

(defun mcts-decision (root-state &key (root-node (make-mcts-node root-state)) (max-sample-size most-positive-fixnum) (verbose t))
  (assert (not (state-endp root-state)))
  (iter (repeat max-sample-size)
        (until *out-of-time*)
        (mcts-sample root-node root-state))
  (let* ((best-child (mcts-node-best-child root-node))
         (move (mcts-node-move best-child))
         (value (mcts-node-win-probability best-child)))
    (when verbose
      (print (list :nsamples (mcts-node-nvisits root-node) :value value :move move)))
    (values move value)))


;; persistent mcts tree
(defstruct pmcts-tree
  (root-node    nil :type (or null mcts-node))
  (current-node nil :type (or null mcts-node)))

(defun make-pmcts-tree-for-state (state)
  (let ((root-node (make-mcts-node state)))
    (make-pmcts-tree :root-node root-node :current-node root-node)))

(defun mcts-child-for-state (node state)
  ;; traverse the list of children to find the node with the appropriate state-hash
  ;; (can't really do this more cheaply without keeping a map of some sort. not sure
  ;; if that is cheaper; there are fewer than twenty moves on average.)
  (dolist (child (mcts-node-children node))
    (when (= (state-hash state) (mcts-node-state-hash child))
      (return-from mcts-child-for-state child))))

(defun update-pmcts-tree (tree state move breadcrumb)
  (declare (ignore breadcrumb))
  (with-slots (current-node) tree
    (setf current-node
          (or (mcts-child-for-state current-node state)
              (mcts-add-child current-node move state))))
  tree)

(defun downdate-pmcts-tree (tree state move breadcrumb)
  (declare (ignore state move breadcrumb))
  (with-slots (current-node) tree
    (setf current-node (mcts-node-parent current-node)))
  tree)

(defun pmcts-sample (tree state)
  (mcts-sample (slot-value tree 'current-node) state))

;; disconnect everything
(defun mcts-node-destroy (node)
  (declare (optimize (debug 1) (safety 1) (speed 3)))
  (let ((parent (mcts-node-parent node))
        (children (mcts-node-children node)))
    (setf (mcts-node-parent node) nil)
    (setf (mcts-node-children node) nil)
    (dolist (child children)
      (mcts-node-destroy child))
    (when parent
      (mcts-node-destroy parent))))

;; disconnect ancestors and siblings of node
(defun mcts-node-destroy-trunk (node)
  (let ((parent (mcts-node-parent node)))
    (setf (mcts-node-parent node) nil)
    (when parent
      (deletef (mcts-node-children parent) node)
      (mcts-node-destroy parent))))

;; set root-node to the levelth parent of current-node. the idea is that
;; nodes above it can be GCed.  minimax-pmcts will crash when this is done
;; and you then undo level moves.
(defun pmcts-tree-uproot (tree level)
  (with-slots (root-node current-node) tree
    (let ((new-root-node (do ((i 0 (1+ i))
                              (node current-node (or (mcts-node-parent node)
                                                     node)))
                             ((= i level) node))))
      (setf root-node new-root-node)
      (mcts-node-destroy-trunk new-root-node))))

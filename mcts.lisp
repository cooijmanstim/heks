(in-package :heks)

(declaim (optimize (debug 3) (safety 3)))

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
            untried-moves (if *moves-cache*
                              (lookup-moves state)
                              (moves state))
            last-player (opponent (state-player state))
            state-hash (state-hash state)))
    node))

(defun mcts-node-uct-child (parent)
  (declare (optimize (speed 3) (safety 1)))
  (with-slots (children (parent-nvisits nvisits)) parent
    (declare (type (integer 0) parent-nvisits))
    (if (= parent-nvisits 0)
      (random-elt children)
      (iter (for child in children)
            (for uct-score = (+ (mcts-node-win-rate child)
                                (sqrt (/ (* 2 (log (the (integer 1) parent-nvisits)))
                                         (mcts-node-nvisits child)))))
            (finding child maximizing uct-score)))))

(defun mcts-node-best-child (node)
  (declare (optimize (speed 3) (safety 1)))
  (iter (for child in (mcts-node-children node))
        (finding child maximizing (mcts-node-nvisits child))))

(declaim (ftype (function (mcts-node) single-float) mcts-node-win-rate))
(defun mcts-node-win-rate (node)
  (with-slots (nwins nvisits) node
    (if (zerop nvisits)
        0.5
        (coerce (/ nwins nvisits) 'single-float))))

(defun mcts-add-child (parent move state)
  (let ((node (make-mcts-node state move parent)))
    (with-slots (untried-moves children) parent
      (deletef untried-moves move :test #'move-equal)
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

(defun mcts-expand (node state)
  (with-slots (untried-moves) node
    (unless (emptyp untried-moves)
      (let ((move (random-elt untried-moves)))
        (apply-move state move)
        (setf node (mcts-add-child node move state)))))
  node)

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
(defun mcts-decision (root-state &key (root-node (make-mcts-node root-state)) (max-sample-size most-positive-fixnum) (verbose t))
  (assert (not (state-endp root-state)))
  (iter (repeat max-sample-size)
        (until *out-of-time*)
        (mcts-sample root-node root-state))
  (let* ((best-child (mcts-node-best-child root-node))
         (move (mcts-node-move best-child))
         (value (mcts-node-win-rate best-child)))
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

(defun update-pmcts-tree (tree state move breadcrumb)
  (declare (ignore breadcrumb))
  (with-slots (current-node) tree
    ;; traverse the list of children to find the node with the appropriate state-hash
    ;; (can't really do this more cheaply without keeping a map of some sort. not sure
    ;; if that is cheaper; there are fewer than twenty moves on average.)
    (let (foundp)
      (dolist (child (mcts-node-children current-node))
        (when (= (state-hash state) (mcts-node-state-hash child))
          (setf current-node child
                foundp t)))
      (unless foundp
        (setf current-node (mcts-add-child current-node move state)))))
  tree)

(defun downdate-pmcts-tree (tree state move breadcrumb)
  (declare (ignore state move breadcrumb))
  (with-slots (current-node) tree
    (setf current-node (mcts-node-parent current-node)))
  tree)

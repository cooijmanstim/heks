(in-package :heks)

(declaim (optimize (debug 3)))


;;; STATE MANIPULATION

;; place the object on ij, assume b empty
(defun place-piece* (board ij tile)
  (assert (eq (tile-object (board-tile board ij)) :empty))
  (setf (board-tile board ij) tile))

;; displace the object on a to b, assume b empty
(defun displace-piece* (board a b)
  (assert (eq (tile-object (board-tile board b)) :empty))
  (psetf (board-tile board b) (board-tile board a)
         (board-tile board a) (board-tile board b)))

;; empty the tile and return whatever was there
(defun remove-piece* (board ij)
  (prog1
      (board-tile board ij)
    (setf (board-tile board ij) (make-tile :empty))))

;; turn a man into a king when he is Ready
(defun maybe-crown* (tile ij)
  (when (crown-p tile ij)
    (setf (tile-object tile) :king)
    t))

;; turn a king back into a man (used for unapply-move)
(defun uncrown* (tile)
  (with-slots (object) tile
    (assert (eq object :king))
    (setf object :man)))

;; toggle whoever is to move
(defun toggle-player* (state)
  (setf (state-player state) (opponent (state-player state))))


;; like place-piece* with hash update
(defun place-piece (state ij tile)
  (place-piece* (state-board state) ij tile)
  (logxorf (state-hash state)
           (tile-zobrist-bitstring tile ij)))

;; like displace-piece* with hash update
(defun displace-piece (state a b)
  (let ((board (state-board state)))
    (logxorf (state-hash state)
             (tile-zobrist-bitstring (board-tile board a) a)
             (tile-zobrist-bitstring (board-tile board a) b))
    (displace-piece* board a b)))

;; remove-piece* with hash update
(defun remove-piece (state ij)
  (let ((tile (remove-piece* (state-board state) ij)))
    (logxorf (state-hash state)
             (tile-zobrist-bitstring tile ij))
    tile))

;; maybe-crown* with hash update
(defun maybe-crown (state move)
  (with-slots (board hash) state
    (let* ((ij (lastcar move))
           (tile (board-tile board ij)))
      (logxorf hash (tile-zobrist-bitstring tile ij))
      (prog1 (maybe-crown* tile ij)
        (logxorf hash (tile-zobrist-bitstring tile ij))))))

;; uncrown* with hash update
(defun uncrown (state move)
  (with-slots (board hash) state
    (let* ((ij (lastcar move))
           (tile (board-tile board ij)))
      (logxorf hash (tile-zobrist-bitstring tile ij))
      (uncrown* (board-tile (state-board state) ij))
      (logxorf hash (tile-zobrist-bitstring tile ij)))))

;; toggle-player* with hash update
(defun toggle-player (state)
  (toggle-player* state)
  (logxorf (state-hash state) (zobrist-player-bitstring)))


;;; MOVE GENERATION

(defun displacement-direction (ij ij2)
  (let* ((didj (v-v ij2 ij))
         (n (max (abs (s1 didj))
                 (abs (s2 didj)))))
    (values (s*v (/ 1 n) didj) n)))

(defparameter *all-directions*
  (mapcar #'list->v
          '((-1  0) (0 -1)
            ( 0  1) (1  0)
            (-1 -1) (1  1))))

(defun moveset-equal (a b)
  (set-equal a b :test #'move-equal))
(defun move-equal (a b)
  (not (mismatch a b :test #'v=)))

(defun piece-moves (board ij)
  (declare (optimize (speed 3) (safety 1))
           (type board board))
  (let ((tile (board-tile board ij)))
    (ccase (tile-object tile)
      (:man
       (iter (for didj in (player-forward-directions (tile-owner tile)))
             (for ij2 = (v+v ij didj))
             (when (eq (tile-object (board-tile board ij2)) :empty)
               (collect (list ij ij2)))))
      (:king
       (iter (for didj in *all-directions*)
             (nconcing (iter (for d from 1)
                             (for ij2 = (v+v ij (s*v d didj)))
                             (declare (fixnum d))
                             (while (eq (tile-object (board-tile board ij2)) :empty))
                             (collect (list ij ij2)))))))))

(defun piece-captures (board ij)
  (declare (optimize (speed 3) (safety 1))
           (type board board))
  (let* ((tile (board-tile board ij))
         (player (tile-owner tile))
         (can-fly (can-fly (tile-object tile))))
    (labels ((recur (ij capture-points)
               (iter (for didj in *all-directions*)
                     (with captures = '())
                     (iter inner-loop
                           (for d from 1)
                           (for ij2 = (v+v ij (s*v d didj)))
                           (with capture-point = nil)
                           (declare (fixnum d))
                           (with-slots (object owner) (board-tile board ij2)
                             (cond ((and (null capture-point)
                                         (eq object :empty)
                                         can-fly)
                                    ;; haven't yet passed over an enemy piece. no capture, but keep going.
                                    )
                                   ((and (null capture-point)
                                         (member object '(:man :king))
                                         (eq owner (opponent player))
                                         (not (member ij2 capture-points :test #'v=)))
                                    ;; passing over a fresh enemy piece.
                                    (setf capture-point ij2))
                                   ((and capture-point
                                         (eq object :empty))
                                    ;; have passed over an enemy piece. possible stopping point or turning point.
                                    (push (list ij ij2) captures)
                                    (iter (for continuation in (recur ij2 (cons capture-point capture-points)))
                                          (push (cons ij continuation) captures))
                                    ;; if we can't fly over empties, then we must stop at the first empty after
                                    ;; capture.
                                    (unless can-fly
                                      (return-from inner-loop)))
                                   (t
                                    ;; this is going nowhere
                                    (return-from inner-loop)))))
                     (finally (return captures)))))
      (recur ij '()))))

;;; a move is a list (x y z ...) of points visited, and everything between the points is captured
;;; NOTE: potentially modifies state; sets endp if no moves. this is unconditionally set to nil by
;;; unapply-move.
(defun moves (state)
  (declare (optimize (debug 3) (safety 1)))
  (with-slots (board player endp) state
    (declare (type board board))
    (iter (for tile at ij of board)
          (with-slots (owner) tile
            (when (eq owner player)
              (nconcing (piece-captures board ij) into captures)
              (unless captures
                (nconcing (piece-moves board ij) into moves))))
          (finally
           (return (if captures
                       ;; longest capture is mandatory
                       (cl-utilities:extrema captures #'> :key #'length)
                       (progn
                         ;; if no moves, mark the game as over
                         (when (null moves)
                           (setf endp t))
                         moves)))))))


;;; MOVE APPLICATION

;; returned by apply-move, accepted by unapply-move. contains undo information.
(defstruct breadcrumb
  (move nil :type list)
  (capture-points '() :type list)
  (capture-tiles '() :type list)
  (crowned nil :type boolean))

(defun apply-move (state move)
  (with-slots (board player endp) state
    (assert (and move (not (state-endp state))))
    (let* ((initial-tile (board-tile board (car move)))
           (moving-object (tile-object initial-tile))
           (can-fly (can-fly moving-object))
           (capture-points '()))
      (assert (eq player (tile-owner initial-tile)))
      (assert (member moving-object '(:man :king)))
      ;; trace the path to figure out what's captured. might as well
      ;; do some validity checking. the checks here are safety nets
      ;; rather than guarantees. men's backward movement is not prevented,
      ;; mandatory captures are not enforced.
      (setq capture-points
            (iter (for next-ij in (rest move))
                  (for prev-ij previous next-ij initially (first move))
                  (nconcing
                   (multiple-value-bind (didj n) (displacement-direction prev-ij next-ij)
                     (assert (member didj *all-directions* :test #'v=))
                     ;; the only condition on the last tile is that it is empty
                     (assert (eq :empty (tile-object (board-tile board next-ij))))
                     ;; conditions on the rest of the tiles
                     (iter (for k from 1 below n)
                           (for ij = (v+v prev-ij (s*v k didj)))
                           (with capture-point = nil)
                           (with-slots (object owner) (board-tile board ij)
                             (ccase object
                               (:empty
                                (assert can-fly))
                               ((:man :king)
                                (assert (not (or capture-point (eq owner player))))
                                (setq capture-point ij)))
                             (finally (return
                                        (if capture-point
                                            (list capture-point)
                                            '())))))))))
      ;; now modify state
      (displace-piece state (car move) (lastcar move))
      (toggle-player state)
      (let ((capture-tiles (mapcar (lambda (ij)
                                     (remove-piece state ij))
                                   capture-points))
            (crowned (maybe-crown state move)))
        ;; return undo info
        (make-breadcrumb :move move
                         :capture-points capture-points
                         :capture-tiles capture-tiles
                         :crowned crowned)))))
  
(defun unapply-move (state breadcrumb)
  (with-slots (move capture-points capture-tiles crowned) breadcrumb
    (iter (for ij in capture-points)
          (for tile in capture-tiles)
          (place-piece state ij tile))
    (when crowned
      (uncrown state move))
    (displace-piece state (lastcar move) (car move))
    (toggle-player state))
  (setf (state-endp state) nil))


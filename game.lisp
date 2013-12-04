(in-package :heks)

(declaim (optimize (speed 3) (safety 1)))


;;; STATE MANIPULATION

;; place the object on ij, assume b empty
(defun place-piece* (board ij tile)
  (assert (eq (tile-object (board-tile board ij)) :empty))
  (setf (board-tile board ij) tile))

;; displace the object on a to b, assume b empty
(defun displace-piece* (board a b)
  (declare (v a b))
  (assert (eq (tile-object (board-tile board b)) :empty))
  (psetf (board-tile board b) (board-tile board a)
         (board-tile board a) (board-tile board b)))

;; empty the tile and return whatever was there
(defun remove-piece* (board ij)
  (declare (v ij))
  (prog1
      (board-tile board ij)
    (setf (board-tile board ij) (make-tile :object :empty))))

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
    (declare (zobrist-hash hash))
    (let* ((ij (lastcar move))
           (tile (board-tile board ij)))
      (logxorf hash (tile-zobrist-bitstring tile ij))
      (prog1 (maybe-crown* tile ij)
        (logxorf hash (tile-zobrist-bitstring tile ij))))))

;; uncrown* with hash update
(defun uncrown (state move)
  (with-slots (board hash) state
    (declare (zobrist-hash hash))
    (let* ((ij (lastcar move))
           (tile (board-tile board ij)))
      (declare (v ij))
      (logxorf hash (tile-zobrist-bitstring tile ij))
      (uncrown* (board-tile (state-board state) ij))
      (logxorf hash (tile-zobrist-bitstring tile ij)))))

;; toggle-player* with hash update
(defun toggle-player (state)
  (toggle-player* state)
  (logxorf (the zobrist-hash (state-hash state))
           (the zobrist-hash (zobrist-player-bitstring))))


;;; MOVE GENERATION

(defun displacement-direction (ij ij2)
  (declare (optimize (speed 3) (safety 0))
           (v ij ij2))
  (let* ((didj (v-v ij2 ij)))
    (declare (v didj))
    (cond ((= (s1 didj) 0) (values (v 0 (signum (s2 didj))) (the fixnum (abs (s2 didj)))))
          ((= (s2 didj) 0) (values (v (signum (s1 didj)) 0) (the fixnum (abs (s1 didj)))))
          ((= (s1 didj) (s2 didj)) (values (v (signum (s1 didj)) (signum (s2 didj))) (the fixnum (abs (s1 didj)))))
          (t (assert nil)))))

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
  (declare (optimize (speed 3) (safety 0))
           (board board)
           (v ij))
  (with-slots (object owner) (board-tile board ij)
    (let ((directions (ccase object
                        (:man (player-forward-directions owner))
                        (:king *all-directions*)))
          (moves '()))
      (dolist (didj directions)
        (declare (v didj))
        (if (not (can-fly object))
            (let ((ij2 (v+v ij didj)))
              (declare (v ij2))
              (when (tile-empty-p board ij2)
                (push (list ij ij2) moves)))
            (do ((ij2 (v+v ij didj) (v+v ij2 didj)))
                ((not (tile-empty-p board ij2)))
              (declare (v ij2))
              (push (list ij ij2) moves))))
      moves)))

(defun piece-captures (board ij)
  (declare (optimize (speed 3) (safety 0))
           (type board board)
           (v ij))
  (let* ((tile (board-tile board ij))
         (player (tile-owner tile))
         (can-fly (can-fly (tile-object tile))))
    (labels ((recur (ij capture-points &aux captures)
               (declare (v ij))
               (dolist (didj *all-directions*)
                 (declare (v didj))
                 (block inner-loop
                   (do ((ij2 (v+v ij didj) (v+v ij2 didj))
                        (capture-point nil))
                       (nil)
                     (declare (v ij2))
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
                              (if-let ((continuations (recur ij2 (cons capture-point capture-points))))
                                (dolist (continuation continuations)
                                  (push (cons ij continuation) captures))
                                (push (list ij ij2) captures))
                              ;; if we can't fly over empties, then we must stop at the first empty after
                              ;; capture.
                              (unless can-fly
                                (return-from inner-loop)))
                             (t
                              ;; this is going nowhere
                              (return-from inner-loop)))))))
               captures))
      (recur ij '()))))

;;; a move is a list (x y z ...) of points visited, and everything between the points is captured
;;; NOTE: potentially modifies state; sets endp if no moves. this is unconditionally set to nil by
;;; unapply-move.
(defun moves (state)
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (board player endp) state
    (declare (type board board))
    (unless endp
      (let (captures-lists moves-lists
            (m (- *board-size* 1))
            (n (- *board-size* 1)))
        (declare (fixnum m n))
        (do ((i 1 (1+ i)))
            ((= i m))
          (declare (fixnum i))
          (do* ((j 1 (1+ j))
                (ij (v i j) (v i j)))
               ((= j n))
            (declare (fixnum j)
                     (v ij))
            (when (eq (tile-owner (board-tile board ij)) player)
              (let (piece-captures piece-moves)
                (setf piece-captures (piece-captures board ij))
                (if piece-captures
                    (push piece-captures captures-lists)
                    (unless captures-lists
                      (setq piece-moves (piece-moves board ij))
                      (when piece-moves
                        (push piece-moves moves-lists))))))))
        (cond (captures-lists
               ;; player must choose one of the longest captures
               (let ((highest-length 1)
                     (longest-captures '()))
                 (dolist (captures captures-lists)
                   (dolist (capture captures)
                     (let ((length (length (the list capture))))
                       (cond ((> length highest-length)
                              (setf highest-length length
                                    longest-captures (list capture)))
                             ((= length highest-length)
                              (push capture longest-captures))))))
                 longest-captures))
              (moves-lists
               (reduce #'nconc moves-lists))
              (t
               (setf endp t)
               '()))))))

(defun submovep (sub super)
  (iter (for a on sub)
        (for b on super)
        (for sub-has-more = (rest a))
        ;; iter immediately returns nil if this form fails
        (always (v= (first a) (first b)))
        ;; if it doesn't, iter stops when it reaches the end of the shortest
        ;; list; see which one it is. for submovep it is required that super
        ;; is the longer list.
        (finally (return (not sub-has-more)))))

;; those candidates that sub is a submove to
;; use case: user clicks around and builds up a partial move, this will
;; return the valid moves that the user can click together from there
(defun supermoves (sub candidates)
  (iter (for candidate in candidates)
        (when (submovep sub candidate)
          (collect candidate))))


;;; MOVE APPLICATION

;; returned by apply-move, accepted by unapply-move. contains undo information.
(defstruct breadcrumb
  (move nil :type list)
  (capture-points '() :type list)
  (capture-tiles '() :type list)
  (crowned nil :type boolean))

(defun apply-move (state move)
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (board player endp) state
    (assert (and move (not (state-endp state))) (move state))
    (let* ((initial-tile (board-tile board (the v (car move))))
           (moving-object (tile-object initial-tile))
           (can-fly (can-fly moving-object))
           (capture-points '()))
      (assert (eq player (tile-owner initial-tile)) (move state))
      (assert (member moving-object '(:man :king)) (move state))
      ;; trace the path to figure out what's captured. might as well
      ;; do some validity checking. the checks here are safety nets
      ;; rather than guarantees. men's backward movement is not prevented,
      ;; mandatory captures are not enforced.
      (do ((prev-ij-cons move       (cdr prev-ij-cons))
           (next-ij-cons (cdr move) (cdr next-ij-cons)))
          ((null next-ij-cons))
        (multiple-value-bind (didj n)
            (displacement-direction (car prev-ij-cons)
                                    (car next-ij-cons))
          (assert (member didj *all-directions* :test #'v=) (move state))
          ;; the only condition on the last tile is that it is empty
          (assert (eq :empty (tile-object (board-tile board (car next-ij-cons)))) (move state))
          ;; conditions on the rest of the tiles
          (let (capture-point)
            (do ((k 1 (1+ k))
                 (ij (v+v (the v (car prev-ij-cons)) didj) (v+v ij didj)))
                ((= k n))
              (declare (fixnum k n)
                       (v ij))
              (with-slots (object owner) (board-tile board ij)
                (ccase object
                  (:empty
                   (assert can-fly (move state)))
                  ((:man :king)
                   (assert (not (or capture-point (eq owner player))) (move state))
                   (setq capture-point ij)))))
            (when capture-point
              (push capture-point capture-points)))))
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
  (declare (optimize (speed 3) (safety 1)))
  (with-slots (move capture-points capture-tiles crowned) breadcrumb
    (iter (for ij in capture-points)
          (for tile in capture-tiles)
          (place-piece state ij tile))
    (when crowned
      (uncrown state move))
    (displace-piece state (lastcar move) (car move))
    (toggle-player state))
  (setf (state-endp state) nil))


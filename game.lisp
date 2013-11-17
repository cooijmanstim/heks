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
                             (while (eq (tile-object (board-tile board ij2)) :empty))
                             (collect (list ij ij2)))))))))

(defun piece-captures (board ij)
  (let* ((tile (board-tile board ij))
         (player (tile-owner tile))
         (can-fly (can-fly (tile-object tile))))
    (labels ((recur (ij capture-points)
               (iter (for didj in *all-directions*)
                     (with captures = '())
                     (iter inner-loop
                           (for d from 1)
                           (for ij2 = (v+v ij (s*v d didj)))
                           (for tile = (board-tile board ij2))
                           (for object = (tile-object tile))
                           (for owner = (tile-owner tile))
                           (with capture-point = nil)
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
                                  (return-from inner-loop))))
                     (finally (return captures)))))
      (recur ij '()))))

;;; NOTE: a move is a list (x y z ...) of points visited,
;;; and everything between the points is captured
;;; NOTE: capturing is mandatory, so if captures have been found, we skip
;;; finding moves
(defun moves (state)
  (let ((board    (state-board    state))
        (player   (state-player   state)))
    (iter (for i from 1 below (- *board-size* 1))
          (for subresult = (iter (for j from 1 below (- *board-size* 1))
                                 (for ij = (cons i j))
                                 (for tile = (board-tile board ij))
                                 (for owner = (tile-owner tile))
                                 (when (eq owner player)
                                   (nconcing (piece-captures board ij) into captures)
                                   (unless captures
                                     (nconcing (piece-moves board ij) into moves)))
                                 (finally (return (cons captures moves)))))
          (nconcing (car subresult) into captures)
          (unless captures
            (nconcing (cdr subresult) into moves))
          (finally
           (return (if (null captures)
                       moves
                       ;; longest capture is mandatory
                       (cl-utilities:extrema captures #'> :key #'length)))))))


;;; MOVE APPLICATION

;; returned by apply-move, accepted by unapply-move. contains undo information.
(defstruct breadcrumb
  (move nil :type list)
  (capture-points '() :type list)
  (capture-tiles '() :type list))

(defun apply-move (state move)
  (assert (not (null move)))
  (let* ((board (state-board state))
         (player (state-player state))
         (initial-tile (board-tile board (car move)))
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
                            (for tile = (board-tile board ij))
                            (with capture-point = nil)
                            (ccase (tile-object tile)
                              (:empty
                               (assert can-fly))
                              ((:man :king)
                               (assert (not (or capture-point (eq (tile-owner tile) player))))
                               (setq capture-point ij)))
                            (finally (return
                                       (if capture-point
                                           (list capture-point)
                                           '()))))))))
    ;; now modify state
    (displace-piece state (car move) (lastcar move))
    (toggle-player state)
    (let ((capture-tiles (mapcar (lambda (ij)
                                   (remove-piece state ij))
                                 capture-points)))
      ;; return undo info
      (make-breadcrumb :move move
                       :capture-points capture-points
                       :capture-tiles capture-tiles))))

(defun unapply-move (state breadcrumb)
  (let ((move (breadcrumb-move breadcrumb)))
    (iter (for ij in (breadcrumb-capture-points breadcrumb))
          (for tile in (breadcrumb-capture-tiles breadcrumb))
          (place-piece state ij tile))
    (displace-piece state (lastcar move) (car move))
    (toggle-player state)))



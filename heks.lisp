(in-package :heks)

(declaim (optimize (debug 3)))

(deftype player ()
  '(member :white :black))

; XXX: it is assumed that owner is non-nil iff object is :man or :king
(defstruct (tile (:constructor make-tile (object &optional owner)))
  (object :void :type (member :void     ; the tile is not part of the board (necessary
                                        ; because we're cramming a hexagonal board into
                                        ; a rectangular array
                              :empty    ; there is nothing on the tile
                              :man :king))
  (owner nil :type (or nil player)))

(defstruct state
  (board (make-initial-board) :type (array tile))
  (player :white :type player))

;; used for move undo
(defstruct breadcrumb
  (move nil :type list)
  (capture-points '() :type list)
  (capture-tiles '() :type list))

; since our choice of representation already forces us to deal with :void tiles,
; we might as well use a border of :void tiles to avoid the need for explicit
; bounds checking.  hence 11 instead of 9 here.
(defparameter *board-size* 11)
(defparameter *board-dimensions* (v *board-size* *board-size*))
(defparameter *board-center* (s+v -1/2 (s*v 1/2 *board-dimensions*)))

(defun board-tile (board ij)
  (aref board (s1 ij) (s2 ij)))
(defun set-board-tile (board ij value)
  (setf (aref board (s1 ij) (s2 ij)) value))
(defsetf board-tile set-board-tile)

(defun initial-tile (ij)
  (let* ((board-interior-dimensions (s+v -2 *board-dimensions*))
         (ij (s+v -1 ij))
         ;; cumulative inverse coordinates
         (kl (s+v -1 (v-v board-interior-dimensions ij))))
    (cond ((or (<= (+ (s2 kl) (s1 ij)) 3)
               (<= (+ (s1 kl) (s2 ij)) 3)) (make-tile :void))
          ((absv<=s ij 3) (make-tile :man :white))
          ((absv<=s kl 3) (make-tile :man :black))
          (t (make-tile :empty)))))

(defun make-initial-board ()
  (let ((board (make-array (v->list *board-dimensions*)
                           :element-type 'tile
                           :initial-element (make-tile :void)
                           :adjustable nil)))
    (iter (for i from 1 below (- *board-size* 1))
          (iter (for j from 1 below (- *board-size* 1))
                (setf (aref board i j) (initial-tile (v i j)))))
    board))

(defun make-empty-board ()
  (let ((board (make-initial-board)))
    (iter (for i from 1 below (- *board-size* 1))
          (iter (for j from 1 below (- *board-size* 1))
                (unless (eq (tile-object (aref board i j)) :void)
                  (setf (aref board i j) (make-tile :empty)))))
    board))

(defun make-initial-state ()
  (make-state))

(defun displacement-direction (ij ij2)
  (let* ((didj (v-v ij2 ij))
         (n (max (abs (s1 didj))
                 (abs (s2 didj)))))
    (values (s*v (/ 1 n) didj) n)))

(defun displace-piece (board a b)
  (assert (eq (tile-object (board-tile board b)) :empty))
  (psetf (board-tile board b) (board-tile board a)
         (board-tile board a) (board-tile board b)))

;; empty the tile and return whatever was there
(defun empty-tile (board ij)
  (prog1
      (board-tile board ij)
    (setf (board-tile board ij) (make-tile :empty))))

(defparameter *all-directions*
  (mapcar #'list->v
          '((-1  0) (0 -1)
            ( 0  1) (1  0)
            (-1 -1) (1  1))))

;; using {0,1} or {-1,1} for players instead of :white :black would make this unnecessary
(defun player-forward-directions (player)
  (ccase player
    (:white (mapcar #'list->v '((0  1) ( 1 0) ( 1  1))))
    (:black (mapcar #'list->v '((0 -1) (-1 0) (-1 -1))))))
(defun opponent (player)
  (ccase player
    (:white :black)
    (:black :white)))

(defun can-fly (object)
  (ccase object
    (:man nil)
    (:king t)))

(defun moveset-equal (a b)
  (set-equal a b :test #'move-equal))
(defun move-equal (a b)
  (every #'v= a b))

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

(defun toggle-player (state)
  (setf (state-player state) (opponent (state-player state))))

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
    (displace-piece board (car move) (lastcar move))
    (toggle-player state)
    (let ((capture-tiles (mapcar (lambda (ij)
                                   (empty-tile board ij))
                                 capture-points)))
      ;; return undo info
      (make-breadcrumb :move move
                       :capture-points capture-points
                       :capture-tiles capture-tiles))))

(defun unapply-move (state breadcrumb)
  (let ((board (state-board state))
        (move (breadcrumb-move breadcrumb)))
    (iter (for ij in (breadcrumb-capture-points breadcrumb))
          (for tile in (breadcrumb-capture-tiles breadcrumb))
          (setf (board-tile board ij) tile))
    (displace-piece board (lastcar move) (car move))
    (toggle-player state)))

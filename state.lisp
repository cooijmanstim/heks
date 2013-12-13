(in-package :heks)

(declaim (optimize (debug 3) (safety 3)))

(defstruct (state (:copier nil))
  (board nil :type board)
  (player *white-player* :type player)
  (endp nil :type boolean)
  (hash 0 :type zobrist-hash))

(defun make-initial-state ()
  (let ((board (make-initial-board)))
    (make-state :board board
                :player *white-player*
                :hash (logxor (zobrist-hash-board board)
                              (zobrist-hash-player *white-player*))
                :endp nil)))

(defun copy-state (state)
  (with-slots (board player hash endp) state
    (make-state :board (copy-board board)
                :player player
                :hash hash
                :endp endp)))

(defun state-equal (a b)
  (and (= (state-player a) (state-player b))
       (= (state-hash a) (state-hash b))
       (board-equal (state-board a) (state-board b))))

(defun state-winner (state)
  (with-slots (endp player) state
    (assert endp)
    (opponent player)))


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
  (with-slots (player) state
    (setf player (opponent player))))


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

(define-condition invalid-displacement-error (error)
  ((displacement :initarg :displacement)))

(defun displacement-direction (ij ij2)
  (declare (optimize (speed 3) (safety 0))
           (v ij ij2))
  (let* ((didj (v-v ij2 ij)))
    (declare (v didj))
    (cond ((= (s1 didj) 0) (values (v 0 (signum (s2 didj))) (the fixnum (abs (s2 didj)))))
          ((= (s2 didj) 0) (values (v (signum (s1 didj)) 0) (the fixnum (abs (s1 didj)))))
          ((= (s1 didj) (s2 didj)) (values (v (signum (s1 didj)) (signum (s2 didj))) (the fixnum (abs (s1 didj)))))
          (t (error 'invalid-displacement-error :displacement didj)))))

(defun moveset-equal (a b)
  (set-equal a b :test #'move-equal))
(defun move-equal (a b)
  (not (mismatch a b :test #'v=)))

(defun piece-moves (board ij object owner)
  (declare (optimize (speed 3) (safety 0))
           (board board)
           (v ij))
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
    moves))

(defun piece-captures (board ij object owner)
  (declare (optimize (speed 3) (safety 0))
           (type board board)
           (v ij))
  (if (can-fly object)
      (piece-captures-flying board ij owner)
      (piece-captures-nonflying board ij owner)))

(defun piece-captures-nonflying (board ij player)
  (declare (optimize (speed 3) (safety 0))
           (type board board)
           (v ij))
  (let ((opponent (opponent player)))
    (inlinerecur 2 (recur ij '()) (ij capture-ijs &aux captures)
      (declare (optimize (speed 3) (safety 0))
               (v ij))
      (dolist (didj *all-directions*)
        (declare (v didj))
        ;; ij of the tile that is jumped over
        (let ((capture-ij (v+v ij didj)))
          ;; don't jump over the same piece twice
          (unless (member capture-ij capture-ijs :test #'v=)
            (with-slots (object owner) (board-tile board capture-ij)
              (when (eq owner opponent)
                (assert (member object '(:man :king)))
                ;; ij of the tile that we land on
                (let ((land-ij (v+v capture-ij didj)))
                  (when (tile-empty-p board land-ij)
                    ;; maybe continue jumping elsewhere
                    (if-let ((continuations (funcall recur land-ij (cons capture-ij capture-ijs))))
                      (dolist (continuation continuations)
                        (push (cons ij continuation) captures))
                      (push (list ij land-ij) captures)))))))))
      captures)))

(defun piece-captures-flying (board ij player)
  (declare (optimize (speed 3) (safety 0))
           (type board board)
           (v ij))
  (let* ((opponent (opponent player)))
    (inlinerecur 2 (recur ij '()) (ij capture-points &aux captures)
        (declare (optimize (speed 3) (safety 0))
                 (v ij))
      (dolist (didj *all-directions*)
        (declare (v didj))
        (block inner-loop
          (do ((ij2 (v+v ij didj) (v+v ij2 didj))
               (capture-point nil))
              (nil)
            (declare (v ij2))
            (with-slots (object owner) (board-tile board ij2)
              (cond ((and (null capture-point)
                          (eq object :empty))
                     ;; haven't yet passed over an enemy piece. no capture, but keep going.
                     )
                    ((and (null capture-point)
                          (member object '(:man :king))
                          (eq owner opponent)
                          (not (member ij2 capture-points :test #'v=)))
                     ;; passing over a fresh enemy piece.
                     (setf capture-point ij2))
                    ((and capture-point
                          (eq object :empty))
                     ;; have passed over an enemy piece. possible stopping point or turning point.
                     (if-let ((continuations (funcall recur ij2 (cons capture-point capture-points))))
                       (dolist (continuation continuations)
                         (push (cons ij continuation) captures))
                       (push (list ij ij2) captures)))
                    (t
                     ;; this is going nowhere
                     (return-from inner-loop)))))))
      captures)))

;;; a move is a list (x y z ...) of points visited, and everything between the points is captured
;;; NOTE: potentially modifies state; sets endp if no moves. this is unconditionally set to nil by
;;; unapply-move.
(defun moves (state)
  (declare (optimize (speed 3) (safety 0)))
  (when (state-endp state)
    (return-from moves '()))
  (let ((board (state-board state))
        (player (state-player state))
        captures-lists
        moves-lists
        (m (- *board-size* 1))
        (n (- *board-size* 1)))
    (declare (board board)
             (fixnum m n))
    (do ((i 1 (1+ i)))
        ((= i m))
      (declare (fixnum i))
      (do* ((j 1 (1+ j)))
           ((= j n))
        (declare (fixnum j))
        (let* ((ij (v i j))
               (tile (board-tile board ij))
               (owner (tile-owner tile)))
          (declare (v ij))
          (when (eq owner player)
            (let ((object (tile-object tile)))
              (if-let ((piece-captures (piece-captures board ij object owner)))
                (push piece-captures captures-lists)
                (unless captures-lists
                  (when-let ((piece-moves (piece-moves board ij object owner)))
                    (push piece-moves moves-lists)))))))))
    (cond (captures-lists
           (apply #'longest-captures captures-lists))
          (moves-lists
           (apply #'nconc moves-lists))
          (t
           (setf (state-endp state) t)
           '()))))

(defun longest-captures (&rest captures-lists)
  (declare (optimize (speed 3) (safety 0)))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *illegal-is-error-p* t))
(defmacro illegal-if (condition-expr &rest other-assert-arguments)
  (if *illegal-is-error-p*
      `(assert (not ,condition-expr) ,@other-assert-arguments)
      `(when ,condition-expr (return nil))))

(defun apply-move (state move)
  (declare (optimize (speed 3) (safety 0)))
  (with-slots (board player endp) state
    (illegal-if (or (null move)
                    (state-endp state))
                (move state))
    (let* ((initial-tile (board-tile board (the v (car move))))
           (moving-object (tile-object initial-tile))
           (can-fly (can-fly moving-object))
           (capture-points '()))
      (illegal-if (not (eq player (tile-owner initial-tile)))
                  (move state))
      (illegal-if (not (member moving-object '(:man :king)))
                  (move state))
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
          (illegal-if (not (member didj *all-directions* :test #'v=))
                      (move state))
          ;; the only condition on the last tile is that it is empty
          (illegal-if (not (tile-empty-p board (car next-ij-cons)))
                      (move state))
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
                   (illegal-if (not can-fly) (move state)))
                  ((:man :king)
                   ;; can't jump over multiple or own pieces
                   (illegal-if (or capture-point (eq owner player)) (move state))
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


(defun state-dump (state filespec)
  (with-open-file (stream filespec :direction :output :if-exists :supersede :if-does-not-exist :create)
    (prin1 state stream))
  (fresh-line)
  (format t "state dumped to ~A" filespec))

(defun state-load (filespec)
  (with-open-file (stream filespec :direction :input)
    (let ((state (read stream)))
      (setf (state-hash state) (zobrist-hash state))
      state)))

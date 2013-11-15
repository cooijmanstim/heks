(in-package :heks)

(declaim (optimize (debug 3)))

(defun minimax-decision (state)
  (iter (for depth from 1 to 5)
        (with value) (with best-move)
        (multiple-value-setq (value best-move) (minimax state depth))
        (print (list value best-move))
        (finally (return best-move))))

;; evaluation is from the perspective of player-to-move

;; an end-state is a state where no more moves can be made. the
;; player to move loses.
(defun evaluate-end-state (state)
  -1)

;; just count advantage in number of pieces
(defun evaluate-state (state)
  (iter (for i from 1 below (- *board-size* 1))
        (summing
         (iter (for j from 1 below (- *board-size* 1))
               (summing (case (tile-owner (board-tile (state-board state) (v i j)))
                          ((state-player state) 1)
                          ((opponent (state-player state)) -1)
                          (otherwise 0)))))))

(defun minimax (state depth)
  (if (= depth 0)
      (evaluate-state state)
      (let ((moves (moves state)))
        (if (null moves)
            (evaluate-end-state state)
            (iter (for move in moves)
                  (for breadcrumb = (apply-move state move))
                  (finding move maximizing (minimax state (1- depth)) into (best-move value))
                  (unapply-move state breadcrumb)
                  (finally (return (values (- value) best-move))))))))

; Justin Hong
; 604565186
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
; goal-test checks each list in the list of lists of the state for boxes. If a row has no boxes 
; it checks the remaining rows. If all the rows have no boxes, we have reached the goal state

(defun goal-test (s)
	(cond ((NULL s) t) ((equal (count 2 (car s)) 0) (goal-test (cdr s))) (t NIL))
  );end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
; I followed the pseudocode for the implementation of next-states and represented the direction as integers with
; up as 0, down as 1, left as 2, right as 3.

(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 0) (try-move s 1) (try-move s 2) (try-move s 3)))
	 )
    (cleanUpList result);end
   );end let
  );

; try-move is a helper function for next-states. It takes a state s and a move direction d. It returns the state
; that is the result of moving the keeper in state s in direction d. If the move is invalid it returns NIL.
;
; The move direction d is represented as an integer with up as 0, down as 1, left as 2, right as 3
;
; I start by setting variables r and c to location of the keeper
; Then I call the isIllegal helper function to check if the move is illegal
; If the move is not illegal, I check if the keeper is on a goal or by himself
; If the keeper is on a goal, I set the square to goal on the state after the keeper has moved as returned by
; the helper function move-keeper
; If the keeper is by himself, I set the square to blank

(defun try-move (s d)
	(let* ((r (second (getKeeperPosition s 0)))
		(c (first (getKeeperPosition s 0)))
		)
		(cond ((isIllegal s r c d) NIL) 
			(t (cond ((isKeeperStar (get-square s r c)) (set-square (move-keeper s r c d) r c star))
				(t (set-square (move-keeper s r c d) r c blank))))))
	)


; isIllegal is a helper function for try-move. If the move on state s with the keeper at location (r, c)
; in direction d is illegal it returns true otherwise it returns NIL
;
; For each direction, I start by checking if the keeper will move into a wall in which case the move is illegal
; Then I check if the square the keeper wants to move into is occupied by a box or a box and goal
; If it is then I check if the square where the box will be pushed by the keeper is occupied by a box,
; box and goal, or wall. If so then the move is illegal. Otherwise the move is legal

(defun isIllegal (s r c d)
	(cond ((equal d 0) 
		(cond ((isWall (get-square s (- r 1) c)) t)
			((and 
				(or
				(isBox (get-square s (- r 1) c)) 
				(isBoxStar (get-square s (- r 1) c)))
				(or 
					(isBox (get-square s (- r 2) c)) 
				(isBoxStar (get-square s (- r 2) c)) 
				(isWall (get-square s (- r 2) c)))) t)
			(t NIL)))
		((equal d 1)
			(cond ((isWall (get-square s (+ r 1) c)) t)
			((and 
				(or
				(isBox (get-square s (+ r 1) c)) 
				(isBoxStar (get-square s (+ r 1) c)))
				(or 
					(isBox (get-square s (+ r 2) c)) 
				(isBoxStar (get-square s (+ r 2) c)) 
				(isWall (get-square s (+ r 2) c)))) t)
			(t NIL)))
		((equal d 2)
			(cond ((isWall (get-square s r (- c 1))) t)
			((and 
				(or
				(isBox (get-square s r (- c 1))) 
				(isBoxStar (get-square s r (- c 1))))
				(or 
					(isBox (get-square s r (- c 2))) 
				(isBoxStar (get-square s r (- c 2))) 
				(isWall (get-square s r (- c 2))))) t)
			(t NIL)))
		((equal d 3)
			(cond ((isWall (get-square s r (+ c 1))) t)
			((and 
				(or
				(isBox (get-square s r (+ c 1))) 
				(isBoxStar (get-square s r (+ c 1))))
				(or 
					(isBox (get-square s r (+ c 2))) 
				(isBoxStar (get-square s r (+ c 2))) 
				(isWall (get-square s r (+ c 2))))) t)
			(t NIL)))
		(t t))
	)

; move-keeper is a helper function for try-move. It takes a state s, the location of the keeper at row r, column c, and a direction d
; and tries to move the keeper in the specified direction and returns the new state
;
; move-keeper starts by checking if the keeper is on a goal and changing the square to a goal only if that's the case.
; For each direction, I check whether the square that the keeper wants to move into is occupied by a box and a goal or just a goal
; In that case I set the square in the state after the box has been moved to a keeper and a goal
; Otherwise I set the square in the state after the box has been moved to just a keeper

(defun move-keeper (s r c d)
	(cond ((isKeeperStar (get-square s r c)) (set-square s r c star)))
		(cond ((equal d 0)
			(cond 
				((or 
					(isBoxStar (get-square s (- r 1) c))
					(isStar (get-square s (- r 1) c))) 
				(set-square (move-box s (- r 1) c d) (- r 1) c keeperstar))
			(t (set-square (move-box s (- r 1) c d) (- r 1) c keeper))))
			((equal d 1)
			(cond 
				((or 
					(isBoxStar (get-square s (+ r 1) c))
					(isStar (get-square s (+ r 1) c))) 
				(set-square (move-box s (+ r 1) c d) (+ r 1) c keeperstar))
			(t (set-square (move-box s (+ r 1) c d) (+ r 1) c keeper))))
			((equal d 2)
			(cond 
				((or 
					(isBoxStar (get-square s r (- c 1)))
					(isStar (get-square s r (- c 1)))) 
				(set-square (move-box s r (- c 1) d) r (- c 1) keeperstar))
			(t (set-square (move-box s r (- c 1) d) r (- c 1) keeper))))
			((equal d 3)
			(cond 
				((or 
					(isBoxStar (get-square s r (+ c 1)))
					(isStar (get-square s r (+ c 1)))) 
				(set-square (move-box s r (+ c 1) d) r (+ c 1) keeperstar))
			(t (set-square (move-box s r (+ c 1) d) r (+ c 1) keeper))))
			(t NIL)
		))

; move-box is a helper function for move-keeper. It takes a state s, a row number, a column number c, and a direction d
; It attempts to move the box at the location in the direction d and returns the state afterwards
;
; move-box starts by checking if the square specified is a box on a goal and sets it to just a goal if that's the case
; Then if the square specified is a box or a box on a goal, it checks which direction the box is meant to move in
; For each direction, if the square the box is supposed to move into is a goal, the square is set to a box and goal
; Otherwise the square is just set to a box
; If the location passed into move-box is not a box or a box and goal, it just returns the state again

(defun move-box (s r c d)
	(cond ((isBoxStar (get-square s r c)) (set-square s r c star))
		((isBox (get-square s r c)) (set-square s r c blank)))
	(cond ((or (isBox (get-square s r c)) (isBoxStar (get-square s r c)))
		(cond ((equal d 0)
			(cond
				((isStar (get-square s (- r 1) c)) (set-square s (- r 1) c boxstar))
				(t (set-square s (- r 1) c box))))
		((equal d 1)
			(cond
				((isStar (get-square s (+ r 1) c)) (set-square s (+ r 1) c boxstar))
				(t (set-square s (+ r 1) c box))))
		((equal d 2)
			(cond
				((isStar (get-square s r (- c 1))) (set-square s r (- c 1) boxstar))
				(t (set-square s r (- c 1) box))))
		((equal d 3)
			(cond
				((isStar (get-square s r (+ c 1))) (set-square s r (+ c 1) boxstar))
				(t (set-square s r (+ c 1) box))))
		))
		(t s))
	)

; get-square is a helper function for try-move. It takes a state s, a row number r, and a column number c.
; It returns the integer content of state s at square (r, c). If the square is outside the scope of the problem,
; it returns the value of a wall
;
; I start by checking if the square is outside the scope of the problem in which case I return wall
; Otherwise I use nthcdr on both the row and column to extract the value of the square

(defun get-square (s r c)
	(cond ((NULL s) wall)
		((or (< r 0) (< c 0) (> r (- (length s) 1)) (> c (- (length (first s)) 1))) wall)
		(t (car (nthcdr c (car (nthcdr r s)))))))

; set-square is a helper function for try-move. It takes a state s, a row number r, a column number c, and a square content v.
; It returns the new state obtained by setting the square (r, c) to value v.

; In this function I cons rows with the rest of the rows until I reach the row we want to modify
; Then I iterate through this row until I reach the right column to modify
; After that I replace the square with the value I want to set and cons the rest of the state

(defun set-square (s r c v)
	(cond ((NULL s) NIL) 
		((> r 0) (cons (first s) (set-square (cdr s) (- r 1) c v)))
		((> c 0) 
			(let* ((col (set-square (cons (cdr (car s)) (cdr s)) r (- c 1) v))) 
				(cons (cons (car (car s)) (car col)) (cdr col))))
			(t (cons (cons v (cdr (car s))) (cdr s)))))

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
; h1 returns 0 if the state s is NIL. It checks whether the state is a box if it is an atom and returns 1 if so, otherwise it returns 0.
; If the state is not an atom, it calls h1 on (car s) and (cdr s) and adds the results recursively
;
; This heuristic is admissible because it always takes 1 or more moves to push a box into the goal for each misplaced box.
; Therefore the number of misplaced boxes never overestimates the cost of reaching the goal

(defun h1 (s)
	(cond ((NULL s) 0)
		((atom s) 
			(cond ((isBox s) 1)
				(t 0)))
		(t (+ (h1 (car s)) (h1 (cdr s))))
  ))

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
; I decided to use the h1 heuristic for my heuristic after realizing that the heuristic I designed took a lot of time to execute.
; Although it saves space and extra node expansions, the extra time for each call outweighs the benefits.
; Therefore I decided to just use the admissible h1 heuristic.

(defun h604565186 (s)
	(h1 s)
)
; My heuristic calculates the distance between the keeper and a box and adds that distance to how many boxes are not in goals.
; This is admissible because the keeper must at least take that amount of distance to reach the box and push all the boxes into the goals.
; 
; (defun h604565186 (s)
; 	(+ (h1 s) (getDistance 
; 		(first (getKeeperPosition s 0)) 
; 		(second (getKeeperPosition s 0)) 
; 		(first (getBoxPosition s 0)) 
; 		(second (getBoxPosition s 0))))
;   )

; getBoxColumn is a helper function for getBoxPosition. This function works in the same fashion as the provided getKeeperColumn
; function except that it looks for a box instead of the keeper

(defun getBoxColumn (r col)
  (cond ((null r) nil)
	(t (if (isBox (car r))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

; getBoxPosition is a helper function for my heuristic that returns a list indicating the position of a box (c r). This function works in the 
; same fashion as the provided getKeeperPosition function except that it looks for a box instead of the keeper

(defun getBoxPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getBoxColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getBoxPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

; absVal is a helper function for the getDistance function since the lisp abs function is not allowed. It takes a value and returns the
; absolute value of the value

(defun absVal (val)
	(cond ((< val 0) (- val))
		(t val)
		)
	)

; getDistance is a helper function for my heuristic. It calculates the manhattan distance between two pairs of coordinates passed in
; as two pairs of rows and columns and returns the distance

(defun getDistance (c1 r1 c2 r2)
	(+ (absVal (- c1 c2)) (absVal (- r1 r2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun

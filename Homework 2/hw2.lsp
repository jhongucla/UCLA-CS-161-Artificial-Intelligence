; Justin Hong
; 604565186


; DFS takes an argument that is a list representation of the tree and performs a left to right depth-first search
; DFS returns a top-level list of the terminal nodes
; In the base cases, if the tree is null then we return NIL, if it is an atom then we have reached a leaf node and we create a list with that node.
; Otherwise we recurse on the first element and then the rest of the elements in the tree

(defun DFS (TREE)
	(cond ((NULL TREE) NIL)
		((atom TREE) (list TREE))
		(t (append (DFS (first TREE)) (DFS (cdr TREE))))))


; DFID takes two arguments, a list representation of the tree and an integer representing the maximum depth of the tree, and performs a depth-first iterative-deepening search
; DFID returns a single top-level list of the terminal nodes
; The base case is when the maximum depth reaches -1 in which case we return NIL. Otherwise we append calls to DFID with decreasing maximum depths to the beginning of a call to DFS_LIMITED

(defun DFID (TREE maximum_depth)
	(cond ((NULL TREE) NIL)
		((< maximum_depth 0) NIL)
		(t (append (DFID TREE (- maximum_depth 1)) (DFS_LIMITED TREE maximum_depth)))))


; DFS_LIMITED is a helper function that takes two arguments, a list representation of the tree and an integer representing the depth it should search to
; It performs a depth-first search up to the depth that is specified
; DFS_LIMITED returns a single list of the nodes it has searched up to the depth limit
; Essentially DFS_LIMITED is like DFS except we check if the depth reaches -1 in which case we stop and we also decrement the depth in recursive calls to DFS_LIMITED

(defun DFS_LIMITED (TREE depth)
	(cond ((NULL TREE) NIL)
		((< depth 0) NIL)
		((atom TREE) (list TREE))
		(t (append (DFS_LIMITED (first TREE) (- depth 1)) (DFS_LIMITED (cdr TREE) depth)))))


; FINAL-STATE takes a single argument (S), the current state, and returns T if
; it is the goal state (3 3 NIL) and NIL otherwise.

(defun final-state (s)
	(equal s '(3 3 NIL)))


; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), a number of
; missionaries to move (M), and a number of cannibals to move (C). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
; I first set 4 variables to the number of cannibals and missionaries on the two sides. Then I checked all the conditions that would result in an invalid 
; state and finally listed the next state if it would be valid.

(defun next-state (s m c)
	(let ((curr_side_cannibals (- (second s) c)) 
		(curr_side_missionaries (- (first s) m)) 
		(other_side_cannibals (+ (- 3 (second s)) c)) 
		(other_side_missionaries (+ (- 3 (first s)) m)))
	(cond ((or (< curr_side_cannibals 0) (< curr_side_missionaries 0) (< other_side_cannibals 0) (< other_side_missionaries 0)) NIL)
		((and (> curr_side_missionaries 0) (> curr_side_cannibals curr_side_missionaries)) NIL) 
		((and (> other_side_missionaries 0) (> other_side_cannibals other_side_missionaries)) NIL)
		(t (list (list other_side_missionaries other_side_cannibals (not (third s))))))))


; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (S), which encodes the current state, and
; returns a list of states that can be reached by applying legal operators to
; the current state.
; I found all the legal successor states but calling next-state with every combination of missionaries and cannibals to move and listing the outputs

(defun succ-fn (s)
	(append (next-state s 0 1)
		(next-state s 1 0)
		(next-state s 1 1)
		(next-state s 2 0)
		(next-state s 0 2)))


; MULT-DFS is a helper function for SINGLE-DFS. It takes three arguments: the
; path from the initial state to the current state (PATH), the legal successor
; states to the last state on PATH (STATES), and the depth (DEPTH). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a single depth-first iteration to the given depth on
; each element of STATES in turn. If any of those searches reaches the final
; state, MULT-DFS returns the complete path from the initial state to the goal
; state. Otherwise, it returns NIL.
; The base case is when states is empty in which case I return NIL. Otherwise I call single-dfs on the first state and return the path if it is found. 
; If not, I call mult-dfs on the remaining states. 

(defun mult-dfs (states path depth)
	(cond ((NULL states) NIL)
		((single-dfs (first states) path depth) (single-dfs (first states) path depth))
	(t (mult-dfs (cdr states) path depth))))


; SINGLE-DFS does a single depth-first iteration to the given depth. It takes
; three arguments: a state (S), the path from the initial state to S (PATH), and
; the depth (DEPTH). If S is the initial state in our search, PATH should be
; NIL. It performs a depth-first search starting at the given state. It returns
; the path from the initial state to the goal state, if any, or NIL otherwise.
; I start by checking if s is the goal state, in which case I append the state to the path. If not, I then check if the depth limit has been reached. 
; If not, I call mult-dfs with all the successor states of s and a new path that includes s and a decremented depth

(defun single-dfs (s path depth)
	(cond ((final-state s) (append path (list s))) 
		((< depth 0) NIL)
		(t (mult-dfs (succ-fn s) (append path (list s)) (- depth 1)))))


; ID-DFS is the top-level function. It takes two arguments: an initial state (S)
; and a search depth (DEPTH). ID-DFS performs a series of depth-first
; iterations, starting from the given depth until a solution is found. It
; returns the path from the initial state to the goal state. The very first call
; to ID-DFS should use depth = 0.
; I start by calling single-dfs and if the result is NIL then I call id-dfs with an incremented depth. 
; If the result is not NIL, then I call single-dfs again with the same parameters and return the output this time

(defun id-dfs (s depth)
	(cond ((NULL (single-dfs s NIL depth)) (id-dfs s (+ depth 1)))
		(t (single-dfs s NIL depth))))

; Justin Hong
; 604565186

; TREE-CONTAINS takes two arguments, a number N and an ordered tree TREE, and checks whether N appears in TREE
; TREE-CONTAINS returns a boolean
; In the base case, I checked if the number matches what we are looking for when TREE is an atom. I recursed through the subtrees of each node that is not an atom

(defun TREE-CONTAINS (N TREE) 
	(if (NULL TREE) NIL 
		(cond ((numberp TREE) (= N TREE)) 
			((= N (second TREE)) t) 
			((< N (second TREE)) (TREE-CONTAINS N (first TREE))) 
			(t (TREE-CONTAINS N (third TREE))))))

; TREE-MAX takes an ordered tree TREE and finds the maximum number in TREE
; TREE-MAX returns a number
; I recursed through the right subtrees of each node to find the rightmost element, which is the maximum number

(defun TREE-MAX (TREE) 
	(cond ((NULL TREE) NIL) 
		((numberp TREE) TREE) 
		(t (TREE-MAX (third TREE)))))

; TREE-ORDER takes an ordered tree TREE and creates an in-ordered list of numbers in TREE
; TREE-ORDER returns a list
; In the base case, I created a list with the single number of the atom. I recursively appended the lists of numbers in the left subtree, middle element, and right subtree in order

(defun TREE-ORDER (TREE) 
	(cond ((NULL TREE) NIL) 
		((numberp TREE) (list TREE)) 
		(t (append (TREE-ORDER (first TREE)) (TREE-ORDER (second TREE)) (TREE-ORDER (third TREE))))))

; SUB-LIST takes three arguments, a list L and two non-negative integers START and LEN, and returns a sublist of L starting at position START and having length LEN
; SUB-LIST returns a list
; I started by reducing the START index to 0 by recursively looking at the rest of the list. I then constructed the list to return by recursively appending the first element of the list

(defun SUB-LIST (L START LEN) 
	(cond ((> START 0) (SUB-LIST (cdr L) (- START 1) LEN)) 
		(t (cond ((> LEN 0) (cons (car L) (SUB-LIST (cdr L) START (- LEN 1))))))))

; SPLIT-LIST takes a list L and returns a list of two lists L1 and L2 such that L is the result of appending L1 and L2 and the length of L2 minus length of L1 is 0 or 1
; SPLIT-LIST returns a list of two lists
; I created two conditions, one where the length of L would be odd and one where the length would be even. I then calculated the START and LEN values appropriately and called SUB-LIST

(defun SPLIT-LIST (L) 
	(cond ((oddp (length L)) 
		(list (SUB-LIST L 0 (/ (- (length L) 1) 2)) (SUB-LIST L (/ (- (length L) 1) 2) (- (length L) (/ (- (length L) 1) 2))))) 
		(t (list (SUB-LIST L 0 (/ (length L) 2)) (SUB-LIST L (/ (length L) 2) (/ (length L) 2))))))

; BTREE-HEIGHT takes a binary tree TREE and returns the height of TREE
; BTREE-HEIGHT returns a number
; I recursively add 1 to the higher of the left and right subtrees until I reach a single number at which point I stop

(defun BTREE-HEIGHT (TREE)
	(cond ((numberp TREE) 0) 
		((> (BTREE-HEIGHT (first TREE)) (BTREE-HEIGHT (second TREE))) (+ 1 (BTREE-HEIGHT (first TREE)))) 
		(t (+ 1 (BTREE-HEIGHT (second TREE))))))

; LIST2BTREE takes a non-empty list of atoms LEAVES and returns a binary tree such that the tree leaves are the elements of LEAVES and for any internal nodes in the tree, 
; the number of leaves in its right branch minus the number of leaves in its left branch is 0 or 1
; LIST2BTREE returns a binary tree in the form of lists of lists
; I have two base cases where LEAVES is either a list with a single element or two elements, and I return an atom or the list of two elements. Otherwise I recursively split the list using SPLIT-LIST

(defun LIST2BTREE (LEAVES)
	(cond ((= (length LEAVES) 1) (first LEAVES)) 
		((= (length LEAVES) 2) LEAVES) 
		(t (list (LIST2BTREE (first (SPLIT-LIST LEAVES))) (LIST2BTREE (second (SPLIT-LIST LEAVES)))))))

; BTREE2LIST takes a binary tree TREE and returns a list of atoms, with the function acting as the inverse of LIST2BTREE
; BTREE2LIST returns a list
; My base case is when TREE is a single element and I create a list with it. Otherwise I recursively append the left and right subtrees.

(defun BTREE2LIST (TREE)
	(cond ((numberp TREE) (list TREE)) (t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))))

; IS-SAME takes two LISP expressions E1 and E2 whose atoms are all numbers and checks whether the expressions are identical
; IS-SAME returns a boolean
; My base case is when both E1 and E2 are empty lists and I return true. Otherwise I recurse on both lists at the same time and check whether they're both atoms or both lists. If they're both atoms, 
; I check if the first elements are equal and then recursely check the rest of the lists. If they're both lists, I recursely call IS-SAME on both the first elements and the rest of the lists.

(defun IS-SAME (E1 E2)
	(cond ((and (NULL E1) (NULL E2)) t) 
		((and (numberp (first E1)) (numberp (first E2))) (and (equal (first E1) (first E2)) (IS-SAME (cdr E1) (cdr E2)))) 
		((and (listp (first E1)) (listp (first E2))) (and (IS-SAME (first E1) (first E2)) (IS-SAME (first E1) (first E2)))) 
		(t NIL)))

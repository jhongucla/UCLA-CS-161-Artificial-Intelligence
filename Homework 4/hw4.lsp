; Justin Hong
; 604565186

; For this homework, I consulted references on the DPLL algorithm that performs backtracking search to solve the CNF-SAT problem. I used
; wikipedia (https://en.wikipedia.org/wiki/DPLL_algorithm) for general information on the algorithm while I read slides at
; https://courses.cs.washington.edu/courses/cse421/12au/Backtrack.pdf for more specific details on how to implement the algorithm
; for this homework.

; sat? is the top level function. It takes two arguments n and delta, which are the number of variables and the
; propositional sentence as a list of lists respectively. It returns a list as the model of delta if delta is
; satisfiable, otherwise it returns NIL.
; sat? starts by calling the backtrack function and if it doesn't return NIL, it calls setVars on the result and returns it.

(defun sat? (n delta)
	(let* ((model (backtrack NIL delta)))
		(cond ((NULL model) NIL)
			(t (setVars n model)))))

; backtrack implements the backtrack search. It takes the variables n and the delta. It starts by choosing the next available variable. Then it
; checks if delta is NIL in which case we have found a model for delta and it returns the variable assignments. If there is a NIL in delta, then
; there is no variable assignment and backtrack returns NIL. The same case if there are no available variables. Then backtrack tries to simplify the
; delta with the positive and negative values of the chosen variable.

(defun backtrack (n delta)
	(let* ((nextvar (chooseVar (car (car delta)) delta)))
		(cond ((NULL delta) n)
			((occursNil delta) NIL)
			((NULL nextvar) NIL)
			(t (or (backtrack (cons nextvar n) (simplify (car (cons nextvar n)) delta)) 
				(backtrack (cons (- nextvar) (cdr n)) (simplify (car (cons (- nextvar) (cdr n))) delta)))))))

; setVars is a helper function. It takes a value n and a list model. It checks whether a model contains all the variables up the value specified by
; by n. If there are variables not in the model then their value does not matter, so setVars chooses a positive value for them and adds them to the
; model. This ensures that each model includes a value for every variable up to n. It returns the model after the values have been added.

(defun setVars (n model)
	(cond ((equal n 0) model)
		((and (not (occurs n model)) (not (occurs (- n) model))) (setVars (- n 1) (cons n model)))
		(t (setVars (- n 1) model))))

; occursNil is a helper function. It takes a delta, which is a list of lists, and then tries to find if there is a NIL anywhere in it. If so,
; it returns true otherwise it returns NIL.

(defun occursNil (delta)
	(cond ((NULL delta) NIL)
		((NULL (car (car delta))) t)
		(t (occursNil (cdr delta)))))

; occurs is a helper function. It takes a value n and a list list. It checks if the value n occurs in the list anywhere. If so, it returns true
; otherwise it returns NIL.

(defun occurs (n list)
	(cond ((NULL list) NIL)
		((equal (car list) n) t)
		(t (occurs n (cdr list)))))

; simplify is a helper function for sat? that simplifies the delta by assigning a value to a variable and then returning the result. It takes a
; variable var and a list of lists delta. It recurses through the lists in delta and checks if the positive and negative values of the variable
; occur in the lists. If the positive value occurs, then we remove the list it is in from delta. If the negative value occurs, we remove the
; variable from the list it is in. This function returns the delta after it has been simplified.

(defun simplify (var delta)
	(cond ((atom (car delta)) NIL)
		((and (not (occurs var (car delta))) 
			(not (occurs (- var) (car delta))))
			(cons (car delta) (simplify var (cdr delta))))
		((occurs var (car delta)) (simplify var (cdr delta)))
		(t (cons (removeVar var (car delta)) (simplify var (cdr delta))))))

; removeVar is a helper function. When given a variable var and a list list, it removes the negative value of the variable from the list and
; returns the rest of the list. 

(defun removeVar (var list)
	(cond ((NULL list) NIL)
		((equal (- var) (car list)) 
			(append NIL (removeVar var (cdr list))))
		(t (cons (car list) (removeVar var (cdr list))))))

; listLen is a helper function. It takes a list list and recursively finds the length of the list which is its return value. This function was
; made because we're not allowed to use the list-length function.

(defun listLen (list)
	(cond ((NULL list) 0)
		(t (+ 1 (listLen (cdr list))))))

; chooseVar is a helper function. It takes a variable var and a list of lists delta. It tries to get the variable that is in a list by itself 
; in delta. If no such variable exists then it will return the variable passed into the function.

(defun chooseVar (var delta)
	(cond ((NULL delta) var)
		((= (listLen (car delta)) 1) (car (car delta))) 
		(t (chooseVar var (cdr delta)))))

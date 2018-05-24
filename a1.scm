; WINATA, GENTA INDRA
; Student Id: 20378324

; COMP 4221 Assignment 1 - Spring 2017

; Extra for Part 1: 
; I handle the permutation duplicates (see permute function, you can toggle to remove duplicates or not)
; For example 
; query => 
; (permute '(a a b)) 
; output => 
; (a a b)
; (a b a)
; (b a a)
; #f

; Part 1

; Function => remove
; Description => Remove single x from lst
; Algorithm =>
; 1. Base: when lst is empty return empty list
; 2. Recursion: if element = x, but was removed before, don't remove it
; 3. else concat car lst with tail
; input = x : symbol, lst : list, is-remove: boolean
; output = list
(define (remove x lst is-remove)
	(if 
		(null? lst) `()
		(if 
			(and (eq? x (car lst)) (eq? is-remove #f)) (remove x (cdr lst) #t)
			(cons (car lst) (remove x (cdr lst) is-remove))
		)
	)
)

; Function => remove-duplicate
; Description => Remove duplicates for each element of list
; Algorithm =>
; 1. if any element found more than 1, than delete the element
; 2. num = number of x found in lst, if num > 1, remove the num else keep it
; 3. x is the element to be removed if found more than 1
; input = x: symbol, lst:list, num:symbol
; output = list;
(define (remove-duplicate x lst num)
	(if 
		(null? lst) `()
		(if 
			(and (equal? x (car lst)) (> num 0)) (remove-duplicate x (cdr lst) (+ num 1))
			(if
				(and (equal? x (car lst)) (= num 0)) (cons (car lst) (remove-duplicate x (cdr lst) (+ num 1)))
				(cons (car lst) (remove-duplicate x (cdr lst) (+ num 0)))
			)
		)
	)
)

; Function => remove-duplicate-iterator
; Description => Iterator to remove duplicates for each element of list
; Algorithm =>
; 1. will iterate each element of first-lst and remove each of them in lst and update lst in every time-step
; 2. accumulator = num of iterations, stop when list-length equals to accumulator
; input = first-lst: lst, lst:lst, list-length:symbol, accumulator:symbol
; output = list;
(define (remove-duplicate-iterator first-lst lst list-length accumulator)
	(if
		(eq? list-length accumulator) lst
		(remove-duplicate-iterator (cdr first-lst) (remove-duplicate (car first-lst) lst 0) list-length (+ accumulator 1))
	)
)

; Function => permute-generate
; Description => Permutate list, by adding first element and join it with its tail in recursion
; Algorithm =>
; 1. do apply append after the map
; 2. map i and cons with i and j, remove i in lst
; 3. Base: list with one length
; 4. Recursion: remove the i element and do cons i and j
; input = lst : list
; output = list
(define (permute-generate lst)
	(if
		(= (length lst) 1) (list lst)
		(apply append
			(map 
				(lambda (i) 
					(map (lambda (j) (cons i j))
						(permute-generate (remove i lst #f))
					)
				) lst)
		)
	)
)

; Function => list-each
; Description => Print the result
; Algorithm =>
; 1. Base: Print #f and newline if lst length is 0
; 2. Recursion: Print the permutation and following newline, do the recursion
; input = lst: list
; output = text with '#f in the end
(define (list-each lst)
	(cond
		((= (length lst) 0) (display #f) (newline))
		(else
			(display (car lst))
			(newline)
			(list-each (cdr lst))
		)
	)
)

; Function => permute
; Description => Permutation function
; Algorithm =>
; 1. handle duplicate permutations by call remove-duplicate-iterator
; 2. let all permutations in lst to permutations
; 3. print by calling list-each
; Remark => since, removing duplicates is time-consuming, you can call (list-each permutations) instead if you don't need to remove duplicates, it will be faster
; input = list: list
; output = text
(define (permute lst)  
	(let ((permutations (permute-generate lst)))
		; handling duplicates
		(list-each (remove-duplicate-iterator permutations permutations (length permutations) 0))
		; not handling duplicates
		; (list-each permutations)
    )
)


; Part 2

; Define weighted graph
(define g
  '((a . ((b . 5) (c . 8) (d . 3)))
   (b . ((a . 4) (c . 7)))
   (c . ((a . 2) (b . 6) (c . 2) (d . 9)))
   (d . ((b . 1) (c . 4)))))

; Function => iterate
; Description => Find the weight for corresponding start node (x) to end node
; Algorithms =>
; 1. Base: if the length of lst is 1, and car car lst is equal to x, do cdr car lst to get the weight, else return negative value (path is not found)
; 2. Recursion: if car car lst is equal to x, do cdr car lst to get the weight, else do the recursion 
; input = lst : list, x : symbol
; output = weight or -99999999 (if not found)
(define (iterate lst x)
	(if 
		(= (length lst) 1)
			(if
				(eq? (car (car lst)) x) (cdr (car lst))
				-99999999
			)
		(if
			(eq? (car (car lst)) x) (cdr (car lst))
			(iterate (cdr lst) x)
		)
	)
)

; Function => get-length
; Description => Find the weight for node a to node b by searching the graph
; Algorithm =>
; 1. Base: if length of g is one, and car car g equals to a, call iterate to find the weight between a and b
; 2. Recusion: else, if car car g equals to a, call iterate to find the weight between a and b
; 3. else, sum the tail length with 0
; input = g : list, a : symbol, b : symbol
; output = weight or -99999999 (if not found)
(define (get-length g a b)
	(if
		(= (length g) 1)
			(if
				(eq? (car (car g)) a) (iterate (cdr (car g)) b)
				-99999999
			)
		(if
			(eq? (car (car g)) a) (iterate (cdr (car g)) b)
			(+ 0 (get-length (cdr g) a b))
		)
	)
)

; Function => path-length-calculation
; Description => Generate pair and find the weight of each pair
; Algorithm =>
; 1. if the length is 1 or 0, display #f
; 2. Base: assign next_dis with the length between car lst and car cdr lst, car lst and car cdr lst is the pair between two adjacent nodes
; 3. Recursion: call path-length-calculation with tail of lst and added up all the weights
; input = g : list, lst: list
; output = weight
(define (path-length-calculation g lst)
	(if 
		(< (length lst) 2) (display #f)
		(if
			(= (length lst) 2)
				(let ((next_dis (get-length g (car lst) (car (cdr lst)))))
					(+ 0 next_dis)
				)
			(let ((next_dis (get-length g (car lst) (car (cdr lst)))))
				(+ next_dis (path-length-calculation g (cdr lst)))
			)
		)
	)
)

; Function => path-length
; Description => Main function of path-length
; Algorithm =>
; 1. print #f if only found 1 or 0 node in lst
; 2. else calculate the distance, if the distance (res) is less than 0, display #f and newline else display the path length and newline
; Remark => if path is not valid return #f
; input: g: list, lst: list
; output: text (to the standard output)
(define (path-length g lst)
	(cond 
		((< (length lst) 2) (display #f) (newline))
		(else
			(let ((res (path-length-calculation g lst)))
				(cond
					((< res 0) (display #f) (newline))
					(else (display res) (newline))
				)
			)
		)
	)
)

; Function => distance
; Description => same with path-length, but can take multiple arguments
; Algorithm =>
; 1. Call the path-length
; (see path-length function)
; 2. print #f if only found 1 or 0 node in lst
; 3. else calculate the distance, if the distance (res) is less than 0, display #f and newline else display the path length and newline
; input: g: list, args: symbols
; output: text (to the standard output)
(define (distance g . args)
	(path-length g args)
)
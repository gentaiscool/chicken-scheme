; WINATA, GENTA INDRA
; Student Id: 20378324

; COMP 4221 Assignment 2 - Spring 2017

; Part 1
; Define dictionary
(define dictionary '(a act ale at ate cat eat etc tea))

; Function => list-to-string
; Description => Convert list to string
; Algorithm =>
; 1. Base: if lst has length 1, convert symbol to string the head element of lst
; 2. Recursive: Append the string and recur list-to-string the tail of lst
; input = lst: list
; output = string
(define (list-to-string lst)
	(cond
		((= (length lst) 1) (symbol->string (car lst)))
		(else (string-append (symbol->string (car lst)) (list-to-string (cdr lst))))
	)
)

; Function => is-contains-string
; Description => Check if the pattern is inside arr
; Algorithm =>
; 1. Base: if arr has length 1, convert symbol to string the head element of arr and check it with pattern, return boolean
; 2. Recursive: if pattern is equals to string of head element of arr return true, otherwise recur the tail of arr
; input = pattern: string, arr: list
; output = boolean
(define (is-contains-string pattern arr)
	(cond
		((= (length arr) 1) 
			(if (equal? pattern (symbol->string (car arr)))
				#t
				#f
			)
		)
		(else 
			(cond 
				((equal? pattern (symbol->string (car arr))) #t)
				(else (is-contains-string pattern (cdr arr)))
			) 
		)
	)
)

; Function => create-list-in-dictionary
; Description => Create a list of all anagrams that are found inside the dictionary
; Algorithm =>
; 1. Base: if length of res equals to 1, if the head of res is inside the dict, return res, otherwise empty list
; 2. Recursion: if the head element of res is inside dict, cons the element with the recursion of the tail, otherwise recur the tail
; input = dict: lst, res: lst
; output = list
(define (create-list-in-dictionary dict res)
	(cond
		((= (length res) 0)
			'()
		)
		((= (length res) 1)
			(cond
				((is-contains-string (list-to-string (car res)) dict) 
					res
				)
				(else 
					'()
				)
			)
		)
		(else
			(cond 
				((is-contains-string (list-to-string (car res)) dict) 
					(cons (car res) (create-list-in-dictionary dict (cdr res)))
				)
				(else 
					(create-list-in-dictionary dict (cdr res))
				)
			)
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
	(if (> (length lst) 0)
		(begin
			(display (car lst))
			(newline)
			(list-each (cdr lst))
		)
	)
)

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
; Function => appender
; Desciption => Append i to lst
; Algorithm =>
; 1. if lst length is 1 => return list concatenate i and head of lst
; 2. else if the length is 0 => return i, else return concatenate i with head of lst and concatenate with its recursion
; input = i : symbol, lst : list
; output = list : list
(define (appender i lst)
	(cond
		((= (length lst) 1) (list (cons i (car lst))))
		(else
			(if (= (length lst) 0)
				(list i)
				(cons (cons i (car lst)) (appender i (cdr lst)))
			)
		)
	) 
)

; Function => permute-generator
; Description => Print permutation that contains anagrams in the dictionary
; Algorithm =>
; 1. if the length of dict is greater than 0 and if the length of nlst is greater than 0,
; let first = (car nlst), rest (remove (car nlst) in lst)
; 2. if the length of lst is 1 => display lst if it is found the dict
; 3. else, let res to be the concatenation of first and the permutations of its tail
; 4. recurrence: call permute-generator of the tail of the nlst
; lst: list of input, nlst: list of input that will be iterated
; input = dict: list, lst: list, nlst: list
; output = print
(define (permute-generator dict lst nlst)
	(if (> (length dict) 0)
		(if (> (length nlst) 0)
			(let ((first (car nlst)) (rest (remove (car nlst) lst #f)))
				(cond 
					((= (length lst) 1) 
						(if 
							(is-contains-string (symbol->string (car lst)) dict)
							(begin
								(display lst)
								(newline)
							)
						)
					)
					(else 
						(let ((res (appender first (permute-generate rest))))
							(if 
								(eq? dict '()) (list-each '())
								(list-each (create-list-in-dictionary dict res))
							)
							(permute-generator dict lst (cdr nlst))
						)
					) 
				)
			)
		)
	)
)

; Function => anagram
; Description => Find all anagram (lst) in the dictionary (dict)
; Algorithm =>
; 1. let res equals to the permutation result
; 2. if dict is empty, return #f
; 3. else generate the list of anagram and print
; input = dict: list, lst: list
; output = result
(define (anagram dict lst)
	(cond
		((= (length lst) 0) 
			(begin
				(display #f)
				(newline)
			)
		)
		(else 
			(begin
				(permute-generator dict lst lst)
				(display #f)
				(newline)
			)
		)
	)
)

; Part 2
; Define macro for my-or using define-syntax
; Algorithm =>
; 1. define macro of my-or
; 2. let t equals to t
; 3. if t is not #f print t, else y
; input = x, y
; output = result
(define-syntax my-or
	(syntax-rules ()
		(
			(_ x y) 
			(let ((t x)) 
				(if
					t t y
				)
			)
		)
	)
 )
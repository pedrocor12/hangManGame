;; hang-man for REPL Scheme


;;
(define source-name "glossary.txt")

;; Side effect:
;; Strig-> IO([String])
;; Passed the path, open the file containig glossary
(define (read-words-from filename)
  (let* ((port (open-input-file filename))
         (res (read-word-list port '())))
    (close-input-port port)
    res))

;; Side effect
;; Fd -> [String] -> IO ([String])
;; Passed port and acumulator, return the all the words as strings
(define (read-word-list port acc)
  (let ((stuff (read port)))
    (if (eof-object? stuff)
        acc
        (read-word-list port
                        (cons (symbol->string stuff) acc)))))

(define list-of-words (read-words-from source-name))

;; STATE OF THE GAME
(define glossary (map string->list list-of-words)) 
(define word-to-guess
(list-ref glossary(random(length glossary)) ;Getting a random word from the glossary by using list ref and getting a random word from the length
          )
  )                      
(define total-hits (length word-to-guess))

(define partial-sol
  (string->list (make-string total-hits #\*) ;;Making partial solution a list of stars
                )
  )

(define hits 0)
(define plays 0)
(define failures 0)
(define total-failures 6)




;; 
;; IO(String)
(define (game-status)
  (begin
    (format "~a H:~a/~a F:~a/~a ~a ~a"
            (list->string partial-sol)
            hits  total-hits
            failures  total-failures
            plays
            (if (and
                 (< hits total-hits)
                 (< failures total-failures))
                ""
                (string-append "GAME-OVER(" (list->string word-to-guess) ")")))))
          
;;;
;;  PURELY FUNCTIONAL
;;


;occurrences 
(define (occurrences word char)
  (cond
    [(null? word) 0] ;If word is empty returns 0
    [(equal? (car word) char) (+ (occurrences (cdr word) char)1)] ;(Base case comparing first element with char) (increasing occurences if finds a match)
    [else
     (occurrences(cdr word) char) ;recursion step checking for the rest of the list against char
     ]
    )
  )

            
;Indices given a char returns the position of that char is in the given word.
(define (indices word char)
  (let loop ((word word)  ;Initializing word twice because we don't want any changes to be made to it.
             (original-list '()) ;Initializing the list which is going to hold the values for the index.
             (index 0))  ;;Initializing our counter for index
    (cond
      [(empty? word) (reverse original-list)] 
      [(equal? (first word) char) ; Base case checking if first element of word is equal to the char.
       (loop (rest word)  ;Looping the rest of the list. 
             (cons index original-list)  ;;Adding the values to the. 
             (+ 1 index))]               ;;Increasing our counter everytime we find a value
      [else                     
       (loop (rest word)   ;Recursion Step
              original-list
             (+ 1 index))]
      )
    )
  )

;Replace indices using a given index position in the word ,appending that word into the new list
(define (replace-indices word idx new)
  (cond
    [(empty? word) word]
    [(empty? idx) word]
    [(= 0(car idx)) ;;base case checking if the index is equal to 0
     (cons new(replace-indices(cdr word) (map sub1(cdr idx)) new))] ;; map is decrementing one each time from the idx list and adding it to the new list
    [else
     (cons (car word) (replace-indices(cdr word) (map sub1 idx) new))] ;Recursion step
    )
  )


;Number of hits checking for hits in the given list
(define (noOfHits hidden)
  (cond
    [(null? hidden) 0]
    [(not (eq? '#\* (car hidden))) (+ 1 (noOfHits (cdr hidden)))] ;(Base case checking if the first element of hidden is equal to the char increasing hits if it is)
    [else
     (noOfHits (cdr hidden))] ;Recursion step
    )
  )


            
;; Side effects
;; IO(String)
(define (restart)
  (begin
    (set! partial-sol (string->list (make-string total-hits #\*))) ;Setting partial-sol into star chars
    (set! hits 0) ;Hits reset to 0
    (set! plays 0) ;Plays reset to 0
    (set! failures 0) ;Failures reset to 0
    (game-status)
    )
  )


;; Char -> IO(String)
(define (guess char) (void)
  (begin
    (set! plays (+ plays 1))
    (if(member char partial-sol)  ;;(I have added this so the hits dont increase for the already partially guesses word)
       (set! hits (- hits 1)))
    (set! partial-sol(replace-indices partial-sol (indices word-to-guess char) char))
    (cond
      [(= (occurrences word-to-guess char) 0)  ;;(Occurences are equal to 0?) (so it will side effect failures increasing it by 1)
       (set! failures (+ failures 1))]
      [(> (noOfHits partial-sol) 0)            ;;(No of hits is bigger then 0?) (so it will side effect hits increasing it by 1)
       (set! hits (+ hits 1))])
    (game-status)
    )
  )

;(set! guess (string->list (make-string total-hits# #\*))) 
;(set! partial-sol(replace-indices partial-sol (indices word-to-guess char) char))
;; IO(String)
(define (solve word) 
  (let ((loop (string->list word)))
    (begin(for-each (lambda (stringgiven) ; Initializing a varaiable called stringgiven which we are going to use as a the list the word chars.
                      (if (member stringgiven partial-sol) ;Is the string which we converted to a list of chars part of the partial sol?
                          (void)(guess stringgiven))) loop) ;So it guess the string given going over each one of the missing character.
          (set! hits (length word-to-guess)) ;Applying side effects to hits increasing it to the length of the word bringing the game over status for the game.
          (game-status)
          )
    )
  )
  
;;
;; EXTRA -F3
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; p: all-words as list of list of char
(define (words-containing all-words char )
  (cond
    [(empty? all-words) empty]  ;;[Checking if all-words is empty]
    [(member char (car all-words)) ;;[Base case Checking char is a member in all-words where we are visiting the first list inside the list of lists]
     (cons(car all-words)(words-containing(cdr all-words)char))] ;;[if it is a member we extract into words containing and we check for the other lists]
    [else
     (words-containing(cdr all-words)char)] ;Recursion step
    )
  )
                                              

;; p: all-words as list of list of char
;;  : chars as a list of char
(define (words-containing-ext all-words chars)
  (foldl ;Foldl which is going to apply a procedure that i choose to the values.
   (λ (cha wor) (words-containing wor cha)) all-words chars)) ;[Providing the procedure which is words-containing, providing what i want to check and foldl is going to return the lists which contain those chars from the list of lists of chars]



;; IO([String])
;; this is very hard.
(define (sieve chars) (void)) ; I was not able to achieve this function


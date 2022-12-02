(use-modules (ice-9 match)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-26))

(define %example "\
A Y
B X
C Z
")

(define parse-their-shape
  (match-lambda
    ("A" 'rock)
    ("B" 'paper)
    ("C" 'scissors)))

(define parse-my-shape
  (match-lambda
    ("X" 'rock)
    ("Y" 'paper)
    ("Z" 'scissors)))

(define (parse-rounds str)
  (fold-right
   (lambda (line acc)
     (if (string-null? line) acc
         (let* ((tokens (string-split line #\space))
                (theirs (parse-their-shape (first tokens)))
                (mine (parse-my-shape (second tokens))))
           (cons (list mine theirs) acc))))
   '() (string-split str #\newline)))

(define shape-score
  (match-lambda
    ('rock     1)
    ('paper    2)
    ('scissors 3)))

(define shape-beats?
  (match-lambda*
    (('rock 'scissors)  #t)
    (('paper 'rock)     #t)
    (('scissors 'paper) #t)
    (_ #f)))

(define (round-score mine theirs)
  (+ (shape-score mine)
     (cond ((shape-beats? mine theirs) 6)
           ((eq? mine theirs) 3)
           (else 0))))

(define (puzzle-0)
  (let* ((rounds (parse-rounds %example))
         (scores (map (cut apply round-score <>) rounds)))
    (apply + scores)))

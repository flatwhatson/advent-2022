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

(define* (parse-rounds str #:optional (parse-my-strategy parse-my-shape))
  (fold-right
   (lambda (line acc)
     (if (string-null? line) acc
         (let* ((tokens (string-split line #\space))
                (theirs (parse-their-shape (first tokens)))
                (mine (parse-my-strategy (second tokens))))
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

(define (puzzle-1)
  (let* ((input (call-with-input-file "data/input-02" get-string-all))
         (rounds (parse-rounds input))
         (scores (map (cut apply round-score <>) rounds)))
    (apply + scores)))

(define parse-lose-draw-win
  (match-lambda
    ("X" 'lose)
    ("Y" 'draw)
    ("Z" 'win)))

(define select-win
  (match-lambda
    ('rock 'paper)
    ('paper 'scissors)
    ('scissors 'rock)))

(define select-lose
  (match-lambda
    ('rock 'scissors)
    ('paper 'rock)
    ('scissors 'paper)))

(define (select-shape strategy theirs)
  (match strategy
    ('win (select-win theirs))
    ('lose (select-lose theirs))
    ('draw theirs)))

(define (round-score-2 my-strat theirs)
  (let ((mine (select-shape my-strat theirs)))
    (round-score mine theirs)))

(define (puzzle-2)
  (let* ((input (call-with-input-file "data/input-02" get-string-all))
         (rounds (parse-rounds input parse-lose-draw-win))
         (scores (map (cut apply round-score-2 <>) rounds)))
    (apply + scores)))

(use-modules (ice-9 match)
             (ice-9 peg)
             (ice-9 textual-ports)
             (srfi srfi-1))

(define %example "\
    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
")

(define-peg-string-patterns
  "space   < ' '
   newline < '\n'
   lbrace  < '['
   rbrace  < ']'

   crate  <-- lbrace [A-Z] rbrace
   blank  <-- space space space
   number <-  [0-9]+

   stack-row    <- (blank space?/crate space?)+ newline
   stack-labels <  (space* number+)+ newline
   stacks       <-- stack-row+ stack-labels newline

   move-label < 'move '
   from-label < ' from '
   to-label   < ' to '

   move <-- move-label number
   from <-- from-label number
   to   <-- to-label number

   command  <-- move from to newline
   commands <-- command+

   puzzle-input <- stacks commands
 ")

(define (build-stacks raw-stacks)
  (let* ((num-stacks (apply max (map length raw-stacks)))
         (stacks (make-vector num-stacks '())))
    (for-each
     (lambda (crates)
       (format #t "crates: ~a\n" crates)
       (for-each (lambda (crate ix)
                   (when (not (eq? crate 'blank))
                     (format #t "adding ~a to ~a\n" crate ix)
                     (let ((stack (vector-ref stacks ix)))
                       (vector-set! stacks ix (cons crate stack)))))
                 crates
                 (iota (length crates))))
     (reverse raw-stacks))
    stacks))

(define (run-command command stacks)
  (define (pop-stack! ix)
    (let ((stack (vector-ref stacks ix)))
      (vector-set! stacks ix (cdr stack))
      (car stack)))
  (define (push-stack! ix crate)
    (let ((stack (vector-ref stacks ix)))
      (vector-set! stacks ix (cons crate stack))))
  (match command
    (('command ('move count) ('from src) ('to dst))
     (let ((count (string->number count))
           (src (1- (string->number src)))
           (dst (1- (string->number dst))))
       (format #t "move ~a from ~a to ~a\n" count (1+ src) (1+ dst))
       (for-each (lambda (n)
                   (push-stack! dst (pop-stack! src)))
                 (iota count))
       stacks))))

(define (run-commands stacks commands)
  (fold run-command stacks commands))

(define (puzzle-0)
  (match (peg:tree (match-pattern puzzle-input %example))
    ((('stacks . raw-stacks) ('commands . raw-commands))
     (let* ((stacks-start (build-stacks raw-stacks))
            (stacks-end (run-commands stacks-start raw-commands))
            (top-crates (map first (vector->list stacks-end))))
       (apply string-append (map second top-crates))))))

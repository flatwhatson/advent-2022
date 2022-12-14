(use-modules (ice-9 match)
             (ice-9 peg)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-71))

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
   stack-labels <  (space* number+ space*)+ newline
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

(define (run-command-1 command stacks)
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

(define (run-command-2 command stacks)
  (match command
    (('command ('move count) ('from src) ('to dst))
     (let ((count (string->number count))
           (src (1- (string->number src)))
           (dst (1- (string->number dst))))
       (format #t "move ~a from ~a to ~a\n" count (1+ src) (1+ dst))
       (let* ((src-stack (vector-ref stacks src))
              (dst-stack (vector-ref stacks dst))
              (took rest (split-at src-stack count)))
         (vector-set! stacks src rest)
         (vector-set! stacks dst (append took dst-stack))
         stacks)))))

(define (puzzle-0)
  (match (peg:tree (match-pattern puzzle-input %example))
    ((('stacks . raw-stacks) ('commands . commands))
     (let* ((stacks (build-stacks raw-stacks))
            (stacks (fold run-command-1 stacks commands))
            (top-crates (map first (vector->list stacks))))
       (apply string-append (map second top-crates))))))

(define (puzzle-1)
  (define %input (call-with-input-file "data/input-05"
                   get-string-all))
  (match (peg:tree (match-pattern puzzle-input %input))
    ((('stacks . raw-stacks) ('commands . commands))
     (let* ((stacks (build-stacks raw-stacks))
            (stacks (fold run-command-1 stacks commands))
            (top-crates (map first (vector->list stacks))))
       (apply string-append (map second top-crates))))))

(define (puzzle-2)
  (define %input (call-with-input-file "data/input-05"
                   get-string-all))
  (match (peg:tree (match-pattern puzzle-input %input))
    ((('stacks . raw-stacks) ('commands . commands))
     (let* ((stacks (build-stacks raw-stacks))
            (stacks (fold run-command-2 stacks commands))
            (top-crates (map first (vector->list stacks))))
       (apply string-append (map second top-crates))))))

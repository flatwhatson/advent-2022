(use-modules (ice-9 match)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-9))

(define %example "\
30373
25512
65332
33549
35390
")

(define-record-type <grid>
  (%make-grid data rows cols)
  grid?
  (data grid-data)
  (rows grid-rows)
  (cols grid-cols))

(define (make-grid data)
  (let ((rows (string-count data #\newline))
        (cols (string-index data #\newline)))
    (%make-grid data rows cols)))

(define (grid-index grid x y)
  (unless (and (< -1 x (grid-cols grid))
               (< -1 y (grid-rows grid)))
    (error "invalid grid coordinate:" x y))
  (+ x (* y (1+ (grid-cols grid)))))

(define (grid-ref grid x y)
  (string-ref (grid-data grid) (grid-index grid x y)))

(define (grid-coordinate-fold proc init grid)
  (let ((cols (grid-cols grid))
        (rows (grid-rows grid)))
    (let loop ((x 0) (y 0) (result init))
      (cond ((= y rows)
             result)
            ((= x cols)
             (loop 0 (1+ y) result))
            (else
             (loop (1+ x) y (proc grid x y result)))))))

(define (grid-coordinate-map proc grid)
  (reverse (grid-coordinate-fold
            (lambda (grid x y ls)
              (cons (proc grid x y) ls))
            '() grid)))

(define (grid-line-ref grid x y direction)
  (define data (grid-data grid))
  (define 1row (1+ (grid-cols grid)))
  (define count
    ;; cells between the edge and x,y
    (case direction
      ((north) y)
      ((east) (- (grid-cols grid) x 1))
      ((south) (- (grid-rows grid) y 1))
      ((west) x)))
  (define step
    ;; step from the edge towards x,y
    (case direction
      ((north) 1row)
      ((east) -1)
      ((south) (- 1row))
      ((west) 1)))
  (define start
    ;; start from the edge
    (+ (grid-index grid x y)
       (* count (- step))))
  (let loop ((ix start) (count count) (result '()))
    (if (zero? count)
        result
        (loop (+ ix step)
              (1- count)
              (cons (string-ref data ix) result)))))

(define (is-edge? grid x y)
  (or (= x 0) (= x (1- (grid-cols grid)))
      (= y 0) (= y (1- (grid-rows grid)))))

(define (is-visible? grid x y)
  (or
   ;; edges are always visible
   (is-edge? grid x y)
   ;; visible when every tree in a direction is shorter
   (let ((height (grid-ref grid x y)))
     (any (lambda (direction)
            (every (lambda (c)
                     (char<? c height))
                   (grid-line-ref grid x y direction)))
          '(north east south west)))))

(define (count-trees height trees)
  (let loop ((trees trees) (count 0))
    (cond ((null? trees)
           count)
          ((char>=? (car trees) height)
           (1+ count))
          (else
           (loop (cdr trees) (1+ count))))))

(define (scenic-score grid x y)
  (if (is-edge? grid x y) 0
      (let ((height (grid-ref grid x y)))
        (apply * (map (lambda (direction)
                        (let ((trees (grid-line-ref grid x y direction)))
                          (count-trees height trees)))
                      '(north east south west))))))

(define (puzzle-0)
  (count values (grid-coordinate-map
                 is-visible? (make-grid %example))))

(define (puzzle-1)
  (count values (grid-coordinate-map
                 is-visible? (make-grid
                              (call-with-input-file "data/input-08"
                                get-string-all)))))

(define (puzzle-2)
  (apply max (grid-coordinate-map
              scenic-score (make-grid
                            (call-with-input-file "data/input-08"
                              get-string-all)))))

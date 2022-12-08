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

(define (grid-coordinate-count pred grid)
  (grid-coordinate-fold (lambda (grid x y n)
                          (if (pred grid x y)
                              (1+ n)
                              n))
                        0 grid))

(define (grid-line-ref grid x y direction)
  (define next-x (case direction
                   ((west) 1-)
                   ((east) 1+)
                   (else values)))
  (define next-y (case direction
                   ((north) 1-)
                   ((south) 1+)
                   (else values)))
  (define done? (case direction
                  ((west) (lambda (x y) (< x 0)))
                  ((east) (lambda (x y) (= x (grid-cols grid))))
                  ((north) (lambda (x y) (< y 0)))
                  ((south) (lambda (x y) (= y (grid-rows grid))))))
  (let loop ((x (next-x x))
             (y (next-y y))
             (result '()))
    (if (done? x y) result
        (loop (next-x x)
              (next-y y)
              (cons (grid-ref grid x y) result)))))

(define (is-visible? grid x y)
  (or
   ;; edges are always visible
   (= x 0) (= x (1- (grid-cols grid)))
   (= y 0) (= y (1- (grid-rows grid)))
   ;; visible when adjacent is visible and shorter
   (let* ((height (grid-ref grid x y))
          (shorter? (lambda (c) (char<? c height))))
     (any (lambda (direction)
            (every shorter? (grid-line-ref grid x y direction)))
          '(north east south west)))))

(define (count-visible grid)
  (grid-coordinate-count is-visible? grid))

(define (puzzle-0)
  (count-visible (make-grid %example)))

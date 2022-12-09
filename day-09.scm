(use-modules (ice-9 match)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-71))

(define %example "\
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
")

(define (parse-motions str)
  (filter-map
   (lambda (line)
     (and (not (string-null? line))
          (let ((parts (string-split line #\space)))
            (cons (match (first parts)
                    ("U" 'up)
                    ("D" 'down)
                    ("L" 'left)
                    ("R" 'right))
                  (string->number (second parts))))))
   (string-split str #\newline)))

(define-record-type <point>
  (make-point x y)
  vec?
  (x point-x)
  (y point-y))

(define (point-difference a b)
  (values (- (point-x a) (point-x b))
          (- (point-y a) (point-y b))))

(define (move-head p direction)
  (let ((x (point-x p)) (y (point-y p)))
    (match direction
      ('up    (make-point x (1+ y)))
      ('down  (make-point x (1- y)))
      ('left  (make-point (1- x) y))
      ('right (make-point (1+ x) y)))))

(define (move-tail head tail)
  (define (unit n)
    (if (negative? n) (max n -1) (min n 1)))
  (let* ((dx dy (point-difference head tail))
         (distance (max (abs dx) (abs dy))))
    (if (>= distance 2)
        (make-point (+ (point-x tail) (unit dx))
                    (+ (point-y tail) (unit dy)))
        tail)))

(define (fold-motions proc init moves)
  (let ((head (make-point 0 0))
        (tail (make-point 0 0)))
    (let loop ((moves moves) (head head) (tail tail) (result init))
      (match moves
        (() result)
        (((_ . 0) . moves)
         (loop moves head tail result))
        (((direction . count) . moves)
         (let* ((move (cons direction (1- count)))
                (head (move-head head direction))
                (tail (move-tail head tail)))
           (loop (cons move moves) head tail
                 (proc head tail result))))))))

(define (puzzle-0)
  (let* ((moves (parse-motions %example))
         (tails (fold-motions (lambda (head tail result)
                                (cons tail result))
                              '() moves)))
    (length (delete-duplicates tails equal?))))

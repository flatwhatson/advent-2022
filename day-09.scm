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

(define (move-rope prev direction)
  (define size (vector-length prev))
  (define next (make-vector size))
  (let loop ((i 0))
    (cond ((zero? i)
           (let* ((head (vector-ref prev 0))
                  (head (move-head head direction)))
             (vector-set! next 0 head)
             (loop (1+ i))))
          ((< i size)
           (let* ((head (vector-ref next (1- i)))
                  (tail (vector-ref prev i))
                  (tail (move-tail head tail)))
             (vector-set! next i tail)
             (loop (1+ i))))
          (else next))))

(define (fold-motions proc init moves num-points)
  (define points (make-vector num-points (make-point 0 0)))
  (define last-ix (1- (vector-length points)))
  (let loop ((moves moves) (points points) (result init))
    (match moves
      (() result)
      (((_ . 0) . moves)
       (loop moves points result))
      ((and moves ((and move (direction . count)) . _))
       (let* ((points (move-rope points direction))
              (tail (vector-ref points last-ix)))
         (set-cdr! move (1- count))
         (loop moves points (proc tail result)))))))

(define (count-unique-points moves num-points)
  (length (fold-motions (lambda (tail result)
                                (if (not (member tail result))
                                    (cons tail result)
                                    result))
                              '() moves num-points)))

(define (puzzle-0)
  (let ((moves (parse-motions %example))
        (knots 2))
    (count-unique-points moves knots)))

(define (puzzle-1)
  (let ((moves (parse-motions (call-with-input-file "data/input-09"
                                get-string-all)))
        (knots 2))
    (count-unique-points moves knots)))

(define (puzzle-2)
  (let ((moves (parse-motions (call-with-input-file "data/input-09"
                                get-string-all)))
        (knots 10))
    (count-unique-points moves knots)))

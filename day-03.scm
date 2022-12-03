(use-modules (ice-9 match)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-71))

(define %example "\
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
")

(define (string->lines str)
  (filter (negate string-null?)
          (string-split str #\newline)))

(define (string-split-evenly str)
  (let* ((len (string-length str))
         (mid (floor (/ len 2))))
    (values (substring str 0 mid)
            (substring str mid len))))

(define (find-duplicate-chars a b)
  (char-set->list
   (char-set-intersection
    (string->char-set a)
    (string->char-set b))))

(define (char->priority c)
  (let ((i (char->integer c))
        (a (char->integer #\a))
        (z (char->integer #\z))
        (A (char->integer #\A))
        (Z (char->integer #\Z)))
    (cond ((<= a i z) (+ (- i a) 1))
          ((<= A i Z) (+ (- i A) 27))
          (else (error "invalid char:" c)))))

(define (puzzle-0)
  (fold (lambda (str acc)
          (let* ((a b (string-split-evenly str))
                 (dup (first (find-duplicate-chars a b))))
            (+ acc (char->priority dup))))
        0 (string->lines %example)))

(define (puzzle-1)
  (fold (lambda (str acc)
          (let* ((a b (string-split-evenly str))
                 (dup (first (find-duplicate-chars a b))))
            (+ acc (char->priority dup))))
        0 (string->lines
           (call-with-input-file "data/input-03"
             get-string-all))))

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

(define (find-duplicate-chars . strs)
  (char-set->list
   (apply char-set-intersection
          (map string->char-set strs))))

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

(define (puzzle-2)
  (define %lines (string->lines
                  (call-with-input-file "data/input-03"
                    get-string-all)))
  (let loop ((lines %lines) (acc 0))
    (if (null? lines) acc
        (let* ((group rest (split-at lines 3))
               (badge (first (apply find-duplicate-chars group))))
          (loop rest (+ acc (char->priority badge)))))))

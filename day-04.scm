(use-modules (ice-9 match)
             (ice-9 textual-ports)
             (srfi srfi-1))

(define %example "\
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
")

(define separator?
  (let ((valid (char-set #\- #\,)))
    (lambda (c)
      (char-set-contains? valid c))))

(define (parse-input str)
  (filter-map (lambda (line)
                (and (not (string-null? line))
                     (map string->number
                          (string-split line separator?))))
              (string-split str #\newline)))

(define (full-overlap? a1 a2 b1 b2)
  (or (<= a1 b1 b2 a2)
      (<= b1 a1 a2 b2)))

(define (part-overlap? a1 a2 b1 b2)
  (or (<= a1 b1 a2)
      (<= a1 b2 a2)
      (<= b1 a1 b2)
      (<= b1 a2 b2)))

(define (puzzle-0)
  (count (lambda (nums)
           (apply full-overlap? nums))
         (parse-input %example)))

(define (puzzle-1)
  (count (lambda (nums)
           (apply full-overlap? nums))
         (parse-input (call-with-input-file "data/input-04"
                        get-string-all))))

(define (puzzle-2)
  (count (lambda (nums)
           (apply part-overlap? nums))
         (parse-input (call-with-input-file "data/input-04"
                        get-string-all))))

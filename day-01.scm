(use-modules (ice-9 match)
             (ice-9 textual-ports)
             (srfi srfi-1))

(define %example "\
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
")

(define (parse-calories str)
  (let loop ((lines (string-split str #\newline)) (vals '()) (result '()))
    (match lines
      ('()
       (reverse (cons (reverse vals) result)))
      (("" . rest)
       (loop rest '() (cons (reverse vals) result)))
      ((num . rest)
       (loop rest (cons (string->number num) vals) result)))))

(define (list-sum vals)
  (apply + vals))

(define (largest-calorie-sum data)
  (apply max (map list-sum data)))

(define (top-calorie-sums n data)
  (take (sort (map list-sum data) >) n))

(define (puzzle-0)
  (largest-calorie-sum (parse-calories %example)))

(define (puzzle-1)
  (define %input (call-with-input-file "data/input-01" get-string-all))
  (define %data (parse-calories %input))
  (largest-calorie-sum %data))

(define (puzzle-2)
  (define %input (call-with-input-file "data/input-01" get-string-all))
  (define %data (parse-calories %input))
  (list-sum (top-calorie-sums 3 %data)))

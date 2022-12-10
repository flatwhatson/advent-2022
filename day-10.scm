(use-modules (ice-9 match)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-9))

(define %example "\
noop
addx 3
addx -5
")

(define (parse-program str)
  (filter-map (lambda (line)
                (match (string-split line #\space)
                  (("") #f)
                  ((cmd) (string->symbol cmd))
                  ((cmd arg) (cons (string->symbol cmd)
                                   (string->number arg)))))
              (string-split str #\newline)))

(define (fold-program proc init program)
  (let loop ((program program) (cycle 1) (regx 1) (result init))
    (match program
      (()
       (proc cycle regx result))
      (('noop . program)
       (let ((result (proc cycle regx result)))
         (loop program (1+ cycle) regx result)))
      ((('addx . n) . program)
       (let* ((result (proc cycle regx result))
              (result (proc (1+ cycle) regx result)))
         (loop program (+ cycle 2) (+ regx n) result))))))

(define (sum-strengths program)
  (fold-program (lambda (cycle regx sum)
                  (if (= (modulo cycle 40) 20)
                      (+ (* cycle regx) sum)
                      sum))
                0 program))

(define (display-pixels program)
  (fold-program (lambda (cycle regx _)
                  (let ((pixel (modulo (1- cycle) 40)))
                    (if (or (= pixel regx)
                            (= pixel (1- regx))
                            (= pixel (1+ regx)))
                        (display #\#)
                        (display #\.))
                    (when (= pixel 39)
                      (newline))))
                #t program))

(define (puzzle-0)
  (sum-strengths (parse-program
                  (call-with-input-file "data/input-10-example"
                    get-string-all))))

(define (puzzle-1)
  (sum-strengths (parse-program
                  (call-with-input-file "data/input-10"
                    get-string-all))))

(define (puzzle-2)
  (display-pixels (parse-program
                   (call-with-input-file "data/input-10"
                     get-string-all))))

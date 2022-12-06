(use-modules (ice-9 textual-ports)
             (srfi srfi-1))

(define %examples
  '("mjqjpqmgbljsphdztnvjfqwrcgsmlb"    ; 7
    "bvwbjplbgvbhsrlpgdmjqwftvncz"      ; 5
    "nppdvjthqldpwncqszvftbrmjlhg"      ; 6
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" ; 10
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"  ; 11
    ))


(define (unique-chars? str)
  (= (string-length str)
     (char-set-size (string->char-set str))))

(define (string-scan pred len str)
  (let loop ((start 0) (end len))
    (cond ((>= end (string-length str))
           #f)
          ((unique-chars? (substring str start end))
           end)
          (else
           (loop (1+ start) (1+ end))))))

(define (puzzle-0)
  (for-each (lambda (input)
              (format #t "~a => ~a\n" input
                      (string-scan unique-chars? 4 input)))
            %examples))

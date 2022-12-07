(use-modules (ice-9 match)
             (ice-9 peg)
             (ice-9 pretty-print)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-9))

(define %example "\
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
")

;;; Input parser

(define-peg-pattern filename* body
  (+ (or (range #\a #\z) ".")))

(define-peg-pattern filename all
  filename*)

(define-peg-pattern filesize all
  (+ (range #\0 #\9)))

(define-peg-pattern command-cd all
  (and (ignore "$ cd ") (or "/" ".." filename*) (ignore "\n")))

(define-peg-pattern command-ls all
  (ignore "$ ls\n"))

(define-peg-pattern output-dir all
  (and (ignore "dir ") filename (ignore "\n")))

(define-peg-pattern output-file all
  (and filesize (ignore " ") filename (ignore "\n")))

(define-peg-pattern terminal-output body
  (* (or command-cd command-ls output-dir output-file)))

(define (parse-terminal-output str)
  (peg:tree (match-pattern terminal-output str)))

;;; Filesystem structures

(define-record-type <dir>
  (%make-dir name size entries)
  dir?
  (name dir-name)
  (size dir-size set-dir-size!)
  (entries dir-entries set-dir-entries!))

(define-record-type <file>
  (make-file name size)
  file?
  (name file-name)
  (size file-size))

(define (make-dir name)
  (%make-dir name 0 '()))

(define (add-dir-size! dir extra)
  (set-dir-size! dir (+ extra (dir-size dir))))

(define (add-dir-entry! dir file)
  (set-dir-entries! dir (cons file (dir-entries dir))))

(define (dir-or-file-name entry)
  ((if (dir? entry) dir-name file-name) entry))

(define (sort-dir-entries! dir)
  (set-dir-entries! dir (sort! (dir-entries dir)
                               (lambda (a b)
                                 (string<? (dir-or-file-name a)
                                           (dir-or-file-name b))))))

(define* (print-file file #:optional (indent ""))
  (format #t "~a- ~a (~a)\n" indent (file-name file) (file-size file)))

(define* (print-dir dir #:optional (indent ""))
  (format #t "~a+ ~a (~a)\n" indent (dir-name dir) (dir-size dir))
  (for-each (lambda (entry)
              ((if (dir? entry) print-dir print-file)
               entry (string-append indent "  ")))
            (dir-entries dir)))

;; Command interpreter

(define (build-filesystem-tree cmds)
  (let ((root (make-dir "/")))
    (let loop ((cmds cmds) (root root) (pwd '()))
      (match cmds
        (()
         (match pwd
           ((root)
            root)
           ((dir . _)
            ;; pretend we got "cd .." up to root
            (loop '((command-cd "..")) root pwd))))
        (('command-ls . rest)
         (loop rest root pwd))
        ((('command-cd "/") . rest)
         (loop rest root (cons root pwd)))
        ((('command-cd "..") . rest)
         (let ((dir (car pwd))
               (parent (cadr pwd)))
           ;; add sub-directory size to parent
           (add-dir-size! parent (dir-size dir))
           (sort-dir-entries! dir)
           (loop rest root (cdr pwd))))
        ((('command-cd name) . rest)
         (let ((parent (car pwd))
               (dir (make-dir name)))
           (add-dir-entry! parent dir)
           (loop rest root (cons dir pwd))))
        ((('output-dir name) . rest)
         (loop rest root pwd))
        ((('output-file ('filesize size) ('filename name)) . rest)
         (let ((parent (car pwd))
               (file (make-file name (string->number size))))
           ;; add file size to current directory
           (add-dir-entry! parent file)
           (add-dir-size! parent (file-size file))
           (loop rest root pwd)))))))

(define (filesystem-tree-fold proc init dir)
  (let loop ((items (list dir)) (result init))
    (match items
      (() result)
      ((item . rest)
       (loop (if (dir? item)
                 (append (dir-entries item) rest)
                 rest)
             (proc item result))))))

;;; Solutions

(define (find-dirs-under size tree)
  (filesystem-tree-fold
   (lambda (item result)
     (if (and (dir? item) (<= (dir-size item) size))
         (cons item result)
         result))
   '() tree))

(define (puzzle-0)
  (let* ((commands (parse-terminal-output %example))
         (tree (build-filesystem-tree commands))
         (dirs (find-dirs-under 100000 tree)))
    (apply + (map dir-size dirs))))

(define (puzzle-1)
  (let* ((input (call-with-input-file "data/input-07" get-string-all))
         (commands (parse-terminal-output input))
         (tree (build-filesystem-tree commands))
         (dirs (find-dirs-under 100000 tree)))
    (apply + (map dir-size dirs))))

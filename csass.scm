;;; csass.scm -- A command-line SASS compiler in Chicken Scheme
;;;
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(use ports)
(import extras)
(use (prefix sass sass:))
(use args)


(define opts
  `(,(args:make-option (s stdin) #:none
                       "Read input from stdin.")
    ,(args:make-option (o output-file) (required: "FILENAME")
                       "Write the output to FILENAME.")
    ,(args:make-option (I load-path) (required: "PATH")
                       "Set the import path to PATH.")
    ,(args:make-option (t style) (required: "STYLENAME")
                       "Set the output style to STYLENAME.
                        Can be: nested, expanded, compact, compressed.")
    ,(args:make-option (l line-numbers line-comments) #:none
                       "Emit comments showing line numbers in input file.")
    ,(args:make-option (m sourcemap) #:none
                       "Emit source map.")
    ,(args:make-option (M omit-map-comment) #:none
                       "Omit the source map url comment.")
    ,(args:make-option (p precision) (required: "DIGITS")
                       "Set the numerical precision to DIGITS.")
    ,(args:make-option (v version) #:none
                       "Display compiled versions.")
    ,(args:make-option (h help) #:none
                       "Display this help message.")))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (car (argv)) " [options...] [input-file]")
      (newline)
      (print (args:usage opts)))))

(define (version)
  (printf "libsass version: ~A\n" (sass:libsass-version))
  (printf "chicken-sass version: ~A\n" (sass:%version%)))

(define (have-option? key options)
  (let loop ((opts options))
    (cond
      ((null? opts) #f)
      ((eqv? (caar opts) key) #t)
      (else (loop (cdr opts))))))

(define (get-arg key options)
  (let loop ((opts options))
    (cond
      ((null? opts) 'undefined)
      ((eqv? (caar opts) key) (cdar opts))
      (else (loop (cdr opts))))))

(define (defined? var)
  (not (eqv? var 'undefined))) 

(define (run)
  (receive (options operands)
    (args:parse (command-line-arguments) opts)
    (cond
      ((have-option? 'h options) (usage) (exit 0))
      ((have-option? 'v options) (version) (exit 0))
      (else
        (let* ((output-style*
                 (alist-ref 't options))
               (output-style
                 (and output-style* (string->symbol output-style*)))
               (precision*
                 (alist-ref 'p options))
               (precision
                 (and precision* (string->number precision*)))
               (include-paths
                 (alist-ref 'I options))
               (source-comments
                 (get-arg 'l options))
               (omit-source-map-url
                 (get-arg 'M options))
               (infile
                 (and (= (length operands) 1) (car operands)))
               (outfile
                 (alist-ref 'o options))
               (output
                 (or outfile 'stdout))
               (from-stdin
                 (have-option? 's options))
               (emit-source-map
                 (have-option? 'm options))
               (source-map-file
                 (and outfile
                      emit-source-map
                      (string-append outfile ".map"))))
          (unless (or (not output-style)
                      (member output-style '(nested expanded compact compressed)))
            (error "Output style must be one of: nested, expanded, compact, or compressed."))

          (let ((kwargs
                  `(output: ,output
                    output-style: ,output-style
                    precision: ,precision
                    output-path: ,outfile
                    input-path: ,infile
                    include-path: ,include-paths
                    source-comments: ,source-comments
                    omit-source-map-url: ,omit-source-map-url
                    source-map-file: ,source-map-file)))
            (cond
              ((and from-stdin (not infile))
               (apply sass:compile-from-port `(,(current-input-port) ,@kwargs)))
              ((and infile (not from-stdin))
               (apply sass:compile-file `(,infile ,@kwargs)))
              (else
                (error "Please specify an input file OR the -s option (and not both).")))))))))


(run)

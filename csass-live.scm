;;; csass-live.scm -- A library for compiling SASS stylesheets on the fly.
;;;
;;;   Copyright © 2015 by Matthew C. Gushee <matt@gushee.net>
;;;   This program is open-source software, released under the
;;;   BSD license. See the accompanying LICENSE file for details.

(module csass-live
        *
        (import scheme chicken)
        (use (prefix intarweb iw:))
        (use (prefix simple-sha1 ss:))
        (use (prefix sql-de-lite sq:))
        (use csass-utils-common)

(define *site-path* (make-parameter #f))

; This path is optional; if provided, it must be an absolute path
(define (site-path path)
  (assert (absolute-pathname? path))
  (*site-path* path))

; Source path is where the SCSS stylesheets are kept. If site-path is
;   set, this may be a relative path, and its location will be calculated
;   relative to site-path.
(define *source-path* (make-parameter #f))

(define *css-path* (make-parameter #f))

(define *db-path* (make-parameter #f))

(define (get-path which)
  (let ((raw-path
          (case which
            ((source) (*source-path*))
            ((css) (*css-path*))
            ((db) (*db-path*)))))
    (if (absolute-pathname? raw-path)
      raw-path
      (let ((site-path (*site-path*)))
        (when (not site-path)
          (error "site-path is unset"))
        (make-pathname site-path raw-path)))))

(define (set-path! which path)
  (case which
    ((source) (*source-path* path))
    ((css) (*css-path* path))
    ((db) (*db-path* path))))

(define (source-path #!optional (path #f))
  (if path
    (set-path! 'source path)
    (get-path 'source)))

(define (css-path #!optional (path #f))
  (if path
    (set-path! 'css path)
    (get-path 'css)))

(define (db-path #!optional (path #f))
  (if path
    (set-path! 'db path)
    (get-path 'db)))

(define setup-etag-table-query
  "CREATE TABLE etags (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    path TEXT NOT NULL,
    etag TEXT NOT NULL );")

(define (setup-etag-db path #!optional (force #f))
  (when (file-exists? path)
    (if force
      (delete-file path)
      (eprintf "Database '~A' already exists." path)))
  (let* ((conn (sq:open-database path))
         (st (sq:sql/transient conn)))
    (sq:exec st)))

) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------


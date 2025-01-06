;;; test-tramp-gh.el --- Tests for tramp-gh  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Jaehyun Yeom

;; Author: Jaehyun Yeom <jae.yeom@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'tramp-gh)

(ert-deftest tramp-gh-api--run-process ()
  "Check if `tramp-gh-api' runs the process."
  (let ((call-process-args nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest args)
                 (setq call-process-args args)
                 ;; Simulate successful process execution
                 0)))
      ;; Call the function you're testing
      (tramp-gh-api "my/url")

      ;; Assert on captured arguments
      (should (equal call-process-args
                     ;; Replace with the expected arguments
                     '("gh" nil (t nil) nil "api" "my/url"))))))

(ert-deftest tramp-gh-split-filename ()
  "Test `tramp-gh-split-filename' function."
  (should (equal (tramp-gh-split-filename "/gh:emacsmirror@tramp:/path/to/file")
                 (list "emacsmirror" "tramp" "/path/to/file")))

  (should (equal (tramp-gh-split-filename "/gh:tramp:/path/to/file")
                 (list nil "tramp" "/path/to/file"))))

(defvar test-fs
  `(("repos/my-username/my-repo/contents/" .
     [(:name ".dir-locals.el" :type "file" :size 3)
      (:name "README.md" :type "file" :size 13)
      (:name "lisp" :type "dir" :size 0)])
    ("repos/my-username/my-repo/contents/.dir-locals.el" .
     (:name ".dir-locals.el" :size 3 :content ,(base64-encode-string "nil")))
    ("repos/my-username/my-repo/contents/README.md" .
     (:name "README.md" :type "file" :size 13 :content ,(base64-encode-string "Hello, world!")))
    ("repos/my-username/my-repo/contents/lisp/" .
     [(:name "tramp-gh.el" :type "file" :size 19)
      (:name "tramp-gh-test.el" :type "file" :size 24)])
    ("repos/my-username/my-repo/contents/lisp/tramp-gh.el" .
     (:name "tramp-gh.el" :type "file" :size 19 :content ,(base64-encode-string "(provide 'tramp-gh)")))
    ("repos/my-username/my-repo/contents/lisp/tramp-gh-test.el" .
     (:name "tramp-gh-test.el" :type "file" :size 24 :content ,(base64-encode-string "(provide 'tramp-gh-test)"))))
  "File content for the tests.")

(defun make-tramp-gh-api (fs-alist)
  "Create `tramp-gh-api' function based on the given `fs-alist'
where it's keyed by the API URL and its return value."
  (let ((fs-hash(make-hash-table :test 'equal)))
    (dolist (fs fs-alist)
      (puthash (car fs)
               (json-serialize (cdr fs))
               fs-hash))
    (lambda (url)
      (message "make-tramp-gh-api url: %s" url)
      (gethash url fs-hash))))

(ert-deftest tramp-gh-api-contents ()
  "Test `tramp-gh-api-contents' function."
  (cl-letf (((symbol-function 'tramp-gh-api) (make-tramp-gh-api test-fs)))
    (let ((actual (tramp-gh-api-contents "/gh:my-username@my-repo:/")))
      (should (equal (gethash "name" (aref actual 0)) ".dir-locals.el"))
      (should (equal (gethash "name" (aref actual 1)) "README.md"))
      (should (equal (gethash "name" (aref actual 2)) "lisp")))
    (let ((actual (tramp-gh-api-contents "/gh:my-username@my-repo:/README.md")))
      (should (equal (gethash "name" actual) "README.md"))
      (should (equal (gethash "content" actual) (base64-encode-string "Hello, world!"))))
    ;; NOTE(jaeyeom): Currently test for `tramp-gh-handle-insert-file-contents'
    ;; fails when the following test is disabled.
    (let ((actual (tramp-gh-api-contents "/gh:my-username@my-repo:/lisp/tramp-gh.el")))
      (should (equal (gethash "name" actual) "tramp-gh.el"))
      (should (equal (gethash "content" actual) (base64-encode-string "(provide 'tramp-gh)"))))
    )
  )

(ert-deftest tramp-gh-handle-directory-files-and-attributes ()
  "Test `tramp-gh-handle-directory-files-and-attributes' function."
  (cl-letf (((symbol-function 'tramp-gh-api) (make-tramp-gh-api test-fs)))
    (let ((actual (tramp-gh-handle-directory-files-and-attributes "/gh:my-username@my-repo:/")))
      (should (equal actual '((".dir-locals.el" nil 0 1000 1000
                               (0 0 0 0)
                               (0 0 0 0)
                               (0 0 0 0)
                               3 "-r--r--r--"
                               t 0 0)
                              ("README.md" nil 0 1000 1000
                               (0 0 0 0)
                               (0 0 0 0)
                               (0 0 0 0)
                               13 "-r--r--r--"
                               t 0 0)
                              ("lisp" t 0 1000 1000
                               (0 0 0 0)
                               (0 0 0 0)
                               (0 0 0 0)
                               0 "dr-xr-xr-x"
                               t 0 0)
                              ))))
    (let ((actual (tramp-gh-handle-directory-files-and-attributes "/gh:my-username@my-repo:/lisp/")))
      (should (equal actual '(("tramp-gh-test.el" nil 0 1000 1000
                               (0 0 0 0)
                               (0 0 0 0)
                               (0 0 0 0)
                               24 "-r--r--r--"
                               t 0 0)
                              ("tramp-gh.el" nil 0 1000 1000
                               (0 0 0 0)
                               (0 0 0 0)
                               (0 0 0 0)
                               19 "-r--r--r--"
                               t 0 0)))))
    (should-error (tramp-gh-handle-directory-files-and-attributes "/gh:my-username@my-repo:/lisp/tramp-gh.el"))))

(ert-deftest tramp-gh-handle-directory-files ()
  "Test `tramp-gh-handle-directory-files' function."
  (cl-letf (((symbol-function 'tramp-gh-api) (make-tramp-gh-api test-fs)))
    (let ((actual (tramp-gh-handle-directory-files "/gh:my-username@my-repo:/")))
      (should (equal actual '(".dir-locals.el" "README.md" "lisp"))))
    (let ((actual (tramp-gh-handle-directory-files "/gh:my-username@my-repo:/lisp/")))
      (should (equal actual '("tramp-gh-test.el" "tramp-gh.el")))))

  (should-error (tramp-gh-handle-directory-files "/gh:my-username@my-repo:/lisp/tramp-gh.el")))

(ert-deftest tramp-gh-handle-file-attributes ()
  "Test `tramp-gh-handle-file-attributes' function."
  (cl-letf (((symbol-function 'tramp-gh-api) (make-tramp-gh-api test-fs)))
    (let ((actual (tramp-gh-handle-file-attributes "/gh:my-username@my-repo:/")))
      (should (equal actual '(t 0 1000 1000
                                (0 0 0 0)
                                (0 0 0 0)
                                (0 0 0 0)
                                0 "dr-xr-xr-x"
                                t 0 0))))
    (let ((actual (tramp-gh-handle-file-attributes "/gh:my-username@my-repo:/README.md")))
      (should (equal actual '(nil 0 1000 1000
                                  (0 0 0 0)
                                  (0 0 0 0)
                                  (0 0 0 0)
                                  13 "-r--r--r--"
                                  t 0 0))))))

(ert-deftest tramp-gh-handle-insert-file-contents ()
  "Test `tramp-gh-handle-insert-file-contents' function."
  (cl-letf (((symbol-function 'tramp-gh-api) (make-tramp-gh-api test-fs)))
    (with-temp-buffer (tramp-gh-handle-insert-file-contents "/gh:my-username@my-repo:/README.md")
                      (should (equal (buffer-string) "Hello, world!")))
    ;; FIXME(jaeyeom): It seems that the following is not working properly
    ;; before calling `tramp-gh-api-contents' function.
    (with-temp-buffer (tramp-gh-handle-insert-file-contents "/gh:my-username@my-repo:/lisp/tramp-gh.el")
                      (should (equal (buffer-string) "(provide 'tramp-gh)")))
    ))

(ert-deftest tramp-gh-handle-file-local-copy ()
  "Test `tramp-gh-handle-file-local-copy' function."
  (cl-letf (((symbol-function 'tramp-gh-api) (make-tramp-gh-api test-fs)))
    (let ((actual (tramp-gh-handle-file-local-copy "/gh:my-username@my-repo:/README.md")))
      (should (equal (with-temp-buffer (insert-file-contents actual) (buffer-string)) "Hello, world!")))
    (let ((actual (tramp-gh-handle-file-local-copy "/gh:my-username@my-repo:/lisp/tramp-gh.el")))
      (should (equal (with-temp-buffer (insert-file-contents actual) (buffer-string)) "(provide 'tramp-gh)")))
    (should-error (tramp-gh-handle-file-local-copy "/gh:my-username@my-repo:/lisp/"))))

(defmacro with-temp-directory (var &rest body)
  `(let ((,var (make-temp-file "" 'dir)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,var t))))

(ert-deftest tramp-gh-handle-copy-file ()
  "Test `tramp-gh-handle-copy-file' function."
  (cl-letf (((symbol-function 'tramp-gh-api) (make-tramp-gh-api test-fs)))
    (with-temp-directory
     temp-dir
     (let ((filepath (expand-file-name "README.md" temp-dir)))
       (tramp-gh-handle-copy-file "/gh:my-username@my-repo:/README.md" filepath)
       (should (equal (with-temp-buffer (insert-file-contents filepath) (buffer-string)) "Hello, world!")))
     (let ((filepath (expand-file-name "tramp-gh.el" temp-dir)))
       (tramp-gh-handle-copy-file "/gh:my-username@my-repo:/lisp/tramp-gh.el" filepath)
       (should (equal (with-temp-buffer (insert-file-contents filepath) (buffer-string)) "(provide 'tramp-gh)"))))))

(ert-deftest tramp-gh-handle-file-name-all-completions ()
  "Test `tramp-gh-handle-file-name-all-completions' function."
  (cl-letf (((symbol-function 'tramp-gh-api) (make-tramp-gh-api test-fs)))
    (let ((actual (tramp-gh-handle-file-name-all-completions "" "/gh:my-username@my-repo:/")))
      (should (equal actual '(".dir-locals.el" "README.md" "lisp"))))
    (let ((actual (tramp-gh-handle-file-name-all-completions "." "/gh:my-username@my-repo:/")))
      (should (equal actual '(".dir-locals.el"))))
    (let ((actual (tramp-gh-handle-file-name-all-completions "li" "/gh:my-username@my-repo:/")))
      (should (equal actual '("lisp"))))
    (let ((actual (tramp-gh-handle-file-name-all-completions "RA" "/gh:my-username@my-repo:/")))
      (should (equal actual nil)))
    (let ((actual (tramp-gh-handle-file-name-all-completions "" "/gh:my-username@my-repo:/lisp/")))
      (should (equal actual '("tramp-gh-test.el" "tramp-gh.el"))))
    (let ((actual (tramp-gh-handle-file-name-all-completions "tramp-gh" "/gh:my-username@my-repo:/lisp/")))
      (should (equal actual '("tramp-gh-test.el" "tramp-gh.el"))))))

(ert-deftest tramp-gh-file-name-p()
  "Test `tramp-gh-file-name-p' function."
  (should (tramp-gh-file-name-p "/gh:my-username@my-repo:/"))
  (should (tramp-gh-file-name-p "/gh:my-username@my-repo:/README.md"))
  (should (tramp-gh-file-name-p "/gh:my-username@my-repo:/lisp/"))
  (should (tramp-gh-file-name-p "/gh:my-username@my-repo:/lisp/tramp-gh.el"))
  (should (not (tramp-gh-file-name-p "/ssh:my-username@my-repo:/lisp/tramp-gh.el/")))
  (should (not (tramp-gh-file-name-p "/lisp/tramp-gh.el/"))))

(provide 'test-tramp-gh)
;;; test-tramp-gh.el ends here

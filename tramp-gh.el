;;; tramp-gh.el --- TRAMP for read-only access to files on github  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Jaehyun Yeom

;; Author: Jaehyun Yeom <jae.yeom@gmail.com>
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience
;; URL: https://github.com/jaeyeom/tramp-gh
;; Version: 0.0.1

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package enables read-only navigate to Github repositories using TRAMP.
;;
;; Requirements:
;; - gh
;;
;; Usage:
;; Access files on github by "/gh:owner@repo:/path/to/file".

;;; Code:

(require 'tramp)

(defun tramp-gh-api (url)
  "Run the gh api command.

URL is the path in the API endpoint, e.g.,
\"repos/jaeyeom/tramp-gh/contents/README.org\"."
  ;; TODO(jaeyeom): Make this configurable, so either command line command or
  ;; REST API can be used.
  (with-temp-buffer
    ;; FIXME(jaeyeom): Setting `default-directory' to home directory, so Emacs
    ;; does not try to run "gh" on the remote server, which isn't possible at
    ;; all.
    (let ((default-directory "~/"))
      (call-process "gh" nil '(t nil) nil "api" url))
    (buffer-string)))

(defun tramp-gh-split-filename (filename)
  "Split the FILENAME into owner, repo, and path."
  (let* ((vec (tramp-dissect-file-name filename))
         (owner (tramp-file-name-user vec))
         (repo (tramp-file-name-host vec))
         (path (tramp-file-name-localname vec)))
    (list owner repo path)))

(defvar tramp-gh-api-contents-cache (make-hash-table :test 'equal))
(defvar tramp-gh-api-contents-cache-ttl 10)

(defun tramp-gh-api-contents (filename)
  "Get the contents of the FILENAME on GitHub.

The content is in vector of hash table if the file is a
directory, and a hash table if the file is a file."
  (let ((vec (tramp-dissect-file-name filename)))
    (tramp-debug-message vec "Called with args: %s" filename))
  (let* ((parts (tramp-gh-split-filename filename))
         (owner (car parts))
         (repo (cadr parts))
         (path (caddr parts))
         (url (format "repos/%s/%s/contents%s" owner repo path))
         (cached (gethash url tramp-gh-api-contents-cache)))
    (cdr
     (if (and cached (time-less-p (current-time) (car cached)))
         cached
       (let ((vec (tramp-dissect-file-name filename)))
         (tramp-debug-message vec "Actually calling tramp-gh-api: %s" url))
       (puthash url
                (cons (time-add (current-time) (seconds-to-time tramp-gh-api-contents-cache-ttl))
                      (json-parse-string (tramp-gh-api url)))
                tramp-gh-api-contents-cache)))))

(defun tramp-gh-handle-directory-files-and-attributes (directory &optional full match nosort id-format count)
  "Get the list of files and attributes in the DIRECTORY on GitHub.

Please refer to `directory-files-and-attributes' for the meaning
of FULL, MATCH, NOSORT, ID-FORMAT, and COUNT."
  (let ((vec (tramp-dissect-file-name directory)))
    (tramp-debug-message vec "Called with args: %s %s %s %s %s %s" directory full match nosort id-format count))
  (let ((contents (tramp-gh-api-contents directory))
        (id (if (eq id-format 'string) "git" 1000))
        (ts '(0 0 0 0)))
    (unless (vectorp contents)
      (error "Directory is not a directory: %s" directory))
    (let* ((all-files (mapcar
                       (lambda (x)
                         (cons (gethash "name" x)
                               (if (string-equal (gethash "type" x) "dir")
                                   (list t 0 id id ts ts ts 0 "dr-xr-xr-x" t 0 0)
                                 (let ((size (gethash "size" x)))
                                   (list nil 0 id id ts ts ts size "-r--r--r--" t 0 0)))))
                       contents))
           (files (if match
                      (seq-filter (lambda (f) (string-match-p match (car f))) all-files)
                    all-files))
           (paths (if full
                      (mapcar
                       (lambda (f)
                         (cons
                          (concat (file-name-as-directory directory) (car f))
                          (cdr f)))
                       files)
                    files))
           (sorted-paths (if nosort
                             paths
                           (sort paths (lambda (s1 s2) (string-lessp (car s1) (car s2)))))))
      (if count
          (seq-take sorted-paths count)
        sorted-paths))))

(defun tramp-gh-handle-directory-files (directory &optional full match nosort count)
  "Get the list of files in the DIRECTORY on GitHub.

Please refer to `directory-files' for the meaning of FULL, MATCH,
NOSORT, and COUNT."
  (let ((vec (tramp-dissect-file-name directory)))
    (tramp-debug-message vec "Called with args: %s %s %s %s %s" directory full match nosort count))
  (mapcar #'car
          (tramp-gh-handle-directory-files-and-attributes directory full match nosort nil count)))

(defun tramp-gh-handle-file-attributes (filename &optional id-format)
  "Get the attributes of the FILENAME on GitHub.

Please refer to `file-attributes' for the meaning of ID-FORMAT."
  (let ((vec (tramp-dissect-file-name filename)))
    (tramp-debug-message vec "Called with args: %s %s" filename id-format))
  (let ((id (if (eq id-format 'string) "git" 1000))
        (ts '(0 0 0 0))
        (parent (file-name-parent-directory filename)))
    (if (not parent)
        (list t 0 id id ts ts ts 0 "dr-xr-xr-x" t 0 0)
      (cdr (assoc-string
            (file-name-nondirectory (directory-file-name filename))
            (tramp-gh-handle-directory-files-and-attributes parent nil nil nil id-format))))))

(defun tramp-gh-handle-insert-file-contents (filename &optional visit beg end replace)
  "Insert the FILENAME contents.

Please refer to `insert-file-contents' for the meaning of VISIT,
BEG, END, and REPLACE."
  (let ((vec (tramp-dissect-file-name filename)))
    (tramp-debug-message vec "Called with args: %s %s %s %s %s" filename visit beg end replace))
  (let ((contents (tramp-gh-api-contents filename)))
    (when (vectorp contents)
      (error "Filename is a directory: %s" filename))
    (let ((data (substring (base64-decode-string (gethash "content" contents)) beg end))
          (start (point))
          (inserted 0))
      ;; FIXME(jaeyeom): Where the original handler works without moving the
      ;; mark, this handles moves the mark to the beginning of the buffer. And
      ;; I don't know what I'm doing now. Figure out and make it work
      ;; properly.
      (save-excursion
        (when replace
          (setq inserted (- inserted (- start (point-min))))
          (delete-region (point-min) start)
          (setq inserted (- inserted (- (point-max) (point))))
          (delete-region (point) (point-max)))
        (setq inserted (length data))
        (insert data)
        (decode-coding-inserted-region (point-min) (point) filename
                                       visit beg end replace))
      (when visit
        (setq buffer-file-name filename
              buffer-read-only t)
        (set-visited-file-modtime)
        (set-buffer-modified-p nil))
      (list filename (max 0 inserted)))))

(defun tramp-gh-handle-file-local-copy (filename)
  "Get the file path of the local copy of the FILENAME on GitHub."
  (let ((vec (tramp-dissect-file-name filename)))
    (tramp-debug-message vec "Called with args: %s" filename))
  (let ((temp-file (make-temp-file "gh-")))
    (with-temp-file temp-file
      (tramp-gh-handle-insert-file-contents filename))
    temp-file))

;; TODO(jaeyeom): Implement this without using the local file.
(defun tramp-gh-handle-copy-file (from newname &optional ok-if-already-exists keep-time preserve-uid-gid preserve-permissions)
  "Copy the FROM from GitHub to NEWNAME.

Please refer to `copy-file' for the meaning of
OK-IF-ALREADY-EXISTS, KEEP-TIME, PRESERVE-UID-GID, and
PRESERVE-PERMISSIONS."
  (let ((vec (tramp-dissect-file-name from)))
    (tramp-debug-message vec "Called with args: %s %s %s %s %s %s"
                         from newname ok-if-already-exists keep-time preserve-uid-gid preserve-permissions))
  (let ((local-file (tramp-gh-handle-file-local-copy from)))
    (copy-file local-file newname ok-if-already-exists keep-time preserve-uid-gid preserve-permissions)))

(defun tramp-gh-handle-file-name-all-completions (file directory)
  "Get the list of all completions of the FILE name on GitHub in the DIRECTORY."
  (let ((vec (tramp-dissect-file-name directory)))
    (tramp-debug-message vec "Called with args: %s %s" file directory))
  (let ((files (tramp-gh-handle-directory-files directory)))
    (seq-filter (lambda (f) (string-prefix-p file f)) files)))

(defsubst tramp-gh-file-name-p (vec-or-filename)
  "Check if it's a VEC-OR-FILENAME for gh."
  (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename)))
    (string= (tramp-file-name-method vec) "gh")))

(defconst tramp-gh-file-name-handler-alist
  '(;; `abbreviate-file-name' performed by default handler.
    (access-file . tramp-handle-access-file)
    (add-name-to-file . tramp-handle-add-name-to-file)
    ;; `byte-compiler-base-file-name' performed by default handler.
    (copy-directory . tramp-handle-copy-directory)
    (copy-file . tramp-gh-handle-copy-file)
    (delete-directory . ignore)
    (delete-file . ignore)
    ;; `diff-latest-backup-file' performed by default handler.
    (directory-file-name . tramp-handle-directory-file-name)
    (directory-files . tramp-gh-handle-directory-files)
    (directory-files-and-attributes . tramp-gh-handle-directory-files-and-attributes)
    (dired-compress-file . ignore)
    (dired-uncache . tramp-handle-dired-uncache)
    (exec-path . ignore)
    (expand-file-name . tramp-handle-expand-file-name)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-acl . ignore)
    (file-attributes . tramp-gh-handle-file-attributes)
    (file-directory-p . tramp-handle-file-directory-p)
    (file-equal-p . tramp-handle-file-equal-p)
    (file-executable-p . always)
    (file-exists-p . tramp-handle-file-exists-p)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-local-copy . tramp-gh-handle-file-local-copy)
    (file-locked-p . tramp-handle-file-locked-p)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions . tramp-gh-handle-file-name-all-completions)
    (file-name-as-directory . tramp-handle-file-name-as-directory)
    (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p)
    (file-name-completion . tramp-handle-file-name-completion)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    ;; `file-name-sans-versions' performed by default handler.
    (file-newer-than-file-p . tramp-handle-file-newer-than-file-p)
    (file-notify-add-watch . tramp-handle-file-notify-add-watch)
    (file-notify-rm-watch . tramp-handle-file-notify-rm-watch)
    (file-notify-valid-p . tramp-handle-file-notify-valid-p)
    (file-ownership-preserved-p . ignore)
    (file-readable-p . tramp-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-selinux-context . tramp-handle-file-selinux-context)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-system-info . ignore)
    (file-truename . tramp-handle-file-truename)
    (file-writable-p . ignore)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . tramp-handle-insert-directory)
    (insert-file-contents . tramp-gh-handle-insert-file-contents)
    (list-system-processes . tramp-handle-list-system-processes)
    (load . tramp-handle-load)
    (lock-file . ignore)
    (make-auto-save-file-name . ignore)
    (make-directory . ignore)
    (make-directory-internal . ignore)
    (make-lock-file-name . tramp-handle-make-lock-file-name)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-process . ignore)
    (make-symbolic-link . tramp-handle-make-symbolic-link)
    (memory-info . tramp-handle-memory-info)
    (process-attributes . tramp-handle-process-attributes)
    (process-file . ignore)
    (rename-file . ignore)
    (set-file-acl . ignore)
    (set-file-modes . ignore)
    (set-file-selinux-context . ignore)
    (set-file-times . ignore)
    (set-visited-file-modtime . ignore)
    (shell-command . tramp-handle-shell-command)
    (start-file-process . tramp-handle-start-file-process)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (temporary-file-directory . tramp-handle-temporary-file-directory)
    (tramp-get-home-directory . ignore)
    (tramp-get-remote-gid . ignore)
    (tramp-get-remote-groups . ignore)
    (tramp-get-remote-uid . ignore)
    (tramp-set-file-uid-gid . ignore)
    (unhandled-file-name-directory . ignore)
    (unlock-file . tramp-handle-unlock-file)
    (vc-registered . ignore)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (write-region . ignore))
  "Alist of handler functions for Tramp gh method.")

(defun tramp-gh-file-name-handler (operation &rest args)
  "Handle gh TRAMP methods.

OPERATION is the regular file operation and ARGS is arguments to
it."
  (if-let ((fn (assoc operation tramp-gh-file-name-handler-alist)))
      (apply (cdr fn) args)
    (tramp-run-real-handler operation args)))

(add-to-list 'tramp-methods '("gh"))

(tramp-register-foreign-file-name-handler
 #'tramp-gh-file-name-p #'tramp-gh-file-name-handler)

(provide 'tramp-gh)
;;; tramp-gh.el ends here

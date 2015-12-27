;;; helm-codesearch.el --- helm interface for codesearch

;; Copyright (C) 2015 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((helm "1.7.7"))

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

;;; Commentary:

;; helm interface for codesearch
;;
;; See documentation on https://github.com/youngker/helm-codesearch.el

;;; Code:

(require 'helm)
(require 'helm-grep)

(defgroup helm-codesearch nil
  "Helm interface for codesearch."
  :group 'helm)

(defcustom helm-codesearch-csearchindex
  ".csearchindex"
  "Index file for each projects."
  :group 'helm-codesearch)

(defvar helm-codesearch-buffer "*helm codesearch*")
(defvar helm-codesearch-indexing-buffer "*helm codesearch indexing*")

(defun helm-codesearch-search-csearchindex ()
  "Search for Project index file."
  (let* ((dir (expand-file-name default-directory)))
    (catch 'done
      (while dir
        (when (file-exists-p (concat dir helm-codesearch-csearchindex))
          (setenv "CSEARCHINDEX" (expand-file-name
                                  (concat dir helm-codesearch-csearchindex)))
          (throw 'done (concat dir helm-codesearch-csearchindex)))
        (setq dir (file-name-as-directory
                   (file-name-directory
                    (directory-file-name dir))))
        (when (string-match "^\\(/\\|[A-Za-z]:[\\/]\\)$" dir)
          (error "Can't find a csearchindex"))))))

(defun helm-codesearch-find-pattern-transformer (candidates source)
  "Transformer is run on the CANDIDATES and not use SOURCE."
  (cl-loop for c in (helm-fast-remove-dups candidates :test 'equal)
           collect (helm-grep--filter-candidate-1 c)))

(defun helm-codesearch-find-pattern-process ()
  "Execute the csearch for a pattern."
  (let ((proc (apply 'start-process "codesearch" nil
                     "csearch" (list "-n" helm-pattern))))
    (prog1 proc
      (set-process-sentinel
       proc
       (lambda (process event)
         (helm-process-deferred-sentinel-hook
          process event (helm-default-directory)))))))

(defun helm-codesearch-find-file-process ()
  "Execute the csearch for a files."
  (let ((proc (apply 'start-process "codesearch" nil
                     "csearch" (list "-l" "-f" helm-pattern "$"))))
    (prog1 proc
      (set-process-sentinel
       proc
       (lambda (process event)
         (helm-process-deferred-sentinel-hook
          process event (helm-default-directory)))))))

(defun helm-codesearch-create-csearchindex-process (dir)
  "Execute the cindex from a DIR."
  (let ((proc (apply 'start-process "codesearch"
                     helm-codesearch-indexing-buffer
                     "cindex" (list dir))))
    (with-current-buffer helm-codesearch-indexing-buffer
      (pop-to-buffer helm-codesearch-indexing-buffer)
      (goto-char (point-max)))))

(defclass helm-codesearch-source-pattern (helm-source-async)
  ((header-name
    :initform
    (lambda (name)
      (concat name " [" (helm-attr 'cindex-dir) "]")))
   (cindex-dir :initarg :cindex-dir
               :initform nil
               :custom string
               :documentation " Index file.")
   (candidates-process :initform 'helm-codesearch-find-pattern-process)
   (filtered-candidate-transformer
    :initform 'helm-codesearch-find-pattern-transformer)
   (candidate-number-limit :initform 100)
   (action :initform (helm-make-actions
                      "Find File" 'helm-grep-action
                      "Find file other frame" 'helm-grep-other-frame
                      (lambda () (and (locate-library "elscreen")
                                  "Find file in Elscreen"))
                      'helm-grep-jump-elscreen
                      "Save results in grep buffer" 'helm-grep-save-results
                      "Find file other window" 'helm-grep-other-window))
   (persistent-action :initform 'helm-grep-persistent-action)
   (history :initform 'helm-grep-history)
   (help-message :initform 'helm-grep-help-message)
   (requires-pattern :initform 4)))

(defclass helm-codesearch-source-file (helm-source-async)
  ((header-name
    :initform
    (lambda (name)
      (concat name " [" (helm-attr 'cindex-dir) "]")))
   (cindex-dir :initarg :cindex-dir
               :initform nil
               :custom string
               :documentation " Index file.")
   (candidates-process :initform 'helm-codesearch-find-file-process)
   (action :initform 'helm-type-file-actions)
   (filtered-candidate-transformer
    :initform 'helm-file-name-history-transformer)
   (candidate-number-limit :initform 100)
   (requires-pattern :initform 4)))

;;;###autoload
(defun helm-codesearch-find-pattern (query)
  "Find pattern with QUERY."
  (interactive
   (list (read-string "Find pattern: " (current-word))))
  (when (helm-codesearch-search-csearchindex)
    (helm :sources (helm-make-source
                       "Codesearch: Find Pattern at"
                       'helm-codesearch-source-pattern
                     :cindex-dir (getenv "CSEARCHINDEX"))
          :buffer helm-codesearch-buffer
          :input query
          :keymap helm-grep-map
          :truncate-lines t)))

;;;###autoload
(defun helm-codesearch-find-file (query)
  "Find files with QUERY."
  (interactive
   (list (read-string "Find file: " (current-word))))
  (when (helm-codesearch-search-csearchindex)
    (helm :sources (helm-make-source
                       "Codesearch: Find File at"
                       'helm-codesearch-source-file
                     :cindex-dir (getenv "CSEARCHINDEX"))
          :buffer helm-codesearch-buffer
          :input query
          :keymap helm-grep-map
          :truncate-lines t)))

;;;###autoload
(defun helm-codesearch-create-csearchindex (dir)
  "Create index file at DIR."
  (interactive "DIndex files in directory: ")
  (setenv "CSEARCHINDEX" (expand-file-name
                          (concat dir helm-codesearch-csearchindex)))
  (helm-codesearch-create-csearchindex-process (expand-file-name dir)))

(provide 'helm-codesearch)
;;; helm-codesearch.el ends here

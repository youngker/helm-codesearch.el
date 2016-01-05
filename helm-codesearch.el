;;; helm-codesearch.el --- helm interface for codesearch

;; Copyright (C) 2016 Youngjoo Lee

;; Author: Youngjoo Lee <youngker@gmail.com>
;; Version: 0.2.0
;; Keywords: tools
;; Package-Requires: ((s "1.10.0") (dash "2.12.0") (helm "1.7.7") (cl-lib "0.5"))

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
(require 's)
(require 'dash)
(require 'helm)
(require 'helm-grep)
(require 'helm-files)
(require 'cl-lib)

(defgroup helm-codesearch nil
  "Helm interface for codesearch."
  :group 'helm)

(defface helm-codesearch-file-face
  '((t :inherit font-lock-function-name-face))
  "Face for file."
  :group 'helm-codesearch)

(defface helm-codesearch-lineno-face
  '((t :inherit font-lock-constant-face))
  "Face for lineno."
  :group 'helm-codesearch)

(defface helm-codesearch-source-face
  '((t :inherit font-lock-doc-face))
  "Face for source."
  :group 'helm-codesearch)

(defcustom helm-codesearch-csearchindex
  ".csearchindex"
  "Index file for each projects."
  :group 'helm-codesearch)

(defcustom helm-codesearch-abbreviate-filename 80
  "Abbreviate filename length."
  :group 'helm-codesearch)

(defvar helm-codesearch-buffer "*helm codesearch*")
(defvar helm-codesearch-indexing-buffer "*helm codesearch indexing*")
(defvar helm-codesearch-file nil)

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

(defun helm-codesearch-abbreviate-file (file)
  "FILE."
  (with-temp-buffer
    (insert file)
    (let* ((start (- (point) (length file)))
           (end (point))
           (amount (if (numberp helm-codesearch-abbreviate-filename)
                       (- (- end start) helm-codesearch-abbreviate-filename)
                     999))
           (advance-word (lambda ()
                           "Return the length of the text made invisible."
                           (let ((wend (min end (progn (forward-word 1) (point))))
                                 (wbeg (max start (progn (backward-word 1) (point)))))
                             (goto-char wend)
                             (if (<= (- wend wbeg) 1)
                                 0
                               (put-text-property (1+ wbeg) wend 'invisible t)
                               (1- (- wend wbeg)))))))
      (goto-char start)
      (while (and (> amount 0) (> end (point)))
        (cl-decf amount (funcall advance-word)))
      (goto-char end))
    (buffer-substring (point-min) (point-max))))

(defconst helm-codesearch-pattern-regexp
  "^\\([[:lower:][:upper:]]?:?.*?\\):\\([0-9]+\\):\\(.*\\)")

(defun helm-codesearch-make-pattern-format (candidate)
  "Make pattern format from CANDIDATE."
  (-when-let* (((_ file lineno source)
                (s-match helm-codesearch-pattern-regexp candidate))
               (file (propertize file 'face 'helm-codesearch-file-face))
               (lineno (propertize lineno 'face 'helm-codesearch-lineno-face))
               (source (propertize source 'face 'helm-codesearch-source-face))
               (display-line (format "%08s: %s" lineno source))
               (real-file (format "%s:%s:" file 1)))
    (if (string= file helm-codesearch-file)
        (list (cons display-line candidate))
      (progn
        (setq helm-codesearch-file file)
        (list (cons (helm-codesearch-abbreviate-file file) real-file)
              (cons display-line candidate))))))

(defun helm-codesearch-find-pattern-transformer (candidates source)
  "Transformer is run on the CANDIDATES and not use the SOURCE."
  (-mapcat 'helm-codesearch-make-pattern-format candidates))

(defun helm-codesearch-make-file-format (candidate)
  "Make file format from CANDIDATE."
  (-when-let* ((file (propertize candidate 'face 'helm-codesearch-file-face)))
    (list (cons (helm-codesearch-abbreviate-file file) candidate))))

(defun helm-codesearch-find-file-transformer (candidates source)
  "Transformer is run on the CANDIDATES and not use the SOURCE."
  (-mapcat 'helm-codesearch-make-file-format candidates))

(defun helm-codesearch-pattern-command ()
  "Find pattern command."
  (s-join " " (list "csearch" "-n" helm-pattern)))

(defun helm-codesearch-file-command ()
  "Find file command."
  (s-join " " (list "csearch" "-l" "-f" helm-pattern "$")))

(defun helm-codesearch-set-process-sentinel (proc)
  "Set process sentinel to PROC."
  (prog1 proc
    (set-process-sentinel
     proc
     (lambda (process event)
       (helm-process-deferred-sentinel-hook
        process event (helm-default-directory))))))

(defun helm-codesearch-find-pattern-process ()
  "Execute the csearch for a pattern."
  (let ((proc (start-process-shell-command
               "codesearch" nil
               (helm-codesearch-pattern-command))))
    (setq helm-codesearch-file nil)
    (helm-codesearch-set-process-sentinel proc)))

(defun helm-codesearch-find-file-process ()
  "Execute the csearch for a file."
  (let ((proc (start-process-shell-command
               "codesearch" nil
               (helm-codesearch-file-command))))
    (helm-codesearch-set-process-sentinel proc)))

(defun helm-codesearch-create-csearchindex-process (dir)
  "Execute the cindex from a DIR."
  (let ((proc (apply 'start-process "codesearch"
                     helm-codesearch-indexing-buffer
                     "cindex" (list dir))))
    (with-current-buffer helm-codesearch-indexing-buffer
      (let ((buffer-read-only nil))
        (erase-buffer))
      (pop-to-buffer helm-codesearch-indexing-buffer)
      (setq buffer-read-only t)
      (goto-char (point-max)))))

(defclass helm-codesearch-source-pattern (helm-source-async)
  ((header-name
    :initform
    (lambda (name)
      (concat name " [" (helm-attr 'csearchindex) "]")))
   (csearchindex :initarg :csearchindex
                 :initform nil
                 :custom string
                 :documentation " Index file.")
   (candidates-process :initform 'helm-codesearch-find-pattern-process)
   (filtered-candidate-transformer
    :initform 'helm-codesearch-find-pattern-transformer)
   (action :initform (helm-make-actions
                      "Find File" 'helm-grep-action
                      "Find file other frame" 'helm-grep-other-frame
                      (lambda () (and (locate-library "elscreen")
                                 "Find file in Elscreen"))
                      'helm-grep-jump-elscreen
                      "Save results in grep buffer" 'helm-grep-save-results
                      "Find file other window" 'helm-grep-other-window))
   (persistent-action :initform 'helm-grep-persistent-action)
   (nohighlight :initform t)
   (history :initform 'helm-grep-history)
   (help-message :initform 'helm-grep-help-message)
   (keymap :initform helm-grep-map)
   (candidate-number-limit :initform 200)
   (requires-pattern :initform 4)))

(defclass helm-codesearch-source-file (helm-source-async)
  ((header-name
    :initform
    (lambda (name)
      (concat name " [" (helm-attr 'csearchindex) "]")))
   (csearchindex :initarg :csearchindex
                 :initform nil
                 :custom string
                 :documentation " Index file.")
   (candidates-process :initform 'helm-codesearch-find-file-process)
   (action :initform 'helm-type-file-actions)
   (filtered-candidate-transformer
    :initform 'helm-codesearch-find-file-transformer)
   (keymap :initform helm-generic-files-map)
   (candidate-number-limit :initform 200)
   (requires-pattern :initform 4)))

;;;###autoload
(defun helm-codesearch-find-pattern (query)
  "Find pattern with QUERY."
  (interactive
   (list (read-string "Find pattern: " (current-word))))
  (when (helm-codesearch-search-csearchindex)
    (helm :sources (helm-make-source
                       "Codesearch: Find pattern"
                       'helm-codesearch-source-pattern
                     :csearchindex (getenv "CSEARCHINDEX"))
          :buffer helm-codesearch-buffer
          :input query
          :keymap helm-grep-map
          :truncate-lines t)))

;;;###autoload
(defun helm-codesearch-find-file (query)
  "Find file with QUERY."
  (interactive
   (list (read-string "Find file: " (current-word))))
  (when (helm-codesearch-search-csearchindex)
    (helm :sources (helm-make-source
                       "Codesearch: Find file"
                       'helm-codesearch-source-file
                     :csearchindex (getenv "CSEARCHINDEX"))
          :buffer helm-codesearch-buffer
          :input query
          :keymap helm-generic-files-map
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

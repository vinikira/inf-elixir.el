;;; inf-elixir.el --- summary -*- lexical-binding: t -*-

;; Author: Vinícius Simões <viniciussimoes@protonmail.com>
;; Maintainer: Vinícius Simões
;; Version: 0.0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/lambdart/inf-elixir.el
;; Keywords: emacs elisp elixir comint


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 'comint)

(defgroup inf-elixir nil
  "Elixir comint/inferior functionalites."
  :prefix "inf-elixir-"
  :group 'elixir)

(defcustom inf-elixir-prompt-read-only t
  "If non-nil, the comint buffer will be read-only."
  :group 'inf-elixir
  :type 'boolean)

(defcustom inf-elixir-pop-to-buffer-flag nil
  "Non-nil means pop to buffer in the same window."
  :group 'inf-elixir
  :type 'boolean)

(defcustom inf-elixir-buffer-name "*inf-elixir*"
  "Inferior buffer default name."
  :group 'inf-elixir
  :type 'string)

(defcustom inf-elixir-prompt-regex "^iex>+ *"
  "Regexp to recognize prompt."
  :group 'inf-elixir
  :type 'regexp)

(defcustom inf-elixir-proc-timeout 2
  "The `accept-process-output' timeout in seconds."
  :group 'inf-elixir
  :type 'integer)

(defcustom inf-elixir-program (executable-find "iex")
  "Elixir executable full path program."
  :group 'inf-elixir
  :type 'string)

(defcustom inf-elixir-args '()
  "Command-line arguments to pass to `inf-elixir-program'."
  :group 'inf-elixir
  :type 'list)

(defcustom inf-elixir-start-file nil
  "Inferior Elixir start file."
  :group 'info-clojure
  :type 'string)

(defcustom inf-elixir-source-modes '(elixir-mode)
  "Used to determine if a buffer contains Elixir sourc code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a Elixir source file by `inf-elixir-load-file'."
  :group 'inf-elixir
  :type '(repeat function))

(defvar inf-elixir-ops-alist
  `((import-file . "import_file \"%s\"")
    (help . "h %s")
    (type . "t %s"))
  "Operation associative list: (OP-KEY . OP-FMT).")

(defvar inf-elixir-overlay (make-overlay (point-min) (point-min) nil t t)
  "Overlay used to display the process output text.")

(defvar inf-elixir-proc-buffer nil
  "Comint process buffer.")

(defvar inf-elixir-proc-output-list '()
  "Process output string list.")

(defvar inf-elixir-version "0.0.1"
  "Current version string.")

(defvar inf-elixir-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.")

(defun inf-elixir-proc ()
  "Return comint buffer current process."
  (and inf-elixir-proc-buffer
       (get-buffer-process inf-elixir-proc-buffer)))

(defun inf-elixir-proc-sentinel (process event)
  "Sentinel function to handle (PROCESS EVENT) relation."
  (princ (format "Process: %s had the event '%s'" process event)))

(defun inf-elixir-proc-wait (proc timeout)
  "Wait for the PROC output, leave if reaches the TIMEOUT."
  (let ((string (car inf-elixir-proc-output-list)))
    (while (and (stringp string)
		(not (string-match-p comint-prompt-regexp string)))
      (accept-process-output proc timeout)
      (sleep-for nil 100))))

(defun inf-elixir-comint-preoutput-filter (string)
  "Return the output STRING filtered."
  (let ((string (if (stringp string) string "")))
    (push string inf-elixir-proc-output-list)
    string))

(defun inf-elixir-comint-send (send-func &optional timeout &rest args)
  "Send ARGS (string or region) using the chosen SEND-FUNC.
Possible values of SEND-FUNC are: `comint-send-string' or `comint-send-region'.
TIMEOUT, the `accept-process-output' timeout."
  (let ((proc (inf-elixir-proc))
	(timeout (or timeout inf-elixir-proc-timeout))
	(comint-preoutput-filter-functions '(inf-elixir-comint-preoutput-filter)))
    (when (process-live-p proc)
      (setq inf-elixir-proc-output-list '())
      (apply 'funcall send-func proc args)
      (comint-send-string proc "\n")
      (accept-process-output proc timeout)
      (inf-elixir-proc-sentinel proc timeout))))

(defun inf-elixir-comint-send-string (string &optional op-key timeout)
  "Send STRING to the current inferior process.
Format the string selecting the right format using the OP-KEY.1
TIMEOUT, the `accept-process-output' timeout."
  (let ((string (if (not op-key) string
		  (format (cdr (assoc op-key inf-elixir-ops-alist)) string))))
    (inf-elixir-comint-send #'comint-send-string timeout string)))

(defun inf-elixir-comint-send-region (start end &optional timeout)
  "Send region delimited bu START/END to the inferior process.
TIMEOUT, the `accept-process-output' timeout."
  (inf-elixir-comint-send #'comint-send-region timeout start end))

(defun inf-elixir-comint-input-sender (_ string)
  "Comint input sender STRING function."
  (inf-elixir-comint-send-string string))

(defun inf-elixir-comint-input-filter (string)
  "Don't save anything on the STRING matching `inf-elixir-filter-regexp'."
  (not (string-match-p inf-elixir-filter-regexp string)))

(defun inf-elixir-comint-get-old-input ()
  "Snarf the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun inf-elixir-comint-setup ()
  "Helper function to setup `comint-mode' related variables."
  (setq comint-process-echoes t
	comint-input-ignoredups nil
	comint-use-prompt-regexp t))

(defun inf-elixir-comint-quit ()
  "Quit Elixir comint, i.e, quit subjob and kill the buffer."
  (interactive)
  (and inf-elixir-proc-buffer
       (with-current-buffer inf-elixir-proc-buffer
	 (comint-quit-subjob)))
  (kill-buffer inf-elixir-proc-buffer))

;;;###autoload
(defun inf-elixir-comint-run ()
  "Run an inferior instance of some Elixir REPL inside Emacs."
  (interactive)
  (let ((buffer (apply 'make-comint
		       inf-elixir-buffer-name
		       inf-elixir-program
		       inf-elixir-start-file
		       inf-elixir-args)))
    (unless buffer
      (error "[inf-elixir]: make comint fails"))
    (comint-check-proc buffer)
    (set-process-sentinel (get-buffer-process buffer) #'inf-elixir-proc-sentinel)
    (setq inf-elixir-proc-buffer buffer)
    (with-current-buffer buffer (inf-elixir-mode))
    (display-buffer buffer 'display-buffer-pop-up-window)))

(defun inf-elixir-display-version ()
  "Echo the current `inf-elixir' version."
  (interactive)
  (message "inf-elixir (version %s)" inf-elixir-version))

(defun inf-elixir-display-overlay (line)
  "Display `inf-elixir-overlay' with the last LINE output."
  (move-overlay inf-elixir-overlay (point) (point) (current-buffer))
  (put-text-property 0 1 'cursor t line)
  (overlay-put inf-elixir-overlay 'after-string line))

(defun inf-elixir-remove-overlay ()
  "Remove `info-elixir-overlay' display (if any) prior to new user input."
  (delete-overlay inf-elixir-overlay))

(defun inf-elixir-eval-def ()
  "Send 'def' to the inferior Elixir process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (inf-elixir-comint-send-region (point) end))))

(defun inf-elixir-read-thing (&optional prompt thing)
  "Return `thing-at-point' (string) or read it.
If PROMPT is non-nil use it as the read prompt.
If THING  is non-nil use it as the `thing-at-point' parameter,
default: 'symbol."
  (let* ((str (thing-at-point (or thing 'symbol) t))
	 (fmt (if (not str) "%s:" "%s: [%s]"))
	 (prompt (format fmt (or prompt "Str: ") str)))
    (list (read-string prompt nil nil str))))

(defun inf-elixir-eval-buffer ()
  "Send the current buffer to the inferior Elixir process."
  (interactive)
  (save-excursion
    (widen)
    (let ((case-fold-search t))
      (inf-elixir-comint-send-region (point-min)
				     (point-max)))))

(defun inf-elixir-eval-region (start end)
  "Send the current region delimited by START/END to the inferior Elixir process."
  (interactive "r")
  (inf-elixir-comint-send-region start end))

(defun inf-elixir-load-file (file-name)
  "Load a Clojure file defined by its FILE-NAME into the inferior process."
  (interactive (comint-get-source "Elixir file: "
				  inf-elixir-prev-l/c-dir/file
				  inf-elixir-source-modes t))
  (comint-check-source file-name)
  (setq inf-elixir-prev-l/c-dir/file
        (cons (file-name-directory file-name)
              (file-name-nondirectory file-name)))
  (inf-elixir-comint-send-string file-name 'import-file))

(defun inf-elixir-help (name)
  "Invoke `h NAME` operation."
  (interactive (inf-elixir-read-thing "Help"))
  (inf-elixir-comint-send-string name 'help))

(defun inf-elixir-type-help (name)
  "Invoke `t NAME` operation."
  (interactive (inf-elixir-read-thing "Type Help"))
  (inf-elixir-comint-send-string name 'type))

(defun inf-elixir-apropos (str-or-regex)
  "Invoke Elixir apropos STR-OR-REGEX operation."
  (interactive (inf-elixir-read-thing "Search for"))
  ;; (inf-elixir-comint-send-string str-or-regex 'apropos)
  (error "Not implemented yet"))

(provide 'inf-elixir)

;;; inf-elixir.el ends here

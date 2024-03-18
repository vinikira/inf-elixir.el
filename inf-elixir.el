;;; inf-elixir.el --- An elixir inferior mode -*- lexical-binding: t -*-

;; Author: Vinícius Simões <viniciussimoes@protonmail.com>
;; Maintainer: Vinícius Simões
;; Version: 0.0.1
;; Package-Requires: ((emacs . "29.1"))
;; Homepage: https://github.com/vinikira/inf-elixir.el
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
(require 'ansi-color)
(require 'project)
(require 'thingatpt)
(require 'treesit)

;;; Custom group

(defgroup inf-elixir nil
  "Elixir comint/inferior functionalities."
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

(defcustom inf-elixir-buffer-name "inf-elixir"
  "Inferior buffer default name."
  :group 'inf-elixir
  :type 'string)

(defcustom inf-elixir-prompt-regexp "^\\(iex\\|\\.\\.\\.\\).*>"
  "Regexp to recognize prompt."
  :group 'inf-elixir
  :type 'regexp)

(defcustom inf-elixir-iex-executable "iex"
  "Elixir executable full path program."
  :group 'inf-elixir
  :type 'file
  :set `(lambda (symbol value)
          (set symbol (or (executable-find value) value))))

(defcustom inf-elixir-args '()
  "Command-line arguments to pass to `inf-elixir-iex-executable'."
  :group 'inf-elixir
  :type 'list)

(defcustom inf-elixir-start-file nil
  "Inferior Elixir start file."
  :group 'info-elixir
  :type 'string)

(defcustom inf-elixir-source-modes '(elixir-mode elixir-ts-mode)
  "Used to determine if a buffer contains Elixir sourc code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a Elixir source file by `inf-elixir-load-file'."
  :group 'inf-elixir
  :type '(repeat function))

(defcustom inf-elixir-enable-completion nil
  "If no-nil, enables the completions using iex."
  :group 'inf-elixir
  :type 'bool)

(defcustom inf-elixir-output-timeout 2
  "Comint accept output timeout in seconds."
  :group 'inf-elixir
  :type 'integer)

;;; Variables

(defvar inf-elixir-ops-alist
  `((import-file . "import_file \"%s\"")
     (help . "h(%s)")
     (type . "t(%s)")
     (info . "i(%s)")
     (complete . "case IEx.Autocomplete.expand(Enum.reverse('%s')), do: ({:yes, e, []} -> [to_string(e)]; {:yes, [], o} -> Enum.map(o, &to_string/1); _ -> [])"))
     (behaviour . "b(%s)")
  "Operation associative list: (OP-KEY . OP-FMT).")

(defvar inf-elixir-overlay (make-overlay (point-min) (point-min) nil t t)
  "Overlay used to display the process output text.")

(defvar inf-elixir-proc-buffer nil
  "Comint process buffer.")

(defvar inf-elixir-proc-output-list '()
  "Process output string list.")

(defvar inf-elixir-version "0.0.1"
  "Current version string.")

(defvar inf-elixir-last-output ""
  "Process (cache) last output text.")

(defvar inf-elixir-last-output-line ""
  "Process (cache) last output line.")

(defvar inf-elixir-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.")

(defvar inf-elixir-comint-filter-in-progress nil
  "Check if filter is running.")

(defvar inf-elixir-comint-completion-in-progress nil
  "Check if completion is running.")

;;; Commands

;;;###autoload
(defun inf-elixir-comint-run ()
  "Run an inferior instance of some Elixir REPL inside Emacs."
  (interactive)
  (let ((buffer (apply 'make-comint
                  inf-elixir-buffer-name
                  inf-elixir-iex-executable
                  inf-elixir-start-file
                  inf-elixir-args)))
    (unless buffer
      (error "[inf-elixir]: make comint fails"))
    (comint-check-proc buffer)
    (set-process-sentinel (get-buffer-process buffer) #'inf-elixir--proc-sentinel)
    (setq inf-elixir-proc-buffer buffer)
    (with-current-buffer buffer (inf-elixir-mode))
    (display-buffer buffer 'display-buffer-pop-up-window)))

;;;###autoload
(defalias 'run-elixir #'inf-elixir-comint-run)

;;;###autoload
(defun inf-elixir-comint-project-run (&optional cmd)
  "Run an inferior instance of some Elixir project REPL inside Emacs.
If CMD non-nil, ask for the custom command to invoke iex."
  (interactive (list (if current-prefix-arg
                       (read-from-minibuffer "Type the command: ")
                       "")))
  (let* ((project (project-current))
          (default-directory (if project
                               (project-root project)
                               (read-directory-name "Select the project root: ")))
          (inf-elixir-buffer-name
            (inf-elixir--project-buffer-name))
          (cmd-splited (split-string cmd " "))
          (inf-elixir-iex-executable (cond
                                       ((not (string-empty-p cmd))
                                         (car cmd-splited))
                                       (t
                                         inf-elixir-iex-executable)))
          (inf-elixir-args (cond
                             ((not (string-empty-p cmd))
                               (cdr cmd-splited))
                             ((not (eq inf-elixir-args '()))
                               inf-elixir-args)
                             (t
                               '("-S" "mix")))))
    (inf-elixir-comint-run)))

;;;###autoload
(defalias 'run-elixir-project #'inf-elixir-comint-project-run)

(defun inf-elixir-display-version ()
  "Echo the current `inf-elixir' version."
  (interactive)
  (message "inf-elixir (version %s)" inf-elixir-version))

(defun inf-elixir-eval-def ()
  "Send a def to the inferior Elixir process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (inf-elixir--comint-send-region (point) end))))

(defun inf-elixir-eval-last-sexp ()
  "Send the previous sexp to the inferior Elixir process."
  (interactive)
  (inf-elixir--comint-send-string (thing-at-point 'sexp))
  (inf-elixir--wait-output-filter)
  (when inf-elixir-minor-mode
    (inf-elixir--display-overlay
      (concat " => "  (ansi-color-apply inf-elixir-last-output-line)))))

(defun inf-elixir-eval-buffer ()
  "Send the current buffer to the inferior Elixir process."
  (interactive)
  (save-excursion
    (widen)
    (let ((case-fold-search t))
      (inf-elixir--comint-send-region (point-min)
        (point-max)))))

(defun inf-elixir-eval-region (start end)
  "Send the current region delimited by START/END to the inferior Elixir process."
  (interactive "r")
  (inf-elixir--comint-send-region start end))

(defun inf-elixir-load-file (file-name)
  "Load a Elixir file defined by its FILE-NAME into the inferior process."
  (interactive (comint-get-source "Elixir file: "
                 inf-elixir-prev-l/c-dir/file
                 inf-elixir-source-modes t))
  (comint-check-source file-name)
  (setq inf-elixir-prev-l/c-dir/file
    (cons (file-name-directory file-name)
      (file-name-nondirectory file-name)))
  (inf-elixir--comint-send-string file-name 'import-file))

(defun inf-elixir-doc-help (name)
  "Invoke `h NAME` operation."
  (interactive (inf-elixir--read-thing "Documentation Help"))
  (inf-elixir--comint-send-string name 'help)
  (inf-elixir--wait-output-filter)
  (inf-elixir--show-help-buffer))

(defun inf-elixir-type-help (name)
  "Invoke `t NAME` operation."
  (interactive (inf-elixir--read-thing "Type Help"))
  (inf-elixir--comint-send-string name 'type)
  (inf-elixir--wait-output-filter)
  (inf-elixir--show-help-buffer))

(defun inf-elixir-info-help (name)
  "Invoke `i NAME` operation."
  (interactive (inf-elixir--read-thing "Info Help"))
  (inf-elixir--comint-send-string name 'info)
  (inf-elixir--wait-output-filter)
  (inf-elixir--show-help-buffer))

(defun inf-elixir-info-behaviour-help (name)
  "Invoke `b NAME` operation."
  (interactive (inf-elixir--read-thing "Behaviour Help"))
  (inf-elixir--comint-send-string name 'behaviour)
  (inf-elixir--wait-output-filter)
  (inf-elixir--show-help-buffer))

(defun inf-elixir-apropos (str-or-regex)
  "Invoke Elixir apropos STR-OR-REGEX operation."
  (interactive (inf-elixir--read-thing "Search for"))
  (inf-elixir--comint-send-string str-or-regex 'apropos)
  (error "Not implemented yet"))

(defun inf-elixir-comint-quit ()
  "Quit Elixir comint, i.e, quit subjob and kill the buffer."
  (interactive)
  (and inf-elixir-proc-buffer
    (with-current-buffer inf-elixir-proc-buffer
      (comint-quit-subjob)
      (while (process-live-p (inf-elixir--proc))
        (sleep-for 0 10))
      (kill-buffer inf-elixir-proc-buffer))))

;;; Complete

(defun inf-elixir--complete-at-point ()
  "Invoke completions for elixir expressions."
  (let ((expr (inf-elixir--get-expr)))
    (inf-elixir--clear-cache)
    (setq inf-elixir-comint-completion-in-progress t)
    (inf-elixir--comint-send-string (nth 2 expr) 'complete)
    (inf-elixir--wait-output-filter)
    (setq inf-elixir-comint-completion-in-progress nil)
    (list (nth 0 expr) (nth 1 expr) (inf-elixir--get-completions))))

(defun inf-elixir--get-completions ()
  "Get completions list."
  (let* ((replace-regexp
           "^\\(iex\\|\\.\\.\\.\\).*>\\|,\\|'\\|\"\\|\\[\\|\\]\\|{\\|}\\|\:yes\\|\:no\\|\n")
          (last-output (ansi-color-filter-apply inf-elixir-last-output))
          (sanatized-output (replace-regexp-in-string
                              replace-regexp "" last-output)))
    (split-string sanatized-output)))

(defun inf-elixir--get-expr ()
  "Return the expression under the cursor."
  (if (or (looking-at "\s") (eolp))
    (let (p1 p2 p3 (skip-chars "-_A-Za-z0-9.?!@:"))
      (save-excursion
        (skip-chars-backward skip-chars)
        (setq p1 (point))
        (skip-chars-forward skip-chars)
        (setq p2 (point))
        (thing-at-point--beginning-of-symbol)
        (setq p3 (point))
        `(,p3 ,p2 ,(buffer-substring-no-properties p1 p2))))))

;;; Overlay

(defun inf-elixir--display-overlay (text)
  "Display TEXT using `inf-elixir-overlay'."
  (move-overlay inf-elixir-overlay (point) (point) (current-buffer))
  (put-text-property 0 1 'cursor t text)
  (overlay-put inf-elixir-overlay 'after-string text))

(defun inf-elixir--delete-overlay ()
  "Remove `info-elixir-overlay' display (if any) prior to new user input."
  (delete-overlay inf-elixir-overlay))

;;; Private functions

(defun inf-elixir--clear-cache ()
  "Clear the caches"
  (setq inf-elixir-last-output ""
    inf-elixir-last-output-line ""))

(defun inf-elixir--show-help-buffer ()
  "Show inf-elixir helper buffer."
  (let ((buffer (get-buffer-create "*inf-elixir-help*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert inf-elixir-last-output)
      (ansi-color-apply-on-region (point-min) (point-max))
      (goto-char (point-min))
      (save-excursion
        (while (re-search-forward inf-elixir-prompt-regexp nil t)
          (replace-match "")))
      (help-mode)
      (display-buffer buffer 'display-buffer-pop-up-window))))

(defun inf-elixir--project-buffer-name ()
  "Extract the project name."
  (format "%s-%s"
    inf-elixir-buffer-name
    (car (last (split-string default-directory "/" t)))))

(defun inf-elixir--read-thing (&optional prompt)
  "Prompt the user using the PROMPT about the thing to select.
Tries to infer the module function expression if the thing is function call."
  (let* ((str (inf-elixir--module-function-expr))
          (fmt (if (not str) "%s: " "%s (default %s): "))
          (prompt (format fmt (or prompt "Str: ") str)))
    (list (read-string prompt nil nil str))))

(defun inf-elixir--module-function-expr ()
  "Try to infer the aliases of the current symbol."
  (let* ((node (treesit-node-at (point)))
          (parent-call (treesit-parent-until
                         node
                         (lambda (n)
                           (member (treesit-node-type n)
                             '("dot"))) t))
          (module-name (if (null parent-call)
                         (treesit-node-text node t)
                         (treesit-node-text (treesit-node-child parent-call 0) t)))
          (splitted-module (string-split module-name "\\."))
          (module-name (nth 0 splitted-module))
          (non-alias-module-name (nth 1 splitted-module))
          (function-name (when (not (null parent-call))
                           (treesit-node-text (treesit-node-child parent-call 2) t)))
          (query-result (treesit-query-capture 'elixir
                          `(((call
                               target: (identifier) @keyword
                               (arguments (alias) @alias_name))
                              (:match "^alias$" @keyword)
                              (:match ,(format "%s$" module-name) @alias_name)))
                          (point-min) (point-max))))
    (pcase query-result
      (`(,_ (alias_name . ,node-alias))
        (cond
          ((null function-name)
            (treesit-node-text node-alias t))
          ((null non-alias-module-name)
            (format "%s.%s"
              (treesit-node-text node-alias t)
              function-name))
          (t
            (format "%s.%s.%s"
              (treesit-node-text node-alias t)
              non-alias-module-name
              function-name))))
      (_ (treesit-node-text node t)))))

(defun inf-elixir--proc ()
  "Return comint buffer current process."
  (and inf-elixir-proc-buffer
    (get-buffer-process inf-elixir-proc-buffer)))

(defun inf-elixir--proc-sentinel (process event)
  "Sentinel function to handle (PROCESS EVENT) relation."
  (princ (format "Process: %s had the event '%s'" process event)))

(defun inf-elixir--proc-cache-output ()
  "Parse and cache the process output."
  (let ((text (mapconcat (lambda (str) str)
                (reverse inf-elixir-proc-output-list) "")))
    (setq inf-elixir-last-output text
      inf-elixir-last-output-line
      (car (last (split-string inf-elixir-last-output "\n") 2)))))

(defun inf-elixir--comint-preoutput-filter (string)
  "Return the output STRING."
  (let* ((string (if (stringp string) string ""))
          (string-without-ansi (ansi-color-filter-apply string))
          (match-prompt? (string-match-p inf-elixir-prompt-regexp string-without-ansi)))
    (push string inf-elixir-proc-output-list)
    (when match-prompt?
      (inf-elixir--proc-cache-output)
      (setq inf-elixir-comint-filter-in-progress nil))
    (when (and
            (null inf-elixir-comint-filter-in-progress)
            (not match-prompt?))
      (inf-elixir--comint-send-string ""))
    (if inf-elixir-comint-completion-in-progress
      ""
      (ansi-color-apply string))))

(defun inf-elixir--comint-send (send-func &rest args)
  "Send ARGS (string or region) using the chosen SEND-FUNC.
Possible values of SEND-FUNC are: `comint-send-string' or `comint-send-region'."
  (let ((proc (inf-elixir--proc)))
    (when (process-live-p proc)
      (setq inf-elixir-comint-filter-in-progress t
        inf-elixir-proc-output-list '())
      (apply 'funcall send-func proc args)
      (comint-send-string proc "\n"))))

(defun inf-elixir--wait-output-filter ()
  "Wait for comint output filter."
  (run-with-timer
    inf-elixir-output-timeout
    nil
    (lambda ()
      (setq inf-elixir-comint-filter-in-progress nil)))

  (while inf-elixir-comint-filter-in-progress
    (sleep-for 0 10)))

(defun inf-elixir--spot-prompt (_string)
  "Move the point to the process mark."
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (save-excursion
        (goto-char (process-mark proc))))))

(defun inf-elixir--comint-send-string (string &optional op-key)
  "Send STRING to the current inferior process.
Format the string selecting the right format using the OP-KEY."
  (let ((string (if (not op-key) string
                  (format (cdr (assoc op-key inf-elixir-ops-alist)) string))))
    (inf-elixir--comint-send #'comint-send-string string)))

(defun inf-elixir--comint-send-region (start end)
  "Send region delimited bu START/END to the inferior process."
  (inf-elixir--comint-send #'comint-send-region start end))

(defun inf-elixir--comint-input-sender (_ string)
  "Comint input sender STRING function."
  (inf-elixir--comint-send-string string))

(defun inf-elixir--comint-get-old-input ()
  "Snarf the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun inf-elixir--comint-setup ()
  "Helper function to setup `comint-mode' related variables."
  (setq comint-process-echoes t
    comint-input-ignoredups nil
    comint-use-prompt-regexp t))

;;; Easy menu

(defvar inf-elixir-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (when inf-elixir-enable-completion
      (define-key map "\t" #'completion-at-point)
      (define-key map (kbd "TAB") #'completion-at-point))
    (define-key map (kbd "C-x C-e") #'inf-elixir-eval-last-sexp)
    (define-key map (kbd "C-c C-l") #'inf-elixir-load-file)
    (define-key map (kbd "C-c C-q") #'inf-elixir-comint-quit)
    (easy-menu-define inf-elixir-mode-menu map
      "Inferior Elixir REPL Menu"
      '("Inf-Elixir REPL"
         ["Eval last sexp" inf-elixir-eval-last-sexp t]
         "--"
         ["Load file" inf-elixir-load-file t]
         "--"
         ["Quit" inf-elixir-comint-quit]
         "--"
         ["Version" inf-elixir-display-version]))
    map))

;;; Inf Elixir minor mode definition
(defvar inf-elixir-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-x") #'inf-elixir-eval-def) ;; Emacs convention
    (define-key map (kbd "C-x C-e") #'inf-elixir-eval-last-sexp) ;; Emacs convention
    (define-key map (kbd "C-c C-e") #'inf-elixir-eval-buffer) ;; Emacs convention
    (define-key map (kbd "C-c C-r") #'inf-elixir-eval-region)
    (define-key map (kbd "C-c C-c l") #'inf-elixir-load-file)
    (define-key map (kbd "C-c C-c h h") #'inf-elixir-doc-help)
    (define-key map (kbd "C-c C-c h t") #'inf-elixir-type-help)
    (define-key map (kbd "C-c C-c h i") #'inf-elixir-info-help)
    (define-key map (kbd "C-c C-c h b") #'inf-elixir-info-behaviour-help)
    (define-key map (kbd "C-c C-c q") #'inf-elixir-comint-quit)
    (easy-menu-define inf-elixir-minor-mode-menu map
      "Inferior Elixir Minor Mode Menu"
      '("Inf-Elixir"
         ["Eval region" inf-elixir-eval-region t]
         ["Eval buffer" inf-elixir-eval-buffer t]
         ["Eval function" inf-elixir-eval-def t]
         ["Eval last sexp" inf-elixir-eval-last-sexp t]
         "--"
         ["Load file..." inf-elixir-load-file t]
         "--"
         ["Doc Help..." inf-elixir-doc-help t]
         ["Type Help..." inf-elixir-type-help t]
         ["Info Help..." inf-elixir-info-help t]
         ["Behaviour Help..." inf-elixir-info-behaviour-help t]
         "--"
         ["Quit REPL" inf-elixir-comint-quit]))
    map))

;;;###autoload
(define-minor-mode inf-elixir-minor-mode
  "Minor mode for interacting with the inferior Elixir (comint) process buffer.
If called interactively, toggle ‘Inf-Elixir minor mode’.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.
If called from Lisp, toggle the mode if ARG is ‘toggle’.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.
The mode’s hook is called both when the mode is enabled and when
it is disabled.
The following commands are available:
\\{inf-elixir-minor-mode-map}"
  :lighter ""
  :keymap inf-elixir-minor-mode-map
  (cond
    (inf-elixir-minor-mode
      (setq-local comint-input-sender #'inf-elixir--comint-input-sender)
      (when inf-elixir-enable-completion
        (add-hook 'completion-at-point-functions #'inf-elixir--complete-at-point 100 t))
      (add-hook 'pre-command-hook #'inf-elixir--delete-overlay nil t))
    (t
      (inf-elixir--delete-overlay)
      (remove-hook 'pre-command-hook #'inf-elixir--delete-overlay t))))

;; Inf Elixir mode
(defvar inf-elixir-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" #'completion-at-point)
    (define-key map (kbd "C-c C-q") #'inf-elixir-comint-quit)
    (easy-menu-define inf-elixir-mode-menu map
      "Inferior Elixir Minor Mode Menu"
      '("Inf-Elixir"
         ["Load file..." inf-elixir-load-file t]
         "--"
         ["Quit REPL" inf-elixir-comint-quit]))
    map))

(define-derived-mode inf-elixir-mode comint-mode "Inf-Elixir"
  "Major mode for `inf-elixir' comint buffer.
Runs a Elixir interpreter with the help of `comint-mode',
use the buffer abstraction as the main I/O bridge between
Emacs and the subprocess.
You can send text to the inferior Elixir process from other
buffers or the `minibuffer'
directly.
    `inf-elixir-eval-defn'  sends function definition
    `inf-elixir-eval-region' sends the current region
    `inf-elixir-eval-buffer' sends the current buffer
The following commands are available:
\\{inf-elixir-minor-mode-map}"
  :group 'inf-elixir
  :keymap inf-elixir-mode-map

  (setq-local
    comint-prompt-regexp inf-elixir-prompt-regexp
    comint-prompt-read-only inf-elixir-prompt-read-only
    comint-input-sender #'inf-elixir--comint-input-sender
    comint-get-old-input #'inf-elixir--comint-get-old-input)

  (add-hook 'completion-at-point-functions
    #'inf-elixir--complete-at-point -100 t)

  (add-hook 'comint-preoutput-filter-functions
    #'inf-elixir--comint-preoutput-filter 100 t)

  (add-hook 'comint-output-filter-functions
    #'comint-truncate-buffer 100 t)

  (add-hook 'comint-output-filter-functions
    #'inf-elixir--spot-prompt 100 t)

  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) inf-elixir-prompt-regexp))

;;;###autoload
(add-hook 'inf-elixir-minor-mode-hook #'inf-elixir--comint-setup)

(provide 'inf-elixir)

;;; inf-elixir.el ends here

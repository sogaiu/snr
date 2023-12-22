;;; snr.el --- Spork Netrepl REPL -*- lexical-binding: t; -*-

;; Author: sogaiu
;; Version: 20230406
;; Package-Requires: ((emacs "26.2"))
;; Keywords: janet, spork, netrepl

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Code to interact with a spork netrepl server via a local proxy

;;;; Installation

;;;;; Manual

;;  Having janet's spork library installed may be helpful for starting
;;  a netrepl server.  If you have some other way of providing a
;;  netrepl server, the spork library should not be necessary.
;;
;;  Add the directory this file is in to your load-path.
;;
;;  Put this in your relevant init file:
;;
;;    (require 'snr)
;;
;;  Optionally, add:
;;
;;    (add-hook 'janet-ts-mode-hook
;;              #'snr-interaction-mode)
;;
;;    or
;;
;;    (add-hook 'janet-mode-hook
;;              #'snr-interaction-mode)
;;
;;  for editor features to be enabled when visiting a buffer with
;;  janet code in it.

;;;;; Automatic

;; TODO :)

;;;; Usage

;; 0. Get a spork netrepl server running (e.g. from a project's root
;;    directory [1]).  One way to do this is via spork's janet-netrepl
;;    command:
;;
;;     cd project && janet-netrepl

;; 1. Open a Janet file from the project in Emacs.

;; 2. Start an interactive REPL for Janet by:
;;
;;      M-x snr
;;
;;    A repl buffer (*snr-repl*) should appear.

;; 3. For editor features, in a relevant buffer with a Janet source file:
;;
;;      M-x snr-interaction-mode
;;
;;    There should be a Snr menu containing some convenience commands:
;;
;;      Send buffer
;;      Send expression at point
;;      Send region
;;
;;      Insert last output
;;
;;      Start REPL
;;      Switch to REPL

;; [1] Paths in import, use, and require forms get resolved via the
;;     directory janet-netrepl is started in, so adjust where janet-netrepl
;;     is started accordingly.

;;;;; Acknowledgments

;; Thanks to those involved in:
;;
;;   emacs
;;   janet
;;
;; and transitively involved folks too ;)

;;; License:

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

;;; Code:

;;;; Requirements

(require 'comint)
(require 'subr-x)

;;;; The Rest

(defgroup snr nil
  "Spork Netrepl REPL"
  :prefix "snr-"
  :group 'applications)

(defvar snr-repl-buffer-name "*snr-repl*"
  "Name of repl buffer.")

(defun snr-send-code (code-str)
  "Send CODE-STR to Janet repl."
  (interactive "sCode: ")
  (let ((here (point))
        (original-buffer (current-buffer))
        (repl-buffer (get-buffer snr-repl-buffer-name)))
    (if (not repl-buffer)
        (message (format "%s is missing..." snr-repl-buffer-name))
      ;; switch to snr buffer to prepare for appending
      (set-buffer repl-buffer)
      (goto-char (point-max))
      (insert code-str)
      (comint-send-input)
      (set-buffer original-buffer)
      (if (eq original-buffer repl-buffer)
          (goto-char (point-max))
        (goto-char here)))))

(defun snr-send-region (start end)
  "Send a region bounded by START and END."
  (interactive "r")
  (let ((here (point))
        (original-buffer (current-buffer))
        (repl-buffer (get-buffer snr-repl-buffer-name)))
    (if (not repl-buffer)
        (message (format "%s is missing..." snr-repl-buffer-name))
      ;; switch to snr buffer to prepare for appending
      (set-buffer repl-buffer)
      (goto-char (point-max))
      ;; switch back
      (set-buffer original-buffer)
      (append-to-buffer repl-buffer start end)
      (set-buffer repl-buffer)
      (comint-send-input)
      (set-buffer original-buffer)
      (goto-char here))))

(defun snr-send-buffer ()
  "Send buffer content."
  (interactive)
  (snr-send-region (point-min) (point-max)))

;; XXX: better to support more than just paren expressions -- e.g. symbols
(defvar snr--helper-path
  (expand-file-name
   (concat (expand-file-name
            (file-name-directory (or load-file-name
                                     buffer-file-name)))
           "snr/last-expression.janet"))
  "Path to helper program to determine last paren expression.")

(defvar snr--proxy-path
  (expand-file-name
   (concat (expand-file-name
            (file-name-directory (or load-file-name
                                     buffer-file-name)))
           "netrepl-proxy.janet"))
  "Path to netrepl proxy.")

(defvar snr--debug-output
  nil
  "If non-nil, output debug info to *Messages* buffer.")

(defvar snr--temp-buffers
  '()
  "List of buffers to clean up before executing `snr--helper'.")

(defun snr--helper (start end)
  "Determine last paren expression by asking Janet.

A region bounded by START and END is sent to a helper program."
  (interactive "r")
  (condition-case err
      (let ((temp-buffer (generate-new-buffer "*snr-helper*"))
            (result nil))
        (dolist (old-buffer snr--temp-buffers)
          (kill-buffer old-buffer))
        (add-to-list 'snr--temp-buffers temp-buffer)
        (save-excursion
          (when snr--debug-output
            (message "region: %S"
                     (buffer-substring-no-properties start end)))
          (call-process-region start end
                               "janet"
                               ;; https://emacs.stackexchange.com/a/54353
                               nil `(,temp-buffer nil) nil
                               snr--helper-path)
          (set-buffer temp-buffer)
          (setq result
                (buffer-substring-no-properties (point-min) (point-max)))
          (when snr--debug-output
            (message "snr: %S" result))
          ;; https://emacs.stackexchange.com/a/14599
          (if (string-match "^[\0-\377[:nonascii:]]*" result)
              (match-string 0 result)
            (message "Unexpected result - source ok? <<%s>>" result)
            nil)))
    (error
     (message "Error: %s %s" (car err) (cdr err)))))

(defun snr--start-of-top-level-char-p (char)
  "Return non-nil if CHAR can start a top level container construct.

Supported top level container constructs include:

  * paren tuple            ()
  * quoted constructs      \\='() ~()

Note that constructs such as numbers, keywords, and symbols are excluded."
  (member char '(?\( ?\~ ?\')))

(defun snr--column-zero-target-backward ()
  "Move backward to the closest column zero target.

Does not move point if there is no such construct.

See `snr--start-of-top-level-char-p' for which characters determine
a column zero target."
  (when (not (bobp))             ; do nothing if at beginning of buffer
    (let ((pos (point)))
      ;; only consider positions before the starting point
      (if (bolp)
          (forward-line -1)
        (beginning-of-line))
      (if (snr--start-of-top-level-char-p (char-after (point)))
          (setq pos (point))
        (let ((done nil))
          (while (not done)
            (forward-line -1)
            (cond ((snr--start-of-top-level-char-p
                    (char-after (point)))
                   (setq pos (point))
                   (setq done t))
                  ((bobp)
                   (setq done t))))))
      (goto-char pos))))

(defun snr-send-expression-at-point ()
  "Send expression at point."
  (interactive)
  (save-excursion
    (let ((end (point)))
      ;; XXX: if skipping backward over comments was easy, that might be
      ;;      even nicer
      ;;(skip-chars-backward " \t\n")
      ;; XXX: cheap version
      (save-excursion
        (skip-chars-backward " \t\n")
        (beginning-of-line)
        (when (looking-at "[ \t]*#")
          (setq end (point))))
      (when-let ((beg (snr--column-zero-target-backward)))
        (when-let ((code (snr--helper beg end)))
          (snr-send-code code))))))

(defun snr-switch-to-repl ()
  "Switch to the repl buffer named by `snr-repl-buffer-name`."
  (interactive)
  (pop-to-buffer snr-repl-buffer-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar snr-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-b" 'snr-send-buffer)
    (define-key map "\C-x\C-e" 'snr-send-expression-at-point)
    (define-key map "\C-c\C-r" 'snr-send-region)
    (define-key map "\C-c\C-z" 'snr-switch-to-repl)
    (easy-menu-define snr-interaction-mode-map map
      "SNR Interaction Mode Menu"
      '("Snr"
        ["Send buffer" snr-send-buffer t]
        ["Send expression at point" snr-send-expression-at-point t]
        ["Send region" snr-send-region t]
        "--"
        ["Start REPL" snr t]
        ["Switch to REPL" snr-switch-to-repl t]))
    map)
  "SNR interaction mode map.")

(defvar snr-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (easy-menu-define snr-mode-map map
      "A Janet REPL Mode Menu"
      '("Snr"
        ["Switch to other window" other-window t]))
    map)
  "Snr mode map.")

(define-derived-mode snr-mode comint-mode "A Janet REPL"
  "Major mode for snr.

\\{snr-mode-map}"

  :syntax-table lisp-mode-syntax-table
  (setq comint-prompt-read-only t)
  (setq mode-line-process '(":%s")))

;;;###autoload
(define-minor-mode snr-interaction-mode
  "Minor mode for snr interaction from a lisp buffer.

The following keys are available in `snr-interaction-mode`:

\\{snr-interaction-mode}"
  :init-value nil
  :lighter " snr"
  :keymap snr-interaction-mode-map)

;;;###autoload
(defun snr ()
  "Start snr."
  (interactive)
  (let ((start-buffer (current-buffer)))
    (unless
        ;;(ignore-errors ;; XXX: uncomment at some point...
        (with-current-buffer (get-buffer-create snr-repl-buffer-name)
          (prog1
              (make-comint-in-buffer "snr" snr-repl-buffer-name
                                     snr--proxy-path)
            (goto-char (point-max))
            (snr-mode)
            (pop-to-buffer (current-buffer))
            (goto-char (point-max))
            (pop-to-buffer start-buffer)))
      (message "Failed to connect to netrepl-proxy"))))

(provide 'snr)

;;; snr.el ends here

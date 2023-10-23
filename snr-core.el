;;; snr-core.el --- core for snr -*- lexical-binding: t; -*-

;; Author: sogaiu
;; Version: 20201226
;; Package-Requires: ((emacs "26.2"))
;; Keywords: janet repl

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;;; Acknowledgments:

;; Ruin0x11's janet-mode

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

(defvar snr-prompt "repl:[[:digit:]]+:> "
  "Regular expression to match Janet repl prompt.")

(defun snr-get-process ()
  "Return Janet process for repl buffer."
  (get-buffer-process snr-repl-buffer-name))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun snr-comint-end-of-output-p (output)
  "Return non-nil if OUTPUT ends with input prompt."
  (string-match (concat "\r?\n?" ; \r for macos
                        snr-prompt
                        (rx eos))
                output))

(defun snr-send-string (string &optional process)
  "Send STRING to Janet PROCESS."
  (interactive
   (list (read-string "Eval: ") nil t))
  (let ((process (or process (snr-get-process))))
    (comint-send-string process string)
    (when (or (not (string-match "\n\\'" string))
              (string-match "\n[ \t].*\n?\\'" string))
      (comint-send-string process "\n"))))

(provide 'snr-core)

;;; snr-core.el ends here

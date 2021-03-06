;;; mailq.el --- a major mode for `mailq' interaction
;;
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/el-get.el
;; Version: 0.10
;; Created: 2010-06-17
;; Keywords: emacs mailq mode-line
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.

(require 'simple) ;; kill-whole-line

(defgroup mailq nil
  "Run mailq."
  :group 'comm)

(defvar mailq-executable
  (if (file-executable-p "/sw/bin/mailq")
      "/sw/bin/mailq"
    (executable-find "mailq"))
  "Where to find the `mailq' utility")

(defvar mailq-sendmail-executable
  (if (file-executable-p "/sw/sbin/sendmail")
      "/sw/sbin/sendmail"
    (executable-find "sendmail"))
  "Where to find the `sendmail' utility")

(defvar mailq-modeline-display nil)

(defvar mailq-update-timer nil
  "The timer used to update `mailq-mode-string'.")

(defcustom mailq-symbol-empty "✔"
  "symbol to display in the mode line for empty mailq"
  :group 'mailq)

(defcustom mailq-symbol-not-empty "✘"
  "symbol to display in the mode line for not empty mailq"
  :group 'mailq)

(defvar mailq-mode-line-string nil
  "symbol displayed in the mode-line")

(defcustom mailq-enable-mode-line-p '(member major-mode
					     '(gnus-group-mode
					       gnus-summary-mode))
  "Whether enable mailq mode line status display.
This form is evaluated and its return value determines if the
mailq status should be displayed in the mode line."
  :group 'mailq)

(defun mailq-mode-line ()
  "Return a string to display in mode line."
  (when (eval mailq-enable-mode-line-p)
    mailq-mode-line-string))

(defun mailq-update-modeline ()
  "Update `mailq-mode-line-string"
  (interactive)
  (let* ((queue (process-lines mailq-executable))
	 (empty (string= (car queue) "Mail queue is empty")))
    (unless empty
      (message "mailq-update-modeline: Mail queue is not empty"))
    (setq mailq-mode-line-string
	  (format "Q %s"
		  (if empty mailq-symbol-empty mailq-symbol-not-empty)))))

(defun mailq-modeline-display ()
  "Toggle display of the mailq status (empty or not) in the modeline"
  (interactive)

  ;; always cancel the timer
  (when mailq-update-timer
    (cancel-timer mailq-update-timer)
    (setq mailq-update-timer nil))

  ;; toggle the setting
  (setq mailq-modeline-display (not mailq-modeline-display))

  (when mailq-modeline-display
    (setq mailq-update-timer
	  (run-at-time nil 60 'mailq-update-modeline)))

  (add-to-list 'global-mode-string '(:eval (mailq-mode-line)) t))

(defun mailq-propertize ()
  "Propertize current buffer, expected to contain output from the `mailq' command."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(let ((bol (line-beginning-position))
	      (eol (line-end-position)))
	  (cond
	   ((looking-at "password")
	    (kill-whole-line))

	   ((looking-at "-Queue ID")
	    (put-text-property bol eol 'face font-lock-string-face))

	   ((looking-at "^--")
	    (put-text-property bol eol 'face font-lock-comment-face))

	   ((looking-at " *(.*)") 
	    (put-text-property bol eol 'face font-lock-warning-face))

	   ((looking-at "[0-9A-F]+")
	    (save-excursion
	      (let* ((end-of-id   (1- (re-search-forward "[*! ]")))
		     (queue-id    (buffer-substring-no-properties bol end-of-id))
		     (end-of-item (1- (re-search-forward "^$")))
		     (dummy       (goto-char bol))
		     (domain-pt   (re-search-forward "@\\([^ ]+\\)$" end-of-item))
		     (domain      (match-string 1))
		     (b-o-domain  (match-beginning 1))
		     (e-o-domain  (match-end 1))
		     (b-o-lpart   (re-search-backward " " (line-beginning-position))))
		(put-text-property bol eol 'id queue-id)
		(put-text-property bol eol 'site domain)
		(put-text-property bol end-of-id 'face font-lock-constant-face)
		(put-text-property b-o-domain e-o-domain 'face font-lock-keyword-face)
		(put-text-property b-o-lpart b-o-domain 'face font-lock-preprocessor-face))))))

	(forward-line)))))

(defun mailq-filter (proc string)
  "Filter for `mailq', with sudo password prompting support"
  (with-current-buffer (process-buffer proc)
    (unless (boundp 'mailq-filter-filter-pos)
      (make-local-variable 'mailq-filter-filter-pos)
      (setq mailq-filter-filter-pos (point-min)))

    (save-excursion
      (let ((inhibit-read-only t))
	(goto-char (point-max))
	(insert string)
	;; redirect the subprocess sudo prompt to the user face, and answer it
	(goto-char mailq-filter-filter-pos)
	(while (re-search-forward "password" nil t)
	  (let* ((prompt (thing-at-point 'line))
		 (pass   (read-passwd prompt)))
	    (process-send-string proc (concat pass "\n"))))
	(setq mailq-filter-filter-pos (point-max))))))

(defun mailq-sentinel (proc change)
  "Switch to the *mailq* buffer once the command is done"
  ;; async processing means we get there at the end
  ;; of the subprocess --- upon other state change
  (when (eq (process-status proc) 'exit)
    (with-current-buffer (process-buffer proc)
      (goto-char (point-min))
      (if (looking-at "Mail queue is empty")
      	  (message "Mail queue is empty.")
      	(mailq-mode)
      	(set-window-buffer (selected-window) (process-buffer proc))))))

(defun mailq ()
  "run `mailq' and complain loudly when there's staged message"
  (interactive)
  (let* ((name  "*mailq*")
	 (dummy (when (get-buffer name)
		  (with-current-buffer name
		    (let ((inhibit-read-only t)) (erase-buffer)))))
	 (process-connection-type nil)
	 (proc  (start-process name name mailq-executable)))
    (set-process-filter proc 'mailq-filter)
    (set-process-sentinel proc 'mailq-sentinel)))

(defun mailq-sendmail-sentinel (proc change)
  "Once `sendmail' is done flushing, rerun `mailq'"
  (when (eq (process-status proc) 'exit)
    (kill-buffer (process-buffer proc))
    (mailq)))

(defun mailq-sendmail (option &optional arg)
  "use the `sendmail' command to flush some mails"
  (interactive)
  (let* ((option (cond ((eq 'flush option) "-q")
		       ((eq 'id option)    "-qI")
		       ((eq 'site option)  "-qR")))
	 (name   (format "*sendmail %s*" option))
	 (process-connection-type nil)
	 (start-process-args-1
	  `(,name ,name ;; both the buffer and the process names
		  ,(executable-find "sudo") "-S" 
		  ,mailq-sendmail-executable ,option))
	 (start-process-args (if arg 
				 (append start-process-args-1 (list arg))
			       start-process-args-1))
	 (dummy (message "%s" (mapconcat 'identity (cddr start-process-args) " ")))
	 (proc  (apply 'start-process start-process-args)))
    ;; filter for sudo
    (set-process-filter proc 'mailq-filter)
    (set-process-sentinel proc 'mailq-sendmail-sentinel)))

(defun mailq-mode-flush ()
  (interactive)
  (mailq-sendmail 'flush))

(defun mailq-mode-refresh ()
  (interactive)
  (mailq))

(defun mailq-mode-next-id ()
  (interactive)
  (forward-line 1)
  (let ((next (next-single-property-change (point) 'id)))
    (if (not next)
	(forward-line -1)
      (goto-char next)
      (beginning-of-line))))

(defun mailq-mode-previous-id ()
  (interactive)
  (forward-line -1)
  (let ((previous (previous-single-property-change (point) 'id)))
    (if (not previous)
	(forward-line 1)
      (goto-char previous)
      (beginning-of-line))))

(defun mailq-mode-kill-id ()
  (interactive)
  (let ((id (get-text-property (point) 'id)))
    (when id
      (message id)
      (kill-new id))))

(defun mailq-mode-kill-site ()
  (interactive)
  (let ((site (get-text-property (point) 'site)))
    (when site
      (message site)
      (kill-new site))))

(defun mailq-mode-deliver-id ()
  (interactive)
  (let ((id (get-text-property (point) 'id)))
    (when id
      (message id)
      (mailq-sendmail 'id id))))

(defun mailq-mode-deliver-site ()
  (interactive)
  (let ((site (get-text-property (point) 'site)))
    (when site
      (message site)
      (mailq-sendmail 'site site))))

(defvar mailq-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'bury-buffer)
    (define-key map (kbd "f") 'mailq-mode-flush)
    (define-key map (kbd "g") 'mailq-mode-refresh)
    (define-key map (kbd "p") 'mailq-mode-previous-id)
    (define-key map (kbd "n") 'mailq-mode-next-id)
    (define-key map (kbd "w") 'mailq-mode-kill-id)
    (define-key map (kbd "i") 'mailq-mode-kill-id)
    (define-key map (kbd "s") 'mailq-mode-kill-site)
    (define-key map (kbd "I") 'mailq-mode-deliver-id)
    (define-key map (kbd "S") 'mailq-mode-deliver-site)
    map)
  "Keymap for postqueue -p mode")

(define-derived-mode mailq-mode fundamental-mode "MailQ"
  "A major mode for postqueue interaction."
  :group 'comm
  (mailq-propertize)
  (mailq-mode-next-id)
  (setq buffer-read-only t)
  (setq buffer-undo-list t))

(provide 'mailq)

;;; mailq.el ends here

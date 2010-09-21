;;; mailq.el --- a major mode for `mailq' interaction
;;
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/el-get.el
;; Version: 0.9
;; Created: 2010-06-17
;; Keywords: emacs package elisp install elpa git git-svn bzr cvs apt-get fink http http-tar emacswiki
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.

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

(defun mailq-filter (proc string)
  "Filter for `mailq', with sudo password prompting support"
  (message "mailq-filter: %S" string)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      ;; redirect the subprocess sudo prompt to the user face
      (when (string-match "password" string)
	(process-send-string proc (concat (read-passwd string) "\n")))

      (let ((inhibit-read-only t))
	(goto-char (point-max))
	(dolist (line (split-string string "\n"))
	  (cond ((string-match "^[ 	\n]*$" line) t)

		((string-match "password" line) t)

		((string-match "Mail queue is empty" line) 
		 ;; the sentinel will need to find this
		 (insert line))

		((string-match "Queue ID" line) 
		 (insert (propertize line 'face font-lock-string-face) "\n"))

		((string-match "^--" line)
		 (insert (propertize line 'face font-lock-comment-face) "\n"))

		((string-match "^ " line)
		 (insert (propertize line 'face font-lock-string-face) "\n"))

		(t
		 ;; first line of message, queue-id size, time, sender
		 (let* ((split  (split-string line))
			(id     (car split))
			(size   (cadr split))
			(mail   (car (last split)))
			(lpart  (car (split-string mail "@")))
			(domain (cadr (split-string mail "@")))
			(time   (mapconcat 
				 'identity (cddr (butlast split)) " ")))
		   (insert
		    (propertize id
				'face font-lock-constant-face
				'id id
				'site domain) " "
				(format "%8s" size) " "
				time " "
				(propertize lpart  
					    'face font-lock-preprocessor-face) "@"
					    (propertize domain
							'face font-lock-keyword-face) "\n")))))))))

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
	 (proc  (start-process
		 name name ;; both the process and buffer name
		 (executable-find "sudo") "-S" mailq-executable)))
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
	 (name  (format "*sendmail %s*" option))
	 (process-connection-type nil)
	 (proc
	  (start-process name name ;; both the process and buffer name
			 (executable-find "sudo") "-S" 
			 mailq-sendmail-executable (cons option (list arg)))))
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
  (setq buffer-read-only t)
  (setq buffer-undo-list t))

(provide 'mailq)

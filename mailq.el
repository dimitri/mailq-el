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

(require 'simple) ;; kill-whole-line

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
	 (optarg (mapconcat 'identity (cons option (list arg)) " "))
	 (name   (format "*sendmail %s*" optarg))
	 (process-connection-type nil)
	 (proc
	  (start-process name name ;; both the process and buffer name
			 (executable-find "sudo") "-S" mailq-sendmail-executable optarg)))
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

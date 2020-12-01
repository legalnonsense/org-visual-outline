;;; org-visual-outline.el --- A prettier outline tree -*- lexical-binding: t; -*-

(defvar org-visual-outline-initial-settings nil)

;;(defvar org-visual-outline--heading-re "^\\(?1:\\**\\) ")

(setq org-visual-outline--font-lock-keyword
      `((,org-heading-regexp
	 (1 (progn (list 'face 'org-visual-outline-face
 			 'display (org-visual-outline--create-heading-string)))))))
;;  "Font-lock keyword to fontify heading stars.")

(defface org-visual-outline-face '((t (:foreground "gray")))
  "face")
(defface org-visual-outline-pipe-face '((t (:foreground "gray" :background "gray" :height .1)))
  "adsf")
(defface org-visual-outline-blank-pipe-face '((t (:foreground "gray10" :background "gray10" .1)))
  "adsf")

(setq org-visual-outline--outline-chars 
      `((CHILDREN_T_FOLDED_T_BODY_T . "▶")
	(CHILDREN_T_FOLDED_T_BODY_NIL . "▷")
	(CHILDREN_T_FOLDED_NIL_BODY_T . "▼")
	(CHILDREN_T_FOLDED_NIL_BODY_NIL . "▽")
	(CHILDREN_NIL_BODY_T  . "▬")
	(CHILDREN_NIL_BODY_NIL  . "▭")
	(PIPE . ,(org-add-props "│"
		     '(face org-visual-outline-pipe-face)))
	(BLANK_PIPE . ,(org-add-props "│"
			   '(face org-visual-outline-blank-pipe-face)))
	(BLANK . "   ")))

;;; Getting outline data 

(defun org-visual-outline--has-children-p ()
  "Does the current node have any children?"
  (save-excursion (org-goto-first-child)))

(defun org-visual-outline--get-level ()
  "Get the level of the current heading"
  (org-current-level))

(defun org-visual-outline--last-sibling-p ()
  "Return nil if the current node is the last child. Otherwise, 
return the point of the next child." 
  (save-excursion 
    (not (outline-get-next-sibling))))

(defun org-visual-outline--heading-folded-p ()
  "Is the current heading folded?"
  (save-excursion
    (org-back-to-heading)
    (end-of-line)
    (outline-invisible-p)))

(defun org-visual-outline--body-p ()
  "Does the current heading have any text in its body? 
\"Body\" is defined as any text, including property drawers, 
following the heading."
  (cddr (car (org-element--parse-elements
	      (point)
	      (save-excursion (or (outline-next-heading)
				  (point-max)))
	      nil nil nil nil nil))))

;;; Creating line-prefix, wrap-prefix, display text properties

(defun org-visual-outline--create-heading-string ()
  "Create a string to be displayed in lieu of the 
headings leading starts."
  (let ((children (org-visual-outline--has-children-p))
	(folded (org-visual-outline--heading-folded-p))
	(body (org-visual-outline--body-p)))
    (concat
     ;; Prefix -- must match the plain line prefix 
     (when-let ((prefix
		 (save-excursion
		   (let ((prefixes (cl-loop
				    while (org-up-heading-safe)
				    collect
				    (concat (alist-get 'PIPE org-visual-outline--outline-chars)
					    (alist-get 'BLANK org-visual-outline--outline-chars)))))
		     (when prefixes
		       (cl-loop for prefix in (reverse prefixes)
				concat prefix))))))
       (concat
	(alist-get 'BLANK_PIPE org-visual-outline--outline-chars)
	(alist-get 'BLANK_PIPE org-visual-outline--outline-chars)
	(alist-get 'BLANK_PIPE org-visual-outline--outline-chars)
	(alist-get 'BLANK_PIPE org-visual-outline--outline-chars)
	(substring prefix 0 -1)
        (alist-get 'BLANK_PIPE org-visual-outline--outline-chars)))
     
     ;; Suffix -- i.e., the bullet
     (cond ((and children folded body)
	    (alist-get 'CHILDREN_T_FOLDED_T_BODY_T org-visual-outline--outline-chars))
	   ((and children folded (not body))
	    (alist-get 'CHILDREN_T_FOLDED_T_BODY_NIL org-visual-outline--outline-chars))
	   ((and children (not folded) body)
	    (alist-get 'CHILDREN_T_FOLDED_NIL_BODY_T org-visual-outline--outline-chars))
	   ((and children (not folded) (not body))
	    (alist-get 'CHILDREN_T_FOLDED_NIL_BODY_NIL org-visual-outline--outline-chars))
	   ((and (not children) body)
	    (alist-get 'CHILDREN_NIL_BODY_T org-visual-outline--outline-chars))
	   ((and (not children) (not body))
	    (alist-get 'CHILDREN_NIL_BODY_NIL org-visual-outline--outline-chars))
	   (t (error "You missed something."))))))

(defun org-visual-outline--create-plain-line-prefix ()
  "Create the prefix for plain line entries."
  (let ((prefixes (save-excursion
		    (cl-loop
		     while (org-up-heading-safe)
		     collect
		     (concat (alist-get 'PIPE org-visual-outline--outline-chars)
			     (alist-get 'BLANK org-visual-outline--outline-chars))))))
    (if (org-visual-outline--has-children-p)
	(push (concat
	       (alist-get 'PIPE org-visual-outline--outline-chars)
	       (alist-get 'BLANK org-visual-outline--outline-chars))
	      prefixes)
      (push (concat
	     (alist-get 'BLANK_PIPE org-visual-outline--outline-chars)
	     (alist-get 'BLANK org-visual-outline--outline-chars))
	    prefixes))
    (concat (alist-get 'BLANK_PIPE org-visual-outline--outline-chars)
	    (alist-get 'BLANK_PIPE org-visual-outline--outline-chars)
	    (alist-get 'BLANK_PIPE org-visual-outline--outline-chars)
	    (alist-get 'BLANK_PIPE org-visual-outline--outline-chars)
	    (cl-loop for prefix in (reverse prefixes)
		     concat prefix))))

;;; Refreshing display 

(defun org-visual-outline--fontify (&rest _)
  "Call `font-lock-fontify-block' and update 
org-indent properties for the relevant region. 
Arguments are ignored as this function is called
through various org-mode hooks."
  (save-excursion 
    (let ((beg (save-excursion (if (= 0 (org-current-level))
				   (point)
				 (while (org-up-heading-safe))
				 (point))))
	  (end (save-excursion (outline-end-of-subtree)
			       (point))))
      (font-lock-fontify-region beg end))))

;;; Replacements for org-indent functions

(defun org-visual-outline--set-line-properties ()
  "Set prefix properties on current line an move to next one.
This function is used in place of `org-indent-set-line-properties'."
  (let ((prefix (org-visual-outline--create-plain-line-prefix)))
    (if (org-at-heading-p)
	(forward-line)
      ;; Seems like this stuff is not necessary...
      ;; (progn 
      ;;   (add-text-properties
      ;;    (line-beginning-position) (line-beginning-position 2)
      ;;    `(wrap-prefix ,(substring prefix 0 -1) line-prefix ""))
      ;;   (forward-line))
      ;; Once we start putting properties on a non-heading line,
      ;; continue until the next heading to avoid recalculating
      ;; the prefix for every line 
      (while (and (not (org-at-heading-p))
		  (not (eobp)))
	(add-text-properties
	 (line-beginning-position) (line-beginning-position 2)
	 `(line-prefix ,prefix wrap-prefix ,prefix))
	(forward-line)))))

(defun org-visual-outline--org-indent-add-properties (beg end &optional delay)
  "When using org-visual-outline-mode, call this function 
instead of  `org-indent-add-properties'."
  (save-match-data
    (goto-char beg)
    (beginning-of-line)
    ;; Initialize prefix at BEG, according to current entry's level.
    (let* ((case-fold-search t)
	   (limited-re (org-get-limited-outline-regexp))
	   (level (or (org-current-level) 0))
	   (time-limit (and delay (org-time-add nil delay))))
      ;; For each line, set `line-prefix' and `wrap-prefix'
      ;; properties depending on the type of line (headline, inline
      ;; task, item or other).
      (with-silent-modifications
	(while (and (<= (point) end) (not (eobp)))
	  (cond
	   ;; When in asynchronous mode, check if interrupt is
	   ;; required.
	   ((and delay (input-pending-p)) (throw 'interrupt (point)))
	   ;; In asynchronous mode, take a break of
	   ;; `org-indent-agent-resume-delay' every DELAY to avoid
	   ;; blocking any other idle timer or process output.
	   ((and delay (org-time-less-p time-limit nil))
	    (setq org-indent-agent-resume-timer
		  (run-with-idle-timer
		   (time-add (current-idle-time) org-indent-agent-resume-delay)
		   nil #'org-indent-initialize-agent))
	    (throw 'interrupt (point)))
	   ((looking-at org-outline-regexp)
	    (forward-line)
	    ;; (org-visual-outline--set-line-properties))
	   ((org-at-item-p)
	    (org-visual-outline--set-line-properties))
	   (t
	    (org-visual-outline--set-line-properties))))))))

(defun org-visual-outline--org-indent-add-properties-advice
    (func beg end &optional delay)
  "Advice around `org-indent-add-properties'.  If we using 
`org-visual-outline-mode', then change the call from 
`org-indent-add-properties' to
 `org-visual-outline--org-indent-add-properties'.  Otherwise,
do not disturb the call to `org-indent-add-properties'."
  (if org-visual-outline-mode
      (org-visual-outline--org-indent-add-properties beg end delay)
    (funcall func beg end delay)))

;;; Minor mode

(defcustom org-visual-outline-mode-hooks '(org-cycle-hook
					   org-after-demote-entry-hook
					   org-after-promote-entry-hook
					   org-insert-heading-hook)
  "List of hooks which update the display.")

(define-minor-mode org-visual-outline-mode
  "Display orgmode trees."
  nil
  " visual"
  nil
  (if org-visual-outline-mode
      (progn
	;; Turn off conflicting modes
	(when (fboundp 'org-bullets-mode)
	  (org-bullets-mode -1))
	(when (fboundp 'org-superstar-mode)
	  (org-superstar-mode -1))

	;; Prepare org-indent mode
	(org-indent-mode -1)
	(setq-local org-indent-mode-turns-off-org-adapt-indentation t
		    org-indent-mode-turns-on-hiding-stars nil
		    org-hide-leading-stars nil)
	(advice-add 'org-indent-add-properties :around
		    #'org-visual-outline--org-indent-add-properties-advice)
	(org-indent-mode 1)
	
	;; Prepare font lock 
	(cl-pushnew 'display font-lock-extra-managed-props)
	(font-lock-add-keywords nil org-visual-outline--font-lock-keyword)
	(mapc org-visual-outline-mode-hooks
	      (lambda (hook) 
		(add-hook hook #'org-visual-outline--fontify t t)))
	(font-lock-fontify-buffer))

    ;; Clean up org-indent
    (advice-remove 'org-indent-add-properties
		   #'org-visual-outline--org-indent-add-properties-advice)
    ;; Clean up font lock and hooks
    (font-lock-remove-keywords nil org-visual-outline--font-lock-keyword)
    (mapc org-visual-outline-mode-hooks
	  (lambda (hook) 
	    (remove-hook hook #'org-visual-outline--fontify t)))
    (font-lock-fontify-buffer)))

(provide 'org-visual-outline)

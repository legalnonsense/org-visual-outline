;;; org-visual-outline.el --- Visualized outline tree -*- lexical-binding: t; -*-



(defface org-visual-outline-face '((t (:foreground "gray")))
  "Heading face")

(defface org-visual-outline-pipe-face `((t (:foreground ,(face-foreground 'org-visual-outline-face)
							:background ,(face-foreground 'org-visual-outline-face)
							:height .1)))
  "Vertical bar face. Should match `org-visual-line-face' except with a different height. 
(.1 recommended).")

(defface org-visual-outline-blank-pipe-face `((t (:foreground ,(face-background 'default)
							      :background ,(face-background 'default)
							      :height .1)))
  "Blank vertical bar face. Should match the background, and have the same height as 
`org-visaul-outline-pipe-face'.")



(setq org-visual-outline--heading-re "^\\(?1:\\**\\) ")
;;      "Outline heading regexp")

(setq org-visual-outline--font-lock-keyword
      `((,org-visual-outline--heading-re
	 (1 (list 'face 'org-visual-outline-face
 		  'display (org-visual-outline--create-heading-string))))))
;;      "Font-lock keyword to fontify heading stars.")

(setq org-visual-outline-refresh-hooks '(org-cycle-hook
					 org-after-demote-entry-hook
					 org-after-promote-entry-hook
					 org-insert-heading-hook))
;;      "List of hooks which update the display.")

(setq org-visual-outline-refresh-funcs '(org-show-children
					 org-show-all
					 org-show-entry
					 org-show-subtree
					 org-show-siblings
					 org-show-context))
;;      "List of functions which trigger updating the display.")

(setq org-visual-outline--outline-chars 
      `((CHILDREN-T-FOLDED-T-BODY-T . "▶")
	(CHILDREN-T-FOLDED-T-BODY-NIL . "▷")
	(CHILDREN-T-FOLDED-NIL-BODY-T . "▼")
	(CHILDREN-T-FOLDED-NIL-BODY-NIL . "▽")
	(CHILDREN-NIL-BODY-T  . "▬")
	(CHILDREN-NIL-BODY-NIL  . "▭")
	(DASH . "─")
	(PIPE . ,(org-add-props "│"
		     '(face org-visual-outline-pipe-face)))
	(BLANK-PIPE . ,(org-add-props "│"
			   '(face org-visual-outline-blank-pipe-face)))
	(BLANK . "   ")))
      ;; "outline characters")

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
following the heading and before the next heading."
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
	(alist-get 'BLANK-PIPE org-visual-outline--outline-chars)
	(alist-get 'BLANK-PIPE org-visual-outline--outline-chars)
	(alist-get 'BLANK-PIPE org-visual-outline--outline-chars)
	(alist-get 'BLANK-PIPE org-visual-outline--outline-chars)
	(substring prefix 0 -1)
	;; (when (not (= (org-current-level) 0))
	;;   "─")
        (alist-get 'BLANK-PIPE org-visual-outline--outline-chars)))
     
     ;; Suffix -- i.e., the bullet
     (cond ((and children folded body)
	    (alist-get 'CHILDREN-T-FOLDED-T-BODY-T org-visual-outline--outline-chars))
	   ((and children folded (not body))
	    (alist-get 'CHILDREN-T-FOLDED-T-BODY-NIL org-visual-outline--outline-chars))
	   ((and children (not folded) body)
	    (alist-get 'CHILDREN-T-FOLDED-NIL-BODY-T org-visual-outline--outline-chars))
	   ((and children (not folded) (not body))
	    (alist-get 'CHILDREN-T-FOLDED-NIL-BODY-NIL org-visual-outline--outline-chars))
	   ((and (not children) body)
	    (alist-get 'CHILDREN-NIL-BODY-T org-visual-outline--outline-chars))
	   ((and (not children) (not body))
	    (alist-get 'CHILDREN-NIL-BODY-NIL org-visual-outline--outline-chars))
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
	     (alist-get 'BLANK-PIPE org-visual-outline--outline-chars)
	     (alist-get 'BLANK org-visual-outline--outline-chars))
	    prefixes))
    (concat (alist-get 'BLANK-PIPE org-visual-outline--outline-chars)
	    (alist-get 'BLANK-PIPE org-visual-outline--outline-chars)
	    (alist-get 'BLANK-PIPE org-visual-outline--outline-chars)
	    (alist-get 'BLANK-PIPE org-visual-outline--outline-chars)
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
	(progn 
	  (add-text-properties
	   (line-beginning-position) (line-beginning-position 2)
	   `(wrap-prefix ,(substring prefix 0 -1)
			 line-prefix ""))
	  (forward-line))
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
"Advice for `org-indent-add-properties' to change the call from 
`org-indent-set-line-properties' to `org-visual-outline--set-line-properties'."
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
	 ;; Headline or inline task.
	 ;; ((looking-at org-outline-regexp)
	 ;;    (org-visual-outline--set-line-properties))
	 ;; ;; List item: `wrap-prefix' is set where body starts.
	 ;; ;; TODO: Fix item identation
	 ;; ((org-at-item-p)
	 ;;  (org-visual-outline--set-line-properties))
	 ;; ;; Regular line.
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

(defun org-visual-outline--refresh-advice (&rest args)
  "Advice added :after functions listed in
 `org-visual-outline-refresh-funcs'.
The effect is that any time these functions are called, 
the refresh function is called." 
  (when org-visual-outline-mode
    (org-visual-outline--fontify)))

;;; Minor mode

(define-minor-mode org-visual-outline-mode
  "Display orgmode trees."
  nil
  " visual"
  nil
  (if org-visual-outline-mode
      ;; Enabling: 
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
	;;; Keywords:
	(cl-pushnew 'display font-lock-extra-managed-props)
	(font-lock-add-keywords nil org-visual-outline--font-lock-keyword)
	;;; Hooks:
	(mapc (lambda (hook) 
		(add-hook hook #'org-visual-outline--fontify t t)) 
	      org-visual-outline-refresh-hooks)

	;; TODO
	;;; Advice
	(mapc
	 (lambda (func)
	   (advice-add func :after
		       #'org-visual-outline--refresh-advice))
	 org-visual-outline-refresh-funcs)
	
	(font-lock-fontify-buffer))
    
    ;; Disabling: 
    ;; Clean up org-indent
    (advice-remove 'org-indent-add-properties
		   #'org-visual-outline--org-indent-add-properties-advice)

    ;; Clean up font lock:

    ;; Keywords:
    (font-lock-remove-keywords nil org-visual-outline--font-lock-keyword)
    ;; Hooks:
    (mapc (lambda (hook) 
	    (remove-hook hook #'org-visual-outline--fontify t))
	  org-visual-outline-refresh-hooks)
    ;; Advice:

    ;; TODO
    (mapc
     (lambda (func)
       (advice-remove func #'org-visual-outline--fontify))
     org-visual-outline-refresh-funcs)
    
    (font-lock-fontify-buffer)))

(provide 'org-visual-outline)


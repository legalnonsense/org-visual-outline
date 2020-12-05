;;; org-visual-outline.el --- Visualized outline tree -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jeff Filipovits

;; Author: Jeff Filipovits <jrfilipovits@gmail.com>
;; Url: https://github.com/legalnonsense/org-visual-outline
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (org "9.0")
;;
;; Keywords: Org, outline

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Replace leading stars with icons indicating whether
;; a heading is folded and whether it has text in its
;; body.  Provide vertical lines in the left margin
;; indicating the depth of the outline. 

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

;;;; Installation and usage

;; Put this file in your load-path. 
;; (require 'org-visual-outline)
;; (org-visual-outline-mode)

;;;; Tips

;; You can customize settings in the `org-visual-outline' group.

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'org)
(require 'org-element)
(require 'org-indent)

;;;; Faces

(defface org-visual-outline-face '((t (:foreground "gray")))
  "Heading face"
  :group 'org-visual-outline)

(defface org-visual-outline-pipe-face `((t (:foreground ,(face-foreground 'org-visual-outline-face)
							:background ,(face-foreground 'org-visual-outline-face)
							:height .1)))
  "Vertical bar face. Recommend a height property of .1."
  :group 'org-visual-outline)

(defface org-visual-outline-blank-pipe-face `((t (:foreground ,(face-background 'default)
							      :background ,(face-background 'default)
							      :height .1)))
  "Blank vertical bar face. Should match the background; must have the same height as 
`org-visual-outline-pipe-face'."
  :group 'org-visual-outline)

;;;; Customization 

(defcustom org-visual-outline-show-lines t
  "Show vertical lines to indicate outline depth?"
  :type 'boolean
  :group 'org-visual-outline)

;; TODO: Figure out the most efficient way to refresh the bullets.
;;       As it stands, this is overkill.  See `org-visual-outline--fontify'
;;       functions. 
(defcustom org-visual-outline-refresh-hooks '(org-cycle-hook
					      org-insert-heading-hook
					      org-after-demote-entry-hook
					      org-after-promote-entry-hook)
  "List of hooks which update the display."
  :type 'list
  :group 'org-visual-outline)

;; TODO: See above. 
(defcustom org-visual-outline-refresh-funcs '(org-show-children
					      org-show-all
					      org-show-entry
					      org-promote-subtree
					      org-demote-subtree
					      org-show-subtree
					      org-show-siblings
					      org-show-context)
  "List of functions which trigger updating the display."
  :type 'list
  :group 'org-visual-outline)

(defcustom org-visual-outline-folded-body-text-bullet "▶"
  "Bullet for a folded node with text in its body."
  :type 'string
  :group 'org-visual-outline)

(defcustom org-visual-outline-folded-no-body-text-bullet "▷"
  "Bullet for folded node with no text in its body."
  :type 'string
  :group 'org-visual-outline)

(defcustom org-visual-outline-unfolded-body-text-bullet "▼"
  "Bullet for an unfolded node with text in its body."
  :type 'string
  :group 'org-visual-outline)

(defcustom org-visual-outline-unfolded-no-body-text-bullet "▽"
  "Bullet for an unfolded node with no text in its body."
  :type 'string
  :group 'org-visual-outline)

(defcustom org-visual-outline-leaf-body-text-bullet "▬"
  "Bullet for a left node with body text."
  :type 'string
  :group 'org-visual-outline)

(defcustom org-visual-outline-left-no-body-text-bullet "▭"
  "Bullet for a leaf node with no body text."
  :type 'string
  :group 'org-visual-outline)

(defcustom org-visual-outline-fontify-func #'org-visual-outline--fontify-tree
  "Function to fontify outline headings via font-lock. Used by 
`org-visual-outline-refresh-funcs' and `org-visual-outline-refresh-hooks'."
  :type 'function
  :group 'org-visual-outline)

;;;; Constants

(defconst org-visual-outline-pipe "│"
  "Pipe character.  (For some reason a space does not seem to work.)")
(put-text-property 0 1
		   'face 'org-visual-outline-pipe-face
		   org-visual-outline-pipe)

(defconst org-visual-outline-blank-pipe "│"
  "Blank character.  (For some reason a space does not seem to work.).")
(put-text-property 0 1
		   'face 'org-visual-outline-blank-pipe-face
		   org-visual-outline-blank-pipe)

(defconst org-visual-outline-span "   "
  "Span string to separate vertical lines.")

(defconst org-visual-outline-small-span (concat org-visual-outline-blank-pipe
						org-visual-outline-blank-pipe
						org-visual-outline-blank-pipe
						org-visual-outline-blank-pipe)
  "Smaller span used to align vertial lines with heading bullets.")

(defconst org-visual-outline--heading-re "^\\(?1:\\**\\) "
  "Outline heading regexp")

(defconst org-visual-outline--font-lock-keyword
  `((,org-visual-outline--heading-re
     (1 (list 'face 'org-visual-outline-face
 	      'display (org-visual-outline--create-heading-string)))))
  "Font-lock keyword to fontify heading stars.")

;;;; Functions 

;;;;; Outline position predicates

(defun org-visual-outline--has-children-p ()
  "Does the current node have any children?"
  (save-excursion (org-goto-first-child)))

(defun org-visual-outline--get-level ()
  "Get the level of the current heading"
  (org-current-level))

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

;;;;; Creating prefix strings
(defun org-visual-outline--calculate-prefix ()
  "Calculate the initial prefix string."
  (when-let ((level (org-current-level)))
    (cl-loop for x from 1 to (1- level)
	     collect
	     (concat org-visual-outline-pipe
		     org-visual-outline-span))))

(defun org-visual-outline--create-heading-string ()
  "Create a string to be displayed in lieu of the 
headings leading starts."
  (let ((children (org-visual-outline--has-children-p))
	(folded (org-visual-outline--heading-folded-p))
	(body (org-visual-outline--body-p))
	(prefix (org-visual-outline--calculate-prefix)))
    ;; You might think that this prefix should be placed
    ;; by `org-indent.' For reasons I don't understand,
    ;; you are wrong.  The line-prefix for the headings needs
    ;; to be placed via font-lock to work properly.  Oddly,
    ;; `org-indent' does a good job of handling the wrap-prefix
    ;; for headlines. 
    (concat
     (when (and prefix
		org-visual-outline-show-lines)
       (concat org-visual-outline-small-span 
	       (substring 
		(cl-loop for s in (reverse prefix )
			 concat s)
		0 -1)
	       org-visual-outline-blank-pipe))
     ;; Suffix -- i.e., the bullet
     (cond ((and children folded body)
	    org-visual-outline-folded-body-text-bullet)
	   ((and children folded)
	    org-visual-outline-folded-no-body-text-bullet)
	   ((and children body)
	    org-visual-outline-unfolded-body-text-bullet)
	   (children
	    org-visual-outline-unfolded-no-body-text-bullet)
	   (body
	    org-visual-outline-leaf-body-text-bullet)
	   (t 
	    org-visual-outline-left-no-body-text-bullet)))))

(defun org-visual-outline--create-plain-line-prefix ()
  "Create the prefix for plain line entries."
  (let ((prefixes (org-visual-outline--calculate-prefix)))
    (if (org-visual-outline--has-children-p)
	(push (concat
	       org-visual-outline-pipe
	       org-visual-outline-span)
	      prefixes)
      (push (concat
	     org-visual-outline-blank-pipe
	     org-visual-outline-span)
	    prefixes))
    (concat org-visual-outline-small-span
	    (cl-loop for prefix in (reverse prefixes)
		     concat prefix))))

;;;;; Org-indent replacement functions

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
	 ;; TODO: figure out if we need to distinguish between being
	 ;; at a heading, item, or plain line. I don't think it's necessary. 
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

;;;;; Refreshing display

(defun org-visual-outline--fontify (beg end)
  "Function to fontify the leading starts from BEG to END. 
Seems efficient. From a test running on an entire buffer:

| Form                         | x faster than next | Total runtime |
|------------------------------+--------------------+---------------+
| org-visual-outline--fontify  | 1689.81            |      0.000126 |
| font-lock-fontify-region     | slowest            |      0.212215 |

For now, however, the only function that uses this is 
`org-visual-outline--fontify-tree', which is overkill for the vast
majority of folding and promoting/demoting actions.
"
  (goto-char beg)
  (while 
      (re-search-forward org-heading-regexp end t)
    (save-excursion 
      (font-lock-fontify-region (match-beginning 0)
				(match-end 0)))))

(defun org-visual-outline--fontify-tree (&rest _)
  "Fontify the entire tree from root to last leaf."
  (save-excursion
    (when-let* ((level (org-current-level))
		(beg (save-excursion (if (= 1 level)
					 (progn (beginning-of-line)
						(point))
				       (while (org-up-heading-safe))
				       (point))))
		(end (save-excursion (outline-end-of-subtree)
				     (point))))
      (org-visual-outline--fontify beg end))))

(defun org-visual-outline--fontify-heading (&rest _)
  "Fontify the current heading only."
  (save-excursion
    (when (org-back-to-heading)
      (org-visual-outline--fontify (point-at-bol)
				   (point-at-eol)))))

(defun org-visual-outline--fontify-children (&rest _)
  "Fontify current heading to last child."
  (save-excursion
    (when (org-back-to-heading)
      (let ((beg (point-at-bol))
	    (end (save-excursion 
		   (outline-end-of-subtree)
		   (point))))
	(org-visual-outline--fontify beg end)))))

(defun org-visual-outline--refresh-hook (&rest _)
  "Hook function added to hooks listed in
 `org-visual-outline-refresh-hooks'."
  (funcall org-visual-outline-fontify-func))

(defun org-visual-outline--refresh-advice (&rest _)
  "Advice added :after functions listed in
 `org-visual-outline-refresh-funcs'.
The effect is that any time these functions are called, 
the refresh function is called." 
  (when org-visual-outline-mode
    (funcall org-visual-outline-fontify-func)))

;;; Minor mode

(define-minor-mode org-visual-outline-mode
  "Display orgmode trees."
  nil
  " visual"
  nil
  (if org-visual-outline-mode
      (progn
	(when (fboundp 'org-bullets-mode)
	  (org-bullets-mode -1))
	(when (fboundp 'org-superstar-mode)
	  (org-superstar-mode -1))

	(when org-visual-outline-show-lines
	  (org-indent-mode -1)
	  (setq-local org-indent-mode-turns-off-org-adapt-indentation t
		      org-indent-mode-turns-on-hiding-stars nil
		      org-hide-leading-stars nil)
	  (advice-add 'org-indent-add-properties :around
		      #'org-visual-outline--org-indent-add-properties-advice)
	  (org-indent-mode 1))
	
	(cl-pushnew 'display font-lock-extra-managed-props)
	(font-lock-add-keywords nil org-visual-outline--font-lock-keyword)
	(mapc (lambda (hook) 
		(add-hook hook #'org-visual-outline--refresh-hook t t)) 
	      org-visual-outline-refresh-hooks)
	(mapc
	 (lambda (func)
	   (advice-add func :after
		       #'org-visual-outline--refresh-advice))
	 org-visual-outline-refresh-funcs)
	(font-lock-ensure (point-min) (point-max)))

    ;; Disabling: 
    (advice-remove 'org-indent-add-properties
		   #'org-visual-outline--org-indent-add-properties-advice)
    (font-lock-remove-keywords nil org-visual-outline--font-lock-keyword)
    (mapc (lambda (hook) 
	    (remove-hook hook #'org-visual-outline--refresh-hook t))
	  org-visual-outline-refresh-hooks)
    (mapc
     (lambda (func)
       (advice-remove func #'org-visual-outline--refresh-advice))
     org-visual-outline-refresh-funcs)
    (font-lock-ensure (point-min) (point-max))))

;;;; Footer 

(provide 'org-visual-outline)

;;; org-visual-outline.el ends here

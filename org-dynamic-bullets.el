;;; org-dynamic-bullets.el --- Visualized outline tree -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jeff Filipovits

;; Author: Jeff Filipovits <jrfilipovits@gmail.com>
;; Url: https://github.com/legalnonsense/org-dynamic-bullets
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

;; 1. Put this file in your load-path.
;; 2. (require 'org-dynamic-bullets)
;; 3. (org-dynamic-bullets-mode)

;;;; Tips

;; You can customize settings in the `org-dynamic-bullets' group.
;; This package pairs well with `org-visual-indent'. 

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

;;;; Faces

(defface org-dynamic-bullets-face '((t (:foreground "gray")))
  "Bullet face"
  :group 'org-dynamic-bullets)

;;;; Customization

(defcustom org-dynamic-bullets-show-lines nil
  "Show vertical lines to indicate outline depth?"
  :type 'boolean
  :group 'org-dynamic-bullets)

(defcustom org-dynamic-bullets-hide-ellipsis t
  "Hide the ellipsis on startup?"
  :type 'boolean
  :group 'org-dynamic-bullets)

(defcustom org-dynamic-bullets-update-triggers
  '((org-cycle-hook . org-dynamic-bullets--org-cycle-hook-func)
    (org-promote . org-dynamic-bullets--fontify-heading-and-previous-sibling)
    (org-demote . org-dynamic-bullets--fontify-heading-and-previous-sibling)
    (org-promote-subtree . org-dynamic-bullets--fontify-heading-and-previous-sibling)
    (org-demote-subtree . org-dynamic-bullets--fontify-heading-and-previous-sibling))

  ;; TODO: I do not know the most efficient way to ensure
  ;;       bullets are redrawn.  This seems to be working
  ;;       and is efficient enough that it does not slow
  ;;       me down.  There are a number of other org functions
  ;;       that could potentially be used here:
  ;; 
  ;;       org-show-all
  ;;       org-show-entry
  ;;       org-show-subtree
  ;;       org-show-context
  ;;       org-show-siblings
  ;;       org-show-children

  "Alist in the form '((hook-or-function . refresh-function)))
where refresh-function is added as a hook or advice to hook-or-function.
Refresh function should be one of the `org-dynamic-bullets--fontify' functions
below."
  :type 'alist
  :group 'org-dynamic-bullets)

(defcustom org-dynamic-bullets-folded-body-text-bullet "▶"
  "Bullet for a folded node with text in its body."
  :type 'string
  :group 'org-dynamic-bullets)

(defcustom org-dynamic-bullets-folded-no-body-text-bullet "▷"
  "Bullet for folded node with no text in its body."
  :type 'string
  :group 'org-dynamic-bullets)

(defcustom org-dynamic-bullets-unfolded-body-text-bullet "▼"
  "Bullet for an unfolded node with text in its body."
  :type 'string
  :group 'org-dynamic-bullets)

(defcustom org-dynamic-bullets-unfolded-no-body-text-bullet "▽"
  "Bullet for an unfolded node with no text in its body."
  :type 'string
  :group 'org-dynamic-bullets)

(defcustom org-dynamic-bullets-leaf-body-text-bullet "▬"
  "Bullet for a leaf node with body text."
  :type 'string
  :group 'org-dynamic-bullets)

(defcustom org-dynamic-bullets-leaf-no-body-text-bullet "▭"
  "Bullet for a leaf node with no body text."
  :type 'string
  :group 'org-dynamic-bullets)

;;;; Constants

(defconst org-dynamic-bullets--heading-re "^\\(?1:\\*+\\) "
  "Outline heading regexp.")

(defconst org-dynamic-bullets--font-lock-keyword
  `((,org-dynamic-bullets--heading-re
     (1 (list 'face 'org-dynamic-bullets-face
 	      'display (org-dynamic-bullets--create-heading-string)))))
  "Font-lock keyword to fontify heading stars.")

;;;; Minor mode

(define-minor-mode org-dynamic-bullets-mode
  "Display orgmode trees."
  nil
  " dbullets"
  nil
  (if org-dynamic-bullets-mode
      (progn
	(when (fboundp 'org-bullets-mode)
	  (org-bullets-mode -1))
	(when (fboundp 'org-superstar-mode)
	  (org-superstar-mode -1))
	(org-dynamic-bullets--add-all-hooks-and-advice)
	(cl-pushnew 'display font-lock-extra-managed-props)
	(font-lock-add-keywords nil org-dynamic-bullets--font-lock-keyword)
	(org-dynamic-bullets--fontify-buffer))
    (font-lock-remove-keywords nil org-dynamic-bullets--font-lock-keyword)
    (org-dynamic-bullets--add-all-hooks-and-advice 'remove)
    (font-lock-flush (point-min) (point-max))
    (font-lock-ensure (point-min) (point-max))))

;;;; Functions

;;;;; Outline position predicates

(defun org-dynamic-bullets--has-children-p ()
  "Does the current node have any children?"
  (save-excursion (org-goto-first-child)))

(defun org-dynamic-bullets--heading-folded-p ()
  "Is the current heading folded?"
  (save-excursion
    (org-back-to-heading)
    (end-of-line)
    (outline-invisible-p)))

(defun org-dynamic-bullets--body-p ()
  "Does the current heading have text in its body? \"Body\" is
defined as any text, including property drawers, following
the heading and before the next heading."
  (cddr (car (org-element--parse-elements
	      (point)
	      (save-excursion (or (outline-next-heading)
				  (point-max)))
	      nil nil nil nil nil))))

;;;;; Creating prefix strings

(defun org-dynamic-bullets--create-heading-string ()
  "Create a string to be displayed in lieu of the headings' leading stars."
  (let ((children (org-dynamic-bullets--has-children-p))
	(folded (org-dynamic-bullets--heading-folded-p))
	(body (org-dynamic-bullets--body-p)))
    (cond ((and children folded body)
	   org-dynamic-bullets-folded-body-text-bullet)
	  ((and children folded)
	   org-dynamic-bullets-folded-no-body-text-bullet)
	  ((and children body)
	   org-dynamic-bullets-unfolded-body-text-bullet)
	  (children
	   org-dynamic-bullets-unfolded-no-body-text-bullet)
	  (body
	   org-dynamic-bullets-leaf-body-text-bullet)
	  (t
	   org-dynamic-bullets-leaf-no-body-text-bullet))))

;;;;; Refreshing display

(defun org-dynamic-bullets--fontify (beg end)
  "Fontify only the leading stars from BEG to END.
Seems efficient compared to the hammer of `font-lock-fontify-region'.
From a test running on an entire buffer:

| Form                            | x faster than next | Total runtime |
|---------------------------------+--------------------+---------------+
| `org-dynamic-bullets--fontify'  | 1689.81            |      0.000126 |
| `font-lock-fontify-region'      | slowest            |      0.212215 |

All fontifying functions use this function as their base."
  (save-excursion
    (goto-char beg)
    (while
	(re-search-forward org-heading-regexp end t)
      (save-excursion
	(font-lock-fontify-region (match-beginning 0)
				  (match-end 0))))))

(defun org-dynamic-bullets--fontify-buffer (&rest _)
  "Fontify the entire buffer."
  (org-dynamic-bullets--fontify (point-min) (point-max)))

(defun org-dynamic-bullets--fontify-tree (&rest _)
  "Fontify the entire tree from root to last leaf."
  (when org-dynamic-bullets-mode
    (when-let* ((level (org-current-level))
		(beg (save-excursion (if (= 1 level)
					 (progn (beginning-of-line)
						(point))
				       (while (org-up-heading-safe))
				       (point))))
		(end (save-excursion (outline-end-of-subtree)
				     (point))))
      (org-dynamic-bullets--fontify beg end))))

(defun org-dynamic-bullets--fontify-heading (&rest _)
  "Fontify the current heading only."
  (when (and org-dynamic-bullets-mode
	     (org-back-to-heading))
    (org-dynamic-bullets--fontify (point-at-bol)
				 (point-at-eol))))

(defun org-dynamic-bullets--fontify-heading-and-previous-sibling (&rest _)
  "Fontify the current heading and previous sibling."
  (when org-dynamic-bullets-mode
    (let ((beg (save-excursion (or (outline-get-last-sibling)
				   (outline-previous-heading))
			       (point)))
	  (end (line-beginning-position 2)))
      (org-dynamic-bullets--fontify beg end))))

(defun org-dynamic-bullets--fontify-heading-and-parent (&rest _)
  "Fontify the current heading only."
  (when org-dynamic-bullets-mode
    (let ((beg (save-excursion (org-up-heading-safe)
			       (point)))
	  (end (save-excursion (org-next-visible-heading 1)
			       (point))))
      (org-dynamic-bullets--fontify beg end))))

(defun org-dynamic-bullets--fontify-children (&rest _)
  "Fontify current heading to last child."
  (when org-dynamic-bullets-mode
    (save-excursion
      (when (org-back-to-heading)
	(let ((beg (point-at-bol))
	      (end (save-excursion
		     (outline-end-of-subtree)
		     (point))))
	  (org-dynamic-bullets--fontify beg end))))))

(defun org-dynamic-bullets--org-cycle-hook-func (state)
  "Called after `org-cyle'."
  (pcase state
    ((or `overview
	 `contents
	 `showall)
     (org-dynamic-bullets--fontify-buffer))
    ((or `folded
	 `children
	 `subtree)
     (org-dynamic-bullets--fontify-tree))
    (_ nil)))

;;;; Hooks and advice

(defun org-dynamic-bullets--add-hook-or-advice (hook-or-func
						func
						&optional remove)
  "If HOOK-OR-FUNC is a hook, add FUNC as a local hook.
If HOOK-OF-FUNC is a function, add FUNC as advice after HOOK-OR-FUNC.
if REMOVE is non-nil, remove the hook or advice."
  (pcase hook-or-func
    ((pred (lambda (sym)
	     (s-ends-with-p "-hook" (symbol-name sym))))
     (if remove
	 (remove-hook hook-or-func func t)
       (add-hook hook-or-func func nil t)))
    ((pred (functionp))
     (if remove
	 (advice-remove hook-or-func func)
       (advice-add hook-or-func :after func)))))

(defun org-dynamic-bullets--add-all-hooks-and-advice (&optional remove)
  "Add hooks and advice to all members of
`org-dynamic-bullets-update-triggers.'"
  (cl-loop for (hook-or-func . func) in org-dynamic-bullets-update-triggers
	   when (and hook-or-func func)
	   do (org-dynamic-bullets--add-hook-or-advice
	       hook-or-func
	       func
	       remove)))

;;;; Footer

(provide 'org-dynamic-bullets)

;;; org-dynamic-bullets.el ends here
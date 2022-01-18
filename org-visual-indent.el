;;; org-visual-indent.el --- Add vertical lines to `org-indent' -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jeff Filipovits

;; Author: Jeff Filipovits <jrfilipovits@gmail.com>
;; Url: https://github.com/legalnonsense/org-dynamic-bullets
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (org "9.0"))
;;
;; Keywords: Org, outline

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provide vertical lines in the left margin indicating
;; the depth of the outline when using `org-indent'.

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
;; 2. (require 'org-visual-indent)
;; 3. (org-visual-indent-mode)

;;;; Tips

;; You can customize settings in the `org-visual-indent' group.
;; This package pairs well with `org-dynamic-bullets'.

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

;;; Code

;;;; Requirements

(require 'org-indent)

;;;; Faces

(defface org-visual-indent-pipe-face
  `((t (:foreground ,(face-foreground 'default)
		    :background ,(face-foreground 'default)
		    :height .1)))
  "Vertical bar face. Recommend a height property of .1."
  :group 'org-visual-indent)

(defface org-visual-indent-blank-pipe-face
  `((t (:foreground ,(face-background 'default)
		    :background ,(face-background 'default)
		    :height .1)))
  "Blank vertical bar face. Should match the background; 
must have the same height as `org-visual-indent-pipe-face'."
  :group 'org-visual-indent)

;;;; Customization

(defcustom org-visual-indent-span "   "
  "Span string to separate vertical lines."
  :type 'string
  :group 'org-visual-indent)

(defcustom org-visual-indent-color-indent nil
  "Individually color vertical lines. Alist in the form:
'((1 (:background \"blue\" :foreground \"blue\" :height .1))
  (2 (:background \"red\" :foreground \"red\" :height .1))
  (3 (:background \"green\" :foreground \"green\" :height .1)))
The colors will cycle through to the beginning of the list
once the max depth is reached. This overrides the color set by 
`org-visual-indent-pipe-face'.  Be sure to set the height of
the face (recommended value of .1).")

;;;; Constants

(defconst org-visual-indent-pipe " "
  "Pipe character.  (For some reason a space does not seem to work.)")

(defconst org-visual-indent-blank-pipe " "
  "Blank character.  (For some reason a space does not seem to work.).")

(defconst org-visual-indent-small-span "     "
  "Small span used to align vertial lines with heading bullets.")

;;;; Functions 

(defun org-visual-indent--calculate-prefix (level)
  "Calculate the prefix strings used by `org-indent'"
  (cl-loop for x from 1 to (1- level)
	   concat (concat
		   (if org-visual-indent-color-indent
		       (let* ((length (length org-visual-indent-color-indent))
			      (face
			       (alist-get
				(if (= (% x length) 0) length (% x length))
				org-visual-indent-color-indent)))
			 (propertize 
			  org-visual-indent-pipe
			  'face
			  face))
		     org-visual-indent-pipe)
		   org-visual-indent-span)))

(defun org-visual-indent--org-indent--compute-prefixes ()
  "Compute prefix strings for regular text and headlines.
The function stands in place of `org-indent--compute-prefixes'."
  (setq org-visual-indent--heading-line-prefixes
	(make-vector org-indent--deepest-level nil))
  (setq org-visual-indent--inlinetask-line-prefixes
	(make-vector org-indent--deepest-level nil))
  (setq org-visual-indent--text-line-prefixes
	(make-vector org-indent--deepest-level nil))
  (dotimes (n org-indent--deepest-level)
    (let ((indentation (if (<= n 1) 0
			 (* (1- org-indent-indentation-per-level)
			    (1- n)))))
      ;; Headlines line prefixes
      (let ((heading-prefix (org-visual-indent--calculate-prefix n)))
	(aset org-visual-indent--heading-line-prefixes
	      n
	      (concat (if (> n 1)
			  (concat 
			   org-visual-indent-small-span
			   (substring heading-prefix 0 -1))
			"")))
	;; Inline tasks line prefixes
	(aset org-visual-indent--inlinetask-line-prefixes
	      n
	      (cond ((<= n 1) "")
		    ((bound-and-true-p org-inlinetask-show-first-star)
		     (concat org-indent-inlinetask-first-star
			     (substring heading-prefix 1)))
		    (t heading-prefix)))
	;; Text line prefixes.
	(aset org-visual-indent--text-line-prefixes
	      n
	      (concat 
	       org-visual-indent-small-span
	       heading-prefix))))))

(defun org-visual-indent--org-indent-set-line-properties
    (level indentation &optional heading)
  "Replacement for `org-indent-set-line-properties'."
  (let* ((line (aref (pcase heading
		       (`nil org-visual-indent--text-line-prefixes)
		       (`inlinetask org-visual-indent--inlinetask-line-prefixes)
		       (_ org-visual-indent--heading-line-prefixes))
		     level)))
    (add-text-properties (line-beginning-position) (line-beginning-position 2)
			 `(line-prefix ,line
				       wrap-prefix
				       ,(if heading
					    (aref org-visual-indent--heading-line-prefixes
						  (1+ level))
					  line))))
  (forward-line))

(defun org-visual-indent--org-indent-add-properties (beg end &optional delay)
  "Replacement for `org-indent-add-properties'."
  (save-match-data
    (org-with-wide-buffer
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
	    ((looking-at org-outline-regexp)
	     (let* ((nstars (- (match-end 0) (match-beginning 0) 1))
		    (type (or (looking-at-p limited-re) 'inlinetask)))
	       (org-indent-set-line-properties nstars 0 type)
	       ;; At an headline, define new value for LEVEL.
	       (unless (eq type 'inlinetask) (setq level nstars))))
	    ;; List item: `wrap-prefix' is set where body starts.
	    ;; ((org-at-item-p)
	    ;;  (org-indent-set-line-properties
	    ;;   level
	    ;;   (current-indentation)))
	    ;; Regular line.
	    (t
	     (org-indent-set-line-properties
	      (1+ level)
	      (current-indentation)
	      ;; When adapt indentation is 'headline-data, use
	      ;; `org-visual-indent--heading-line-prefixes' for setting
	      ;; headline data indentation.
	      (and (eq org-adapt-indentation 'headline-data)
		   (or (org-at-planning-p)
		       (org-at-clock-log-p)
		       (looking-at-p org-property-start-re)
		       (looking-at-p org-property-end-re)
		       (looking-at-p org-property-re))))))))))))

(defun org-visual-indent--org-indent-add-properties-advice
    (func beg end &optional delay)
  "Replacement for `org-indent-add-properties'."
  (if org-visual-indent-mode
      (org-visual-indent--org-indent-add-properties beg end delay)
    (funcall func beg end delay)))

(defun org-visual-indent--org-indent--compute-prefixes-advice (func)
  "Advice around `org-indent--compute-prefixes'."
  (if org-visual-indent-mode
      (org-visual-indent--org-indent--compute-prefixes)
    (funcall func)))

(defun org-visual-indent--org-indent-set-line-properties-advice
    (func level indentation &optional heading)
  "Advice around `org-indent-set-line-properties'."
  (if org-visual-indent-mode
      (org-visual-indent--org-indent-set-line-properties
       level indentation heading)
    (funcall func level indentation heading)))

;;;###autoload
(define-minor-mode org-visual-indent-mode 
  "Add vertical lines to `org-indent'."
  nil
  " vert-indent"
  nil
  (if org-visual-indent-mode
      (progn
	(org-add-props org-visual-indent-pipe
	    '(face org-visual-indent-pipe-face))
	(org-add-props org-visual-indent-blank-pipe
	    '(face org-visual-indent-blank-pipe-face))
	(org-add-props org-visual-indent-small-span
	    '(face org-visual-indent-blank-pipe-face))
	(org-indent-mode -1)
	(setq-local org-indent-mode-turns-off-org-adapt-indentation t
		    org-indent-mode-turns-on-hiding-stars nil
		    org-hide-leading-stars nil)
	(advice-add 'org-indent-set-line-properties :around
		    #'org-visual-indent--org-indent-set-line-properties-advice)
	(advice-add 'org-indent-add-properties :around
		    #'org-visual-indent--org-indent-add-properties-advice)
	(advice-add 'org-indent--compute-prefixes :around
		    #'org-visual-indent--org-indent--compute-prefixes-advice)
	(org-indent-mode 1))
    (advice-remove 'org-indent-set-line-properties 
		   #'org-visual-indent--org-indent-set-line-properties-advice)
    (advice-remove 'org-indent-add-properties 
		   #'org-visual-indent--org-indent-add-properties-advice)
    (advice-remove 'org-indent--compute-prefixes 
		   #'org-visual-indent--org-indent--compute-prefixes-advice)
    (org-indent-mode -1)))

(provide 'org-visual-indent)

;;; org-visual-indent.el ends here

;;; -*- indent-tabs-mode: t; tab-width: 8 -*-
;;;
;;; jenkins-watch.el --- Watch continuous integration build status.

;; Copyright (C) 2010 Andrew Taylor

;; Authors: Andrew Taylor <ataylor@redtoad.ca>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defvar jenkins-api-url "http://SERVER/jobs/JOB/api/xml"
  "The jenkins job api URL.  Override this replacing SERVER and JOB with appropriate values.")

(defvar jenkins-watch-timer-interval 90
  "The interval to poll jenkins.")

(defvar jenkins-watch-timer nil
  "Timer object for jenkins polling will be stored here.")

(defun jenkins-watch-start ()
  (interactive)
  (unless jenkins-watch-timer
    (setq jenkins-watch-timer
	  (run-at-time "0 sec"
		       jenkins-watch-timer-interval
		       #'jenkins-watch-timer-action))
    (jenkins-watch-status-indicator-add-to-mode-line)))

(defun jenkins-watch-stop ()
  (interactive)
  (when jenkins-watch-timer
    (cancel-timer jenkins-watch-timer)
    (setq jenkins-watch-timer nil)
    (jenkins-watch-status-indicator-remove-from-mode-line)))

(defun jenkins-watch-timer-action ()
  (condition-case exception  
      (url-retrieve jenkins-api-url #'jenkins-watch-update-status)
    (error 
     (jenkins-watch-log-error exception)
     (setq jenkins-watch-mode-line "X-("))))

(defun jenkins-watch-update-status (status)
  (goto-char (point-min))
  (search-forward "\n\n")
  (let ((status (jenkins-watch-extract-last-status)))
    (cond ((string-match "blue" status)
	   (setq jenkins-watch-mode-line (concat " " jenkins-watch-mode-line-success)))
	  ((string-match "red" status)
	   (setq jenkins-watch-mode-line (concat " " jenkins-watch-mode-line-failure)))
	  ((string-match "grey" status)
	   (setq jenkins-watch-mode-line "X-("))))
  (kill-buffer))

(defun jenkins-watch-extract-last-status ()
  (condition-case exception
      (let*	((xml (xml-parse-region (point) (point-max)))
		 (project (car xml))
		 (color (car (xml-get-children project 'color))))
	(nth 2 color))
    (error 
     (jenkins-watch-log-error exception)
     "ERROR")))

(defconst jenkins-watch-success-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :ascent center
	    :data
	    "/* XPM */
static char *favicon[] = {
/* columns rows colors chars-per-pixel */
\"16 16 16 1\",
\"  c None\",
\". c #52D252A85432\",
\"X c #6A536A536ADF\",
\"o c #61C857355EA3\",
\"O c #511E32653401\",
\"+ c #8FC875586FC5\",
\"@ c #A6D982B57A7A\",
\"# c #904B8D8D8D04\",
\"$ c #B636B14AB030\",
\"% c #AE1F996095CE\",
\"& c #F51FB84DAABF\",
\"* c #D37D9FF59469\",
\"= c #FDA7C771BBBB\",
\"- c #31FF31FF31FF\",
\"; c #D595D00FCECE\",
\": c #7F7F7F7F8484\",
/* pixels */
\"    ;#X%%#      \",
\"   $$-$@&+$     \",
\"   $#@&*=&+     \",
\"   $X#=&%*#%    \",
\"    %+&*o+oo    \",
\"     %&&&*+#    \",
\"      %**&=%    \",
\"      %&@+X;    \",
\"     ;$=&@      \",
\"    #.#+&+%     \",
\" XoX.#.-OOO#    \",
\"$XooooX.;$O.O;  \",
\"#XXoX-#.: X-:.  \",
\":Xoo..X-.$X.#-# \",
\":XoX-..XoXo.:-o;\",
\"..X.-o-XX-#oX-X:\"
};
")) "Image for successful build.")

(defconst jenkins-watch-mode-line-success
  (if jenkins-watch-success-image
      (propertize ":)"
		  'display jenkins-watch-success-image
		  'help-echo "Build succeeded")
    ":)"))

(defconst jenkins-watch-failure-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :ascent center
	    :data
	    "/* XPM */
static char *favicon[] = {
/* columns rows colors chars-per-pixel */
\"16 16 16 1\",
\"  c red\",
\". c #52D252A85432\",
\"X c #6A536A536ADF\",
\"o c #61C857355EA3\",
\"O c #511E32653401\",
\"+ c #8FC875586FC5\",
\"@ c #A6D982B57A7A\",
\"# c #904B8D8D8D04\",
\"$ c #B636B14AB030\",
\"% c #AE1F996095CE\",
\"& c #F51FB84DAABF\",
\"* c #D37D9FF59469\",
\"= c #FDA7C771BBBB\",
\"- c #31FF31FF31FF\",
\"; c #D595D00FCECE\",
\": c #7F7F7F7F8484\",
/* pixels */
\"    ;#X%%#      \",
\"   $$-$@&+$     \",
\"   $#@&*=&+     \",
\"   $X#=&%*#%    \",
\"    %+&*o+oo    \",
\"     %&&&*+#    \",
\"      %**&=%    \",
\"      %&@+X;    \",
\"     ;$=&@      \",
\"    #.#+&+%     \",
\" XoX.#.-OOO#    \",
\"$XooooX.;$O.O;  \",
\"#XXoX-#.: X-:.  \",
\":Xoo..X-.$X.#-# \",
\":XoX-..XoXo.:-o;\",
\"..X.-o-XX-#oX-X:\"
};
")) "Image for failed build.")

(defconst jenkins-watch-mode-line-failure
  (if jenkins-watch-success-image
      (propertize ":("
		  'display jenkins-watch-failure-image
		  'help-echo "Build failed")
    ":("))

(defvar jenkins-watch-mode-line ":|"
  "What gets displayed on the mode line.")
(put 'jenkins-watch-mode-line 'risky-local-variable t)

(defun jenkins-watch-status-indicator-add-to-mode-line ()
  ""
  (if (boundp 'mode-line-modes)
      (add-to-list 'mode-line-modes '(t jenkins-watch-mode-line) t)))

(defun jenkins-watch-status-indicator-remove-from-mode-line ()
  ""
  (if (boundp 'mode-line-modes)
      (delete '(t jenkins-watch-mode-line) mode-line-modes)))

(defun jenkins-watch-log-error (exception)
  ""
  (message "%s" (concat "jenkins-watch error: " 
			(eval (cons 'format (cdr exception))))))

(provide 'jenkins-watch)


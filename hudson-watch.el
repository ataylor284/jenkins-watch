;;; -*- indent-tabs-mode: t; tab-width: 8 -*-
;;;
;;; hudson-watch.el --- Watch continuous integration build status.

;; Copyright (C) 2010 Andrew Taylor

;; Authors: Andrew Taylor <ataylor@its.to>

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

(defvar hudson-rss-url "http://hudson/hudson/rssAll"
  "The hudson build status RSS feed URL.")

(defvar hudson-watch-timer-interval 10
  "The interval to poll hudson.")

(defvar hudson-watch-timer nil
  "Timer object for hudson polling will be stored here.")

(defun hudson-watch-start ()
  (interactive)
  (unless hudson-watch-timer
    (setq hudson-watch-timer
	  (run-at-time "0 sec"
		       hudson-watch-timer-interval
		       #'hudson-watch-timer-action))
    (hudson-watch-status-indicator-add-to-mode-line)))

(defun hudson-watch-stop ()
  (interactive)
  (when hudson-watch-timer
    (cancel-timer hudson-watch-timer)
    (setq hudson-watch-timer nil)
    (hudson-watch-status-indicator-remove-from-mode-line)))

(defun hudson-watch-timer-action ()
  (url-retrieve hudson-rss-url #'hudson-watch-update-status))

(defun hudson-watch-update-status (status)
  (progn
    (goto-char (point-min))
    (search-forward "\n\n")
    (let* ((xml (xml-parse-region (point) (point-max)))
	   (status (hudson-watch-extract-last-status xml)))
      (if (string-match "SUCCESS" status)
	  (setq hudson-watch-mode-line (concat " " hudson-watch-mode-line-success))
	(setq hudson-watch-mode-line (concat " " hudson-watch-mode-line-failure))))))

(defun hudson-watch-extract-last-status (xml)
  (let*	((feed (first xml))
	 (entries (xml-get-children feed 'entry))
	 (last-entry (first entries)))
    (first (xml-node-children (first (xml-get-children last-entry 'title))))))

(defconst hudson-watch-success-image
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

(defconst hudson-watch-mode-line-success
  (if hudson-watch-success-image
      (propertize ":)"
		  'display hudson-watch-success-image
		  'help-echo "Build succeeded")
    ":)"))

(defconst hudson-watch-failure-image
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

(defconst hudson-watch-mode-line-failure
  (if hudson-watch-success-image
      (propertize ":("
		  'display hudson-watch-failure-image
		  'help-echo "Build failed")
    ":("))

(defvar hudson-watch-mode-line (concat " " hudson-watch-mode-line-success)
  "What gets displayed on the mode line.")
(put 'hudson-watch-mode-line 'risky-local-variable t)

(defun hudson-watch-status-indicator-add-to-mode-line ()
  ""
  (if (boundp 'mode-line-modes)
      (add-to-list 'mode-line-modes '(t hudson-watch-mode-line) t)))

(defun hudson-watch-status-indicator-remove-from-mode-line ()
  ""
  (if (boundp 'mode-line-modes)
      (delete '(t hudson-watch-mode-line) mode-line-modes)))

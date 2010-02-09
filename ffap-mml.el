;;; ffap-mml.el --- find Gnus message MML attached file at point

;; Copyright 2007, 2009, 2010 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 5
;; Keywords: files
;; URL: http://user42.tuxfamily.org/ffap-mml/index.html
;; EmacsWiki: FindFileAtPoint

;; ffap-mml.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; ffap-mml.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This spot of code to lets M-x ffap find a file attached as a Gnus
;; "message meta-language" (MML) part, as per C-c C-a when composing a
;; message.  Eg.
;;
;;     <#part type="text/plain" filename="/foo/bar.txt" disposition=attachment>
;;     <#/part>
;;
;; Of course ffap can already follow it when point is right on the filename,
;; but ffap-mml.el makes it work when point is somewhere else in this form,
;; like as the start of the line.

;;; Install:

;; Put ffap-mml.el in one of your `load-path' directories and the following
;; in your .emacs
;;
;;     (eval-after-load "ffap" '(require 'ffap-mml))

;;; History:
;; 
;; Version 1 - the first version.
;; Version 2 - set region for ffap-highlight
;; Version 3 - new home page
;; Version 4 - set ffap-string-at-point variable
;; Version 5 - undo defadvice on unload-feature

;;; Code:

;;;###autoload (eval-after-load "ffap" '(require 'ffap-mml))

(put 'mml-filename 'bounds-of-thing-at-point
     (lambda ()
       ;; This pattern is pretty slack: a backslash can escape a " in a
       ;; quoted string, and > is allowed in a quoted string, and an
       ;; unquoted value ends with a space, etc.
       ;; The optional match of the <#/part> ending makes the thing-at-point
       ;; work with point within that ending.
       (and (thing-at-point-looking-at "\
<#\\(part\\|mml\\)[^>]*filename=\"?\\([^\">]+\\)\\([^>]*>\n*<#/part>\\)?")
            (cons (match-beginning 2) (match-end 2)))))

(defadvice ffap-string-at-point (around ffap-mml activate)
  "Recognise message MML attached files with point at start of line."
  (unless (let ((bounds (bounds-of-thing-at-point 'mml-filename)))
            (when bounds
              (setq ffap-string-at-point-region (list (car bounds)
                                                      (cdr bounds)))
              (setq ad-return-value
                    (setq ffap-string-at-point
                          (buffer-substring-no-properties (car bounds)
                                                          (cdr bounds))))))
    ad-do-it))

(defun ffap-mml-unload-function ()
  (when (ad-find-advice 'ffap-string-at-point 'around 'ffap-mml)
    (ad-remove-advice   'ffap-string-at-point 'around 'ffap-mml)
    (ad-activate        'ffap-string-at-point))
  nil) ;; and do normal unload-feature actions too

(provide 'ffap-mml)

;;; ffap-mml.el ends here
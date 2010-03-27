;;; ffap-mml.el --- find Gnus message MML attached file at point

;; Copyright 2007, 2009, 2010 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 7
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

;; This spot of code to lets M-x ffap find a file attached as a Gnus message
;; meta-language (MML) part, as per `C-c C-a' (mml-attach-part) when
;; composing a message.  Eg.
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
;;
;; There's an autoload cookie for this if you know how to use
;; `update-file-autoloads' and friends.

;;; History:
;; 
;; Version 1 - the first version.
;; Version 2 - set region for ffap-highlight
;; Version 3 - new home page
;; Version 4 - set ffap-string-at-point variable
;; Version 5 - undo defadvice on unload-feature
;; Version 6 - speedup in big buffer
;;           - namespace clean thing-at-point 'ffap-mml-filename
;; Version 7 - thing-at-point now includes the quotes
;;           - unescape with `read'

;;; Code:

;;;###autoload (eval-after-load "ffap" '(require 'ffap-mml))

;; for `ad-find-advice' macro in ffap-mml-unload-function when running
;; uncompiled (don't unload 'advice before ffap-mml-unload-function)
(require 'advice)

(require 'ffap)


;; `mml-insert-tag' puts strings with unusual chars through `prin1'.  The
;; regexp matches a string escaped like that, then `ffap-mml-at-point' puts
;; it through `read' to undo the escaping (as per mml-read-tag in gnus March
;; 2010).
;;
;; An unquoted word like filename=foo is matched and handled too, but that's
;; unusual because the `mml-attach-part' puts a directory in and if you use
;; that then there's always a "/" which induces the prin1.  Without a path
;; makes it vulnerable to a different default-directory when sending too, so
;; probalby isn't a good idea on the whole.
;;
;; The pattern should be resistant to degenerate values in other fields
;; field, since a filename=" anywhere else would be escaped to filename=\"
;; and so not match.
;;
;; The <#/part> is included in the pattern so it matches with point in that
;; bit as well as the main <#part> line.  But the match is only optional so
;; as not to demand it if the <#part> somehow appears alone.  Oh, and
;; there's no "\n" at the end of the match, so if point is at the start of
;; the following line it doesn't hit the MML bit.
;;
;; Restricting to a few lines surrounding point keeps down the searching
;; done by `thing-at-point-looking-at' in its workaround for
;; `re-search-backward' not matching across point.  Without this it can
;; take a few seconds to find no match in a very big buffer.  A few lines
;; are allowed just in case a filename has a newline in it.
;;
;; `mml-read-tag' might be better for picking out the filename value,
;; since that's what will happen when sending the message.  However,
;;    * It depends on on `message-mode' syntax and sexp settings, whereas
;;      would much prefer `ffap' to go only from the buffer contents
;;      irrespective of the mode.
;;    * It doesn't return the filename buffer region, so would still need
;;      some matching just to set `ffap-string-at-point-region'.
;;    * In Emacs 23.1 and earlier it didn't unescape with `read' so
;;      couldn't handle " (double quote) chars.  It may be acceptable
;;      though for ffap to do the same thing as would happen on attempting
;;      to send (ie. the same wrong interpretation of the escaping).

(put 'ffap-mml-filename 'bounds-of-thing-at-point
     (lambda ()
       (and (save-restriction
              (narrow-to-region (save-excursion (forward-line -5) (point))
                                (save-excursion (forward-line 5) (point)))
              (thing-at-point-looking-at "\
^<#part.*?\
filename=\\([^\" \t\r\n]+\
\\|\"\\(\\\\\\(.\\|\n\\)\\|[^\"\\]\\)*\"\
\\)\
.*?>\\(\n<#/part>\\)?"))
            (cons (match-beginning 1) (match-end 1)))))

(defun ffap-mml-at-point ()
  "Return an MML <#part> filename at point.
This is an internal part of ffap-mml.el.

If there's a <#part> with a filename at point then return the
filename and put it in `ffap-string-at-point' and the buffer
region in `ffap-string-at-point-region'.  If no <#part> at point
then return nil."

  (let ((bounds (bounds-of-thing-at-point 'ffap-mml-filename)))
    (when bounds
      (let* ((filename (buffer-substring-no-properties (car bounds)
                                                       (cdr bounds))))
        (if (string-match "\\`\"" filename)
            (setq filename (condition-case nil
                               (read filename)
                             (error nil))))
        (when filename
          (setq ffap-string-at-point-region (list (car bounds)
                                                  (cdr bounds)))
          (setq ffap-string-at-point filename))))))

(defadvice ffap-string-at-point (around ffap-mml activate)
  "Recognise message MML attached files with point at start of line."
  (if (ffap-mml-at-point)
      (setq ad-return-value ffap-string-at-point)
    ad-do-it))

(defun ffap-mml-unload-function ()
  "Remove ffap-mml defadvice and thing-at-point."
  (put 'ffap-mml-filename 'bounds-of-thing-at-point nil)
  (when (ad-find-advice 'ffap-string-at-point 'around 'ffap-mml)
    (ad-remove-advice   'ffap-string-at-point 'around 'ffap-mml)
    (ad-activate        'ffap-string-at-point))
  nil) ;; and do normal unload-feature actions too

(provide 'ffap-mml)

;;; ffap-mml.el ends here

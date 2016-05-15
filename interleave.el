;;; interleave.el --- Interleaving text books since 2015

;; Author: Sebastian Christ <rudolfo.christ@gmail.com>
;; URL: https://github.com/rudolfochrist/interleave
;; Version: 1.1.0

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; In the past, textbooks were sometimes published as 'interleaved' editions.
;; That meant, each page was followed by a blank page and the ambitious student/
;; scholar had the ability to take their notes directly in their copy of the
;; textbook. Newton and Kant were prominent representatives of this technique.

;; Nowadays textbooks (or lecture material) come in PDF format. Although almost
;; every PDF Reader has the ability to add some notes to the PDF itself, it is
;; not as powerful as it could be.

;; This is what this minor mode tries to accomplish. It presents your PDF side by
;; side to an [[http://orgmode.org][Org Mode]] buffer with your notes, narrowing
;; down to just those passages that are relevant to the particular page in the
;; document viewer.

;;; Usage:

;; - Create a Org file that will keep your notes. In the Org headers section, add
;; #+INTERLEAVE_PDF: /the/path/to/your/pdf.pdf
;; - Start `interleave' with `M-x interleave'.
;; - To insert a note for a page, type `i'.
;; - Navigation is the same as in `doc-view-mode'/`pdf-view-mode'."

;;; Code:

(require 'org)

(require 'doc-view)
;; Redefining `doc-view-kill-proc-and-buffer' as `interleave--pdf-kill-proc-and-buffer'
;; because this function is obsolete in emacs 25.1 onwards.
(defun interleave--pdf-kill-proc-and-buffer ()
  "Kill the current converter process and buffer."
  (interactive)
  (when (derived-mode-p 'doc-view-mode)
    (doc-view-kill-proc))
  (when (or (derived-mode-p 'doc-view-mode)
            (derived-mode-p 'pdf-view-mode))
    (kill-buffer (current-buffer))))

(defvar *interleave--org-buffer* nil
  "Org notes buffer")
(defvar *interleave--pdf-buffer* nil
  "PDF buffer associated with the notes buffer")

(defvar interleave--window-configuration nil
  "Variable to store the window configuration before interleave mode was enabled.")

(make-variable-buffer-local
 (defvar *interleave--page-marker* 0
   "Caches the current page while scrolling"))

(make-variable-buffer-local
 (defvar *interleave--multi-pdf-notes-file* t
   "Indicates if the current Org notes file is a multi-pdf notes file."))

(defvar interleave-headline-property-formatter nil
  "Page note property for org headline")

(defvar interleave-custom-id nil
  "Custom id")

(defun interleave--find-pdf ()
  "Searches for `interleave_pdf' property in buffer or file"
  (save-excursion
    (if *interleave--multi-pdf-notes-file*
	(let* ((id (org-entry-get nil "Custom_ID"))
	       (pdf (expand-file-name (concat id ".pdf")
				      org-ref-pdf-directory)))
	  (setq interleave-headline-property-formatter
		(format "%s-page" id))
	  (setq interleave-custom-id id)
	  (if (f-exists? pdf)
	      pdf
	    (user-error "No pdf named %s found!!!!" id)))
      (goto-char (point-min))
      (re-search-forward "^\\+interleave_pdf: \\(.*\\)")
      (when (match-string 0)
        (match-string 1)))))

(defun interleave--open-file (split-window)
  "Opens the interleave pdf file in `doc-view-mode'/`pdf-view-mode'  besides the
notes buffer.

SPLIT-WINDOW is a function that actually splits the window, so it must be either
`split-window-right' or `split-window-below'."
  (let ((buf (current-buffer)))
    (condition-case nil
        (progn
          (delete-other-windows)
          (funcall #'(lambda () (split-window nil 140 'vertical)))
          (find-file (interleave--find-pdf)))
      ('error
       (let ((pdf-file-name
              (read-file-name "No #+INTERLEAVE_PDF property found. Please specify path: " "~/")))
         (find-file (expand-file-name pdf-file-name))
         (with-current-buffer buf
           (insert "#+INTERLEAVE_PDF: " pdf-file-name)))))
    (interleave-pdf-mode 1)))

(defun interleave--goto-search-start-position ()
  "Move point to the search start position.

For multi-pdf notes this is the outermost parent headline. For everything else
this is the beginning of the buffer."
  (widen)
  (if *interleave--multi-pdf-notes-file*
      (progn
	(goto-char (point-min))
	(unless (re-search-forward
		 (format
		  "^\[ \t\r]*\:Custom_ID\: %s"
		  interleave-custom-id) nil t)
	  (user-error "No pdf found")))
    (goto-char (point-min))))

(defun interleave--go-to-page-note (page)
  "Searches the notes buffer for an headline with the `interleave_page_note'
property set to PAGE. It narrows the subtree when found."
  (interleave--switch-to-org-buffer)
  (interleave--goto-search-start-position)
  (if (re-search-forward
       (format "^\[ \t\r\]*\:%s\: %s$" interleave-headline-property-formatter page) nil t)
      (progn (org-narrow-to-subtree)
	     (org-show-entry) 
	     t)
    nil))

(defun interleave-go-to-next-page ()
  "Go to the next page in PDF. Look up for available notes." 
  (interactive)
  (let ((page (doc-view-current-page)))
    (funcall #'doc-view-next-page)
    (unless (= page (doc-view-last-page-number))
      (interleave--go-to-page-note (1+ page))
      (interleave--switch-to-pdf-buffer))))

(defun interleave-go-to-previous-page ()
  "Go to the previous page in PDF. Look up for available notes." 
  (interactive)
  (let ((page (doc-view-current-page))) 
    (funcall #'doc-view-previous-page)
    (unless (= page 1)
      (interleave--go-to-page-note (1- page))
      (interleave--switch-to-pdf-buffer))))

(defun interleave-scroll-up ()
  "Scroll up the PDF. Look up for available notes."
  (interactive)
  (setq *interleave--page-marker* (funcall #'doc-view-current-page))
  (funcall #'doc-view-scroll-up-or-next-page)
  (unless (= *interleave--page-marker* (funcall #'doc-view-current-page))
    (interleave--go-to-page-note (funcall #'doc-view-current-page)))
  (interleave--switch-to-pdf-buffer))

(defun interleave-scroll-down ()
  "Scroll down the PDF. Look up for available notes."
  (interactive)
  (setq *interleave--page-marker* (funcall #'doc-view-current-page))
  (funcall #'doc-view-scroll-down-or-previous-page)
  (unless (= *interleave--page-marker* (funcall #'doc-view-current-page))
    (interleave--go-to-page-note (funcall #'doc-view-current-page)))
  (interleave--switch-to-pdf-buffer))

(defun interleave--switch-to-org-buffer (&optional insert-newline-maybe)
  (if (derived-mode-p 'doc-view-mode) 
      (switch-to-buffer-other-window *interleave--org-buffer*)
    (switch-to-buffer *interleave--org-buffer*))
  (when insert-newline-maybe
    (goto-char (point-max))
    (redisplay)
    ;; Insert a new line if not already on a new line
    (when (not (looking-back "^ *"))
      (org-return))))

(defun interleave--switch-to-pdf-buffer ()
  (if (derived-mode-p 'org-mode)
      (switch-to-buffer-other-window *interleave--pdf-buffer*)
    (switch-to-buffer *interleave--pdf-buffer*)))

(defun interleave--insert-heading-respect-content ()
  (org-insert-heading-respect-content)
  (when *interleave--multi-pdf-notes-file*
    (org-demote)))

(defun interleave--create-new-note (page)
  "Creates a new headline for the page PAGE."
  (interleave--switch-to-org-buffer t) 
  (interleave--goto-search-start-position) 
  (interleave--insert-heading-respect-content)
  (insert (format "Notes for page %d" page))
  (org-set-property interleave-headline-property-formatter
		    (number-to-string page))
  (org-end-of-subtree)
  (newline 2)
  (org-cycle-hide-drawers nil)
  (org-narrow-to-subtree))

(defun interleave-add-note-for-pdf ()
  "Add note for the current page. If there are already notes for this page,
jump to the notes buffer."
  (interactive)
  (let ((page (doc-view-current-page)))
    (if (interleave--go-to-page-note page)
        (interleave--switch-to-org-buffer t)
      (interleave--create-new-note page))))

(defun interleave--sync-pdf-page-current ()
  "Synchronize the page in the pdf buffer to be the same as the page in
the current narrowed down notes view."
  (interactive)
  (interleave--switch-to-org-buffer)
  (let (pdf-page)
    (setq pdf-page
          (string-to-number (org-entry-get nil interleave-headline-property-formatter))) 
    (interleave--switch-to-pdf-buffer)
    (funcall #'(lambda (doc-view-goto-page pdf-page)))
    (interleave--switch-to-org-buffer)))

(defun interleave--sync-pdf-page-previous ()
  "Synchronize the page in the pdf buffer to be the same as the page in the
previous set of notes."
  (interactive)
  (interleave--switch-to-org-buffer)
  (let (pdf-page)
    (org-narrow-to-subtree)
    (goto-char (point-min))
    (widen)
    (when (ignore-errors
	    (re-search-backward (format
				 "^ *:%s: *\\(.*\\)"
				 interleave-headline-property-formatter)
				nil))
      (setq pdf-page (string-to-number (match-string 1))))
    (if pdf-page
        (progn
          (org-narrow-to-subtree)
          ;; (interleave--go-to-page-note pdf-page)
          (interleave--switch-to-pdf-buffer)
          (funcall #'(lambda () (doc-view-goto-page pdf-page)))
	  (interleave--switch-to-org-buffer)
	  (org-end-of-subtree))
      (org-narrow-to-subtree)
      (message "First note")
      (org-end-of-subtree))))

(defun interleave--sync-pdf-page-next ()
  "Synchronize the page in the pdf buffer to be the same as the page in the
next set of notes."
  (interactive)
  (interleave--switch-to-org-buffer)
  (let (pdf-page)
    (org-narrow-to-subtree)
    (goto-char (point-max))
    (widen)
    (when (re-search-forward (format
			      "^ *:%s: *\\(.*\\)"
			      interleave-headline-property-formatter)
			     nil :noerror) ; next page
      (setq pdf-page (string-to-number (match-string 1))))
    (if pdf-page 
        (progn
          (interleave--switch-to-pdf-buffer)
          (funcall #'(lambda () (doc-view-goto-page pdf-page)))
          (interleave--go-to-page-note pdf-page)
          (org-narrow-to-subtree)
	  (org-end-of-subtree))
      (org-narrow-to-subtree)
      (message "No next notes"))))

(defun interleave--add-note-from-pdf ()
  "Open the notes associated with the entry using `find-file'."
  (interactive)
  (when (or (f-exists? (buffer-file-name)) (derived-mode-p 'doc-view-mode))
    (let* ((pdf (buffer-file-name))
	   (key (file-name-base pdf)))
      (if (f-directory? bibtex-completion-notes-path)
	  (let ((path (f-join bibtex-completion-notes-path
			      (s-concat key bibtex-completion-notes-extension))))
	    (find-file path)
	    (unless (f-exists? path)
	      (insert (s-index-of bibtex-completion-notes-template-multiple-files
				  'bibtex-completion-apa-get-value
				  (bibtex-completion-get-entry key)))))
					; One file for all notes:
	(find-file-other-window bibtex-completion-notes-path)
	(widen)
	(show-all)
	(goto-char (point-min))
	(if (re-search-forward (format bibtex-completion-notes-key-pattern key) nil t)
					; Existing entry found:
	    (when (eq major-mode 'org-mode)
	      (org-narrow-to-subtree)
	      (re-search-backward "^\*+ " nil t)
	      (org-cycle-hide-drawers nil)
	      (bibtex-completion-notes-mode 1))
					; Create a new entry:
	  (let ((entry (bibtex-completion-get-entry key)))
	    (goto-char (point-max))
	    (insert (s-format bibtex-completion-notes-template-one-file
			      'bibtex-completion-apa-get-value
			      entry))
	    (org-set-property "interleave_pdf" pdf)
	    (call-interactively 'interleave)))))))

(defun interleave--quit ()
  "Quit interleave mode."
  (interactive)
  (interleave--switch-to-org-buffer)
  (widen)
  (interleave--goto-search-start-position)
  (ignore-errors
    (interleave--sort-notes interleave-sort-order))
  (org-overview)
  (interleave 0)
  (interleave--pdf-kill-proc-and-buffer))

(defcustom interleave-sort-order 'asc
  "Specifiy the notes' sort order in the notes buffer.

The possible values are 'asc for ascending and 'desc for descending."
  :type '(choice (const  asc)
                 (const  desc))
  :group 'interleave)

(defun interleave--sort-notes (sort-order)
  "Sort notes by interleave_page_property.

SORT-ORDER is either 'asc or 'desc."
  (org-sort-entries nil ?f
                    (lambda ()
                      (or (string-to-number
                           (org-entry-get nil
                                          "interleave_page_note"))
                          -1))
                    (if (eq sort-order 'asc)
                        #'<
                      #'>)))

;;; Interleave
;; Minor mode for the org file buffer containing notes

(defvar interleave-map (make-sparse-keymap)
  "Keymap while `interleave' is active in the org file buffer.")

;;;###autoload
(define-minor-mode interleave
  "Interleaving your text books since 2015.

In the past, textbooks were sometimes published as 'interleaved' editions.
That meant, each page was followed by a blank page and the ambitious student/
scholar had the ability to take their notes directly in their copy of the
textbook. Newton and Kant were prominent representatives of this technique.

Nowadays textbooks (or lecture material) come in PDF format. Although almost
every PDF Reader has the ability to add some notes to the PDF itself, it is
not as powerful as it could be.

This is what this minor mode tries to accomplish. It presents your PDF side by
side to an [[http://orgmode.org][Org Mode]] buffer with your notes, narrowing
down to just those passages that are relevant to the particular page in the
document viewer.

Usage:

- Create a Org file that will keep your notes. In the Org headers section, add
#+INTERLEAVE_PDF: /the/path/to/your/pdf.pdf
- Start `interleave' with `M-x interleave'.
- To insert a note for a page, type `i'.
- Navigation is the same as in `doc-view-mode'/`pdf-view-mode'.

Keybindings (`doc-view-mode'/`pdf-view-mode'):

\\{interleave-pdf-mode-map}

Keybindings (org-mode buffer):

\\{interleave-map}"
  :lighter " ≡"
  :global nil
  :keymap  interleave-map
  (if interleave
      (progn
        (message "Interleave enabled")
        (setq interleave--window-configuration (current-window-configuration))
        (setq *interleave--org-buffer* (current-buffer))
        (interleave--open-file (or (and current-prefix-arg #'split-window-below)
                                   #'split-window-right))
        (interleave--go-to-page-note 1)
        (interleave--switch-to-pdf-buffer))
    (progn
      (message "Interleave disabled")
      (set-window-configuration interleave--window-configuration))))

;;; Interleave PDF Mode
;; Minor mode for the pdf file buffer associated with the notes

(defvar interleave-pdf-mode-map (make-sparse-keymap)
  "Keymap while `interleave-pdf-mode' is active in the pdf file buffer.")

;;;###autoload
(define-minor-mode interleave-pdf-mode
  "Interleave view for the pdf."
  :lighter " ≡"
  :global nil
  :keymap  interleave-pdf-mode-map
  (when interleave-pdf-mode
    (setq *interleave--pdf-buffer* (current-buffer))))

;;; Key-bindings

(define-key interleave-map (kbd "M-.") #'interleave--sync-pdf-page-current)
(define-key interleave-map (kbd "M-p") #'interleave--sync-pdf-page-previous)
(define-key interleave-map (kbd "M-n") #'interleave--sync-pdf-page-next)

(define-key interleave-pdf-mode-map (kbd "n")     #'interleave-go-to-next-page)
(define-key interleave-pdf-mode-map (kbd "p")     #'interleave-go-to-previous-page)
(define-key interleave-pdf-mode-map (kbd "SPC")   #'interleave-scroll-up)
(define-key interleave-pdf-mode-map (kbd "S-SPC") #'interleave-scroll-down)
(define-key interleave-pdf-mode-map (kbd "DEL")   #'interleave-scroll-down)
(define-key interleave-pdf-mode-map (kbd "i")     #'interleave-add-note-for-pdf)
(define-key interleave-pdf-mode-map (kbd "q")     #'interleave--quit)
(define-key interleave-pdf-mode-map (kbd "M-.")   #'interleave--sync-pdf-page-current)
(define-key interleave-pdf-mode-map (kbd "M-p")   #'interleave--sync-pdf-page-previous)
(define-key interleave-pdf-mode-map (kbd "M-n")   #'interleave--sync-pdf-page-next)

(define-key doc-view-mode-map (kbd "i") #'interleave--add-note-from-pdf)
(when (featurep 'pdf-view)
  (define-key pdf-view-mode-map (kbd "i") #'interleave--add-note-from-pdf))


(provide 'interleave)

;;; interleave.el ends here

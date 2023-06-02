;;; docsim.el --- find notes that are textually similar to this one.

;; Author: Harry R. Schwartz <hello@harryrschwartz.com>
;; Maintainer: Harry R. Schwartz <hello@harryrschwartz.com>
;; Package-Requires: ((cl-lib "0.5") (org-mode "9.4"))
;; Homepage: https://github.com/hrs/docsim-mode

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Suppose you have a collection of linked notes (a "zettelkasten"). I find
;;; that the value of such a collection is in the links between notes as much as
;;; in the notes themselves! It's easy to write a new note, but it can be tricky
;;; to appropriately link it with other existing notes. You may have forgotten
;;; what notes you've already written, or you may overlook a non-obvious
;;; connection.
;;;
;;; Docsim uses an external tool to suggest connections between your current
;;; note and others in your collection. The collection is parsed and notes are
;;; ranked and displayed according to their textual similarity. The process is
;;; quite snappy and results are displayed in a buffer.
;;;
;;; First, you'll need to install the `docsim' command-line tool.
;;;
;;; Next, tell docsim where to find your notes by configuring `(setq
;;; docsim-search-paths '("~/documents/notes"))'.
;;;
;;; Docsim happens to know about Denote links because its author uses and likes
;;; it. If you are, too, you can tell docsim not to include notes that are
;;; already linked from a note in its list of similar results. Set that up with
;;; `(setq docsim-omit-denote-links t)'.
;;;
;;; Docsim performs best with English-language notes, since it can take
;;; advantage of a built-in stoplist and a stemming algorithm. It'll still work
;;; with non-English notes, of course, but it may not be quite as accurate. Just
;;; `(setq docsim-assume-english nil)' if you're using another language.
;;;
;;; By default docsim shows the 10 most similar notes, but you can modify that
;;; by setting `(setq docsim-limit 5)'

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'org))

(defgroup docsim nil
  "Find similar documents."
  :group 'external)

(defcustom docsim-executable
  "docsim"
  "The docsim executable."
  :type 'string
  :group 'docsim)

(defcustom docsim-search-paths '("~/notes")
  "Directories or files containing notes to be searched."
  :type '(repeat string)
  :group 'docsim)

(defcustom docsim-show-scores t
  "Include similarity score in results."
  :type 'boolean
  :group 'docsim)

(defcustom docsim-omit-denote-links nil
  "Omit Denote notes linked from the document from results."
  :type 'boolean
  :group 'docsim)

(defcustom docsim-limit 10
  "Maximum number of results to show."
  :type '(choice (natnum :tag "Maximum number of links")
                 (const :tag "Show all results" nil))
  :group 'docsim)

(defcustom docsim-assume-english t
  "Assume that notes are in English.

If non-nil, `docsim' can assume that the notes it's comparing are
written in English, and can therefore apply a stoplist and an
appropriate stemming algorithm to improve comparison accuracy.

If nil, well, it should still work, but accuracy may not be quite
as good. Sorry. You can partially mitigate that by setting a
custom stoplist with `docsim-stoplist-path'."
  :type 'boolean
  :group 'docsim)

(defcustom docsim-stoplist-path nil
  "Path to a custom stoplist file.

A stoplist file is a text file of words that should be completely
ignored when comparing documents, separated by newlines. In
English we'd probably include words like \"the\" or \"because\".

If nil `docsim' will assume that it should use its built-in
English stoplist (unless you've also set `docsim-assume-english'
to nil)"
  :type 'string
  :group 'docsim)

(cl-defstruct docsim--record path score)

(defun docsim--parse-markdown-yaml-title ()
  "Treat the current buffer as if it might have YAML front matter and attempt to parse the `title:'."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "^---$")
        (forward-line 1)
        (let ((yaml (buffer-substring-no-properties (point)
                                                    (re-search-forward "^---$"))))
          (when (string-match "^title: \\(.*\\)$" yaml)
            (match-string 1 yaml))))))

(defun docsim--record-title (record)
  "Return the `#+title' of the org file associated with RECORD."
  (with-temp-buffer
    (insert-file-contents (docsim--record-path record))
    (or (cadar (org-collect-keywords '("TITLE")))
        (docsim--parse-markdown-yaml-title))))

(defun docsim--record-to-org (record)
  "Format RECORD as a line of Org markup for the results buffer.

If it's an Org document with a `#+title:', use that as the link
text. If not, do your best by showing the file path."
  (let* ((org-title (docsim--record-title record))
         (link-text (or org-title (docsim--record-path record)))
         (link (format "[[file:%s][%s]]" (docsim--record-path record) link-text))
         (score (if docsim-show-scores
                    (format "%s :: " (docsim--record-score record))
                  "")))
    (format "- %s%s" score link)))

(defun docsim--denote-ids-in-string (s)
  "Given a string S, return every string that plausible matches a Denote ID."
  (let ((denote-id-regexp (rx "[denote:"
                              (group (= 8 digit)
                                     "T"
                                     (= 6 digit))
                              "]"))
        (i 0)
        matches)
    (while (string-match denote-id-regexp s i)
      (setq matches (cons (match-string 1 s) matches))
      (setq i (match-end 0)))
    matches))


(defun docsim--denote-ids-in-file (file-name)
  "Given a file FILE-NAME, return every string that plausible matches a Denote ID."
  (docsim--denote-ids-in-string (with-temp-buffer
                                  (insert-file-contents file-name)
                                  (buffer-string))))

(defun docsim--similar-notes-org (file-name)
  "Return an Org-formatted string of results for running `docsim' on FILE-NAME."
  (thread-last (docsim--similar-notes file-name)
               (mapcar #'docsim--record-to-org)
               ((lambda (xs) (mapconcat 'identity xs "\n")))
               (concat "Similar notes:\n\n")))

(defun docsim--remove-denote-links (file-name records)
  "Return RECORDS excluding notes already linked from FILE-NAME."
  (if docsim-omit-denote-links
      (let ((linked-denote-ids (docsim--denote-ids-in-file file-name)))
        (cl-remove-if-not (lambda (record)
                            (not (seq-some (lambda (denote-id)
                                             (string-match-p denote-id (docsim--record-path record)))
                                           linked-denote-ids)))
                          records))
    records))

(defun docsim--limit-results (records)
  "Return RECORDS with no more than `docsim-limit' results.

Return them all if `docsim-limit' is nil."
  (if docsim-limit
      (seq-take records docsim-limit)
    records))

(defun docsim--similar-notes (file-name)
  "Return a list of records resulting from running `docsim' on FILE-NAME.

Include no more that `docsim-limit' results, and omit any results
that already seem to be linked from FILE-NAME."
  (thread-last (docsim--similarity-results file-name)
               (docsim--remove-denote-links file-name)
               (docsim--limit-results)))

(defun docsim--visit-link ()
  "Visit the next availabile link (which is usually on the current line)."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (point-max))
        link-pos)
    (save-excursion
      (goto-char beg)
      (when (re-search-forward org-link-bracket-re end t)
        (goto-char (match-beginning 0))
        (org-open-at-point)))))

(defun docsim--quit-sidebuffer ()
  "Close the sidebuffer and return point to the previous window."
  (interactive)
  (let ((side-window (get-buffer-window (current-buffer))))
    (kill-buffer (current-buffer))
    (select-window (get-mru-window nil t t side-window))))

(defun docsim--parse-record (line)
  "Parse a LINE of `docsim' results into a `docsim--record' struct."
  (let* ((score (car (split-string line "\t")))
         (path (substring line (1+ (length score)))))
    (make-docsim--record :path path
                         :score score)))

(defun docsim--quote-path (path)
  "Wrap PATH in quotes for interpolation into a shell command."
  (format "\"%s\"" (file-truename path)))

(defun docsim--stemming-stoplist-flags ()
  "Return a list of stemming- and stoplist-related flags to be passed to the shell command."
  (if docsim-assume-english
      (when docsim-stoplist-path
        `("--stoplist" ,(docsim--quote-path docsim-stoplist-path)))
    (if (not docsim-stoplist-path)
        '("--no-stemming" "--no-stoplist")
      `("--no-stemming" "--stoplist" ,(docsim--quote-path docsim-stoplist-path)))))

(defun docsim--shell-command (file-name)
  "Return a string containing the `docsim' command to run on FILE-NAME."
  (mapconcat 'identity
             `(,docsim-executable
               "--best-first"
               "--omit-query"
               "--show-scores"
               ,@(docsim--stemming-stoplist-flags)
               "--query" ,(docsim--quote-path file-name)
               ,@(mapcar 'docsim--quote-path docsim-search-paths))
             " "))

(defun docsim--similarity-results (file-name)
  "Run `docsim' on FILE-NAME and return a list of `docsim--record' structs."
  (mapcar #'docsim--parse-record
          (thread-first (docsim--shell-command file-name)
                        (shell-command-to-string)
                        (substring 0 -1)
                        (split-string "\n"))))

(defun docsim-show-similar-notes (file-name)
  "Display a list of notes that look similar to FILE-NAME.

This calls out to the external `docsim' tool to perform textual
analysis on all the notes in `docsim-search-paths', score them by
similarity to FILE-NAME, and return the sorted results, best
first.

Include the similarity scores (between 0.0 and 1.0) of each note
if `docsim-show-scores' is non-nil.

Show at most `docsim-limit' results (or all of them, if
`docsim-limit' is nil).

If `docsim-omit-denote-links' is non-nil, don't include files
that seem to be already linked from FILE-NAME. This can be
helpful for identifying files that \"should\" be linked but
aren't yet."
  (interactive (list (buffer-file-name)))
  (if file-name
      (let ((sidebar-buffer (get-buffer-create (format "*similar notes: %s*"
                                                       (file-name-nondirectory file-name)))))
        (with-current-buffer sidebar-buffer
          (setq-local buffer-read-only nil)
          (erase-buffer)
          (insert (docsim--similar-notes-org file-name))
          (docsim-mode)
          (goto-char (point-min)))

        (pop-to-buffer sidebar-buffer))
    (error "can't compare this buffer (have you saved it?)")))

(defvar docsim-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'docsim--visit-link)
    (define-key map (kbd "q") 'docsim--quit-sidebuffer)
    map)
  "Keymap for `docsim-mode`.")

(define-derived-mode docsim-mode org-mode "Docsim"
  "Major mode for docsim results buffers."
  (setq-local buffer-read-only t)
  (use-local-map docsim-mode-map))

(provide 'docsim-mode)

;;; docsim-mode.el ends here

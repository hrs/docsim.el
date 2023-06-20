;;; docsim.el --- Search and compare notes with a local search engine

;; Author: Harry R. Schwartz <hello@harryrschwartz.com>
;; Maintainer: Harry R. Schwartz <hello@harryrschwartz.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (org "8.0"))
;; Homepage: https://github.com/hrs/docsim.el

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A fast, local search engine for your text files. Query your notes and get a
;; ranked list of the best matches.
;;
;; `docsim.el' uses a few different information retrieval algorithms to provide
;; a ranked list of text documents. It takes term frequency into account, weighs
;; rare words more heavily, uses stoplists to ignore common terms, and
;; (optionally) stems English words (so "spinning" can match "spinner").
;;
;; Why not just use `grep', `ripgrep', or `ag'? Those tools are all great, but
;; they search for literal text matches, usually line-by-line. You might use
;; docsim when you want to know, "what documents are *most similar* to this
;; query, or to this other note?"
;;
;; If I search for "chunky bacon," I still want to see documents that talk about
;; "chunks of bacon." And, below those, I probably want to see notes that
;; discuss regular "bacon," even if it's not chunky.
;;
;; To use this mode, you'll first need to install the `docsim' command-line
;; tool. See `https://github.com/hrs/docsim'.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)

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

(cl-deftype docsim-query ()
  '(or string buffer))

(cl-deftype docsim-score ()
  '(and cons
        (satisfies (lambda (match)
                     (and (cl-typep (car match) 'string)
                          (cl-typep (cdr match) 'string))))))

(cl-deftype docsim-score-list ()
  '(and list
        (satisfies (lambda (scores)
                     (cl-every (lambda (score)
                                 (cl-typep score 'docsim-score))
                               scores)))))

(defun docsim--parse-title-markdown-yaml ()
  "Attempt to parse `title' from YAML front matter in the current buffer.

This treats the current buffer as if it contains Markdown with
YAML front matter, as used, for example, in Jekyll blog posts. It
attempts to parse out and return the value associated with the
`title' key. If that's not found, return nil."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "^---$")
      (forward-line 1)
      (let ((yaml (buffer-substring-no-properties (point)
                                                  (re-search-forward "^---$"))))
        (when (string-match "^title: \\(.*\\)$" yaml)
          (match-string 1 yaml))))))

(defun docsim--parse-title-org ()
  "Attempt to parse `#+TITLE:' from Org in the current buffer.

This treats the current buffer as if it contains Org text. It
attempts to parse out and return value associated with the
`#+TITLE:' keyword. If that's not found, return nil."
  (if (fboundp 'org-collect-keywords)
      (cadar (org-collect-keywords '("title")))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (when (search-forward-regexp "^#\\+title:" nil t)
          (org-element-property :value (org-element-context (org-element-at-point))))))))

(defun docsim--search-result-title (path)
  "Return a title determined by parsing the file at PATH."
  (cl-check-type path string)
  (with-temp-buffer
    (insert-file-contents path)
    (or (docsim--parse-title-org)
        (docsim--parse-title-markdown-yaml))))

(defun docsim--relative-path (path)
  "Return the relative path of PATH in one of `docsim-search-paths'.

PATH is an absolute file path referencing a file located in one
of the `docsim-search-paths'. This search result must be nested
somewhere under one of the `docsim-search-paths'. This determines
which of those directories contains PATH and returns the path
relative to that directory."
  (cl-check-type path string)
  (file-relative-name (expand-file-name path)
                      (cl-find-if (lambda (search-path)
                                    (string-prefix-p (expand-file-name search-path)
                                                     (expand-file-name path)))
                                  (mapcar #'expand-file-name docsim-search-paths))))

(defun docsim--search-result-to-org (search-result)
  "Format SEARCH-RESULT as a line of Org markup for the results buffer.

If it's an Org document with a `#+title:', use that as the link
text. If not, do your best by showing the file path."
  (let* ((path (car search-result))
         (org-title (docsim--search-result-title path))
         (link-text (or org-title (docsim--relative-path path)))
         (link (format "[[file:%s][%s]]" path link-text))
         (score (if docsim-show-scores
                    (format "%s :: " (cdr search-result))
                  "")))
    (format "- %s%s" score link)))

(defun docsim--denote-ids-in-string (s)
  "Given a string S, return every string that plausible matches a Denote ID."
  (cl-check-type s string)
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

(defun docsim--denote-ids-in-buffer (buffer)
  "Given a buffer BUFFER, return every string that plausible matches a Denote ID."
  (cl-check-type buffer buffer)
  (docsim--denote-ids-in-string (with-current-buffer buffer
                                  (buffer-string))))

(defun docsim--similar-notes-org (search-results)
  "Return Org-formatted string of SEARCH-RESULTS."
  (cl-check-type search-results docsim-score-list)
  (concat "Similar notes:\n\n"
          (mapconcat #'identity
                     (mapcar #'docsim--search-result-to-org search-results)
                     "\n")))

(defun docsim--remove-denote-links (buffer search-results)
  "Return SEARCH-RESULTS excluding notes already linked from BUFFER."
  (cl-check-type buffer buffer)
  (cl-check-type search-results docsim-score-list)
  (let ((linked-denote-ids (docsim--denote-ids-in-buffer buffer)))
    (cl-remove-if (lambda (search-result)
                    (cl-find-if (lambda (denote-id)
                                  (string-match-p denote-id (car search-result)))
                                linked-denote-ids))
                  search-results)))

(defun docsim--limit-results (search-results)
  "Return SEARCH-RESULTS with no more than `docsim-limit' results.

Return them all if `docsim-limit' is nil."
  (cl-check-type search-results docsim-score-list)
  (if (and docsim-limit
           (> (length search-results) docsim-limit))
          (cl-subseq search-results 0 docsim-limit)
    search-results))

(defun docsim--query (query)
  "Return list of search-results from searching for object QUERY.

QUERY may be either a string or a buffer.

Include no more that `docsim-limit' results, and omit any results
that already seem to be linked from QUERY, if (1) it's a buffer
backed by a file and (2) `docsim-denote-omit-links' is t."
  (cl-check-type query docsim-query)
  (let ((search-results (docsim--similarity-results query)))
    (docsim--limit-results
     (if (and (bufferp query) docsim-omit-denote-links)
         (docsim--remove-denote-links query search-results)
       search-results))))

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

(defun docsim--parse-search-result (line)
  "Parse a LINE of `docsim' results into a search-result pair."
  (cl-check-type line string)
  (let* ((score (car (split-string line "\t")))
         (path (substring line (1+ (length score)))))
    (cons path score)))

(defun docsim--version ()
  "Get current version of the `docsim-executable'."
  (string-trim (shell-command-to-string (format "%s --version" docsim-executable))))

(defun docsim--quote-path (path)
  "Wrap PATH in quotes for interpolation into a shell command."
  (cl-check-type path string)
  (format "\"%s\"" (file-truename path)))

(defun docsim--stemming-stoplist-flags ()
  "Return a list of stemming- and stoplist-related flags for the shell command."
  (if docsim-assume-english
      (when docsim-stoplist-path
        `("--stoplist" ,(docsim--quote-path docsim-stoplist-path)))
    (if (not docsim-stoplist-path)
        '("--no-stemming" "--no-stoplist")
      `("--no-stemming" "--stoplist" ,(docsim--quote-path docsim-stoplist-path)))))

(defun docsim--raw-shell-results-from-buffer (buffer)
  "Pipe contents of BUFFER to `docsim' executable and return a string of results."
  (cl-check-type buffer buffer)
  (let ((shell-command
         (mapconcat #'identity
                    `(,docsim-executable
                      "--best-first"
                      "--show-scores"
                      ,@(when (not (string= (docsim--version) "0.1.3")) '("--stdin"))
                      ,@(docsim--stemming-stoplist-flags)
                      ,@(mapcar #'docsim--quote-path docsim-search-paths))
                    " ")))

    (with-output-to-string
      (shell-command-on-region (point-min)
                               (point-max)
                               shell-command
                               standard-output))))

(defun docsim--raw-shell-results (query)
  "Run a `docsim' search on QUERY and return a string of results.

QUERY is either a string containing search terms or a buffer. If
it's a buffer, treat its contents as the query (or, if it's
backed by a file, pass that file as an argument to `docsim')."
  (cl-check-type query docsim-query)
  (cond
   ((stringp query)
    (with-temp-buffer
      (insert query)
      (docsim--raw-shell-results-from-buffer (current-buffer))))

   ((and (bufferp query) (not (buffer-file-name query)))
    (docsim--raw-shell-results-from-buffer query))

   ((bufferp query)
    (shell-command-to-string
     (mapconcat #'identity
                `(,docsim-executable
                  "--best-first"
                  "--omit-query"
                  "--show-scores"
                  ,@(docsim--stemming-stoplist-flags)
                  ,(if (string= (docsim--version) "0.1.3") "--query" "--file")
                  ,(docsim--quote-path (buffer-file-name query))
                  ,@(mapcar #'docsim--quote-path docsim-search-paths))
                " ")))

   (t (error "Not a queryable object: %s" query))))

(defun docsim--similarity-results (query)
  "Return a list of search-result pairs for QUERY.

QUERY may be either a string or a buffer."
  (cl-check-type query docsim-query)
  (let ((result (docsim--raw-shell-results query)))
    (if (string-empty-p result)
        (error "No searchable notes found!")
      (mapcar #'docsim--parse-search-result
              (split-string (substring result 0 -1) "\n")))))

(defun docsim--show-results-buffer (search-results query)
  "Pop up buffer listing formatted SEARCH-RESULTS for QUERY."
  (cl-check-type search-results docsim-score-list)
  (cl-check-type query docsim-query)
  (let* ((buffer-name (format "*docsim: %s*" (if (bufferp query)
                                                 query
                                               (string-trim query))))
         (sidebar-buffer (get-buffer-create buffer-name)))
    (with-current-buffer sidebar-buffer
      (setq-local buffer-read-only nil)
      (erase-buffer)
      (insert (docsim--similar-notes-org search-results))
      (docsim-mode)
      (goto-char (point-min)))

    (pop-to-buffer sidebar-buffer)))

(defun docsim--read-search-term ()
  "Read a search term from the minibuffer.

Return the region, if it's active. Otherwise prompt, defaulting
to the symbol at point."
  (if (use-region-p)
      (buffer-substring (region-beginning) (region-end))
    (read-string "Query: " nil 'docsim-history)))

(defun docsim-search (query)
  "Search for notes similar to QUERY.

This calls out to the external `docsim' tool to perform textual
analysis on all the notes in `docsim-search-paths', score them by
similarity to QUERY, and return the sorted results, best first.

Include the similarity scores (between 0.0 and 1.0) of each note
if `docsim-show-scores' is non-nil.

Show at most `docsim-limit' results (or all of them, if
`docsim-limit' is nil)."
  (interactive (list (docsim--read-search-term)))
  (cl-check-type query string)
  (if (string-empty-p query)
      (error "Can't search with an empty query")
      (docsim--show-results-buffer (docsim--query query) query)))

(defun docsim-search-buffer (buffer)
  "Display a list of notes that look similar to BUFFER.

This calls out to the external `docsim' tool to perform textual
analysis on all the notes in `docsim-search-paths', score them by
similarity to BUFFER, and return the sorted results, best first.

Include the similarity scores (between 0.0 and 1.0) of each note
if `docsim-show-scores' is non-nil.

Show at most `docsim-limit' results (or all of them, if
`docsim-limit' is nil).

If `docsim-omit-denote-links' is non-nil, don't include files
that seem to be already linked from BUFFER. This can be helpful
for identifying files that \"should\" be linked but aren't yet."
  (interactive (list (current-buffer)))
  (cl-check-type buffer buffer)
  (docsim--show-results-buffer (docsim--query buffer) buffer))

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

(provide 'docsim)

;;; docsim.el ends here

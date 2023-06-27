;;; docsim-test.el --- tests for docsim.el

;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Some minimal tests for a few trickier bits of logic.

;;; Code:

(require 'buttercup)
(load-file "docsim.el")

(defun docsim--test-file (content suffix)
  (let ((temp-file (make-temp-file "docsim-test-" nil suffix)))
    (with-temp-file temp-file
      (insert content)
      temp-file)))

(describe "docsim--get-title-yaml"
          (it "returns the title: from YAML frontmatter"
              (with-temp-buffer
                (insert "---\n"
                        "title: Title of the document!\n"
                        "date: today!\n"
                        "---\n"
                        "some stuff in the body\n")
                (expect (docsim--get-title-yaml) :to-equal "Title of the document!")))

          (it "doesn't read a title: keyword if it's not in YAML frontmatter"
              (with-temp-buffer
                (insert "---\n"
                        "date: today!\n"
                        "---\n"
                        "title: This isn't in the metadata block!\n"
                        "some stuff in the body\n")
                (expect (docsim--get-title-yaml) :to-be nil))))

(describe "docsim--get-title-org"
          (it "returns the value of the #+title: keyword, if there is one"
              (with-temp-buffer
                (insert "#+title: Title of the document!\n"
                        "#+date: today!\n"
                        "some stuff in the body\n")
                (expect (docsim--get-title-org) :to-equal "Title of the document!")))

          (it "returns nil if there's no #+title: keyword"
              (with-temp-buffer
                (insert "#+author: It's me!\n"
                        "#+date: today!\n"
                        "some stuff in the body\n")
                (expect (docsim--get-title-org) :to-be nil))))

(describe "docsim--get-title-function-default"
          (it "returns the title: from YAML frontmatter when given a Markdown document"
              (let* ((content (mapconcat 'identity
                                         '("---"
                                           "title: Title of the document!"
                                           "date: today!"
                                           "---"
                                           "some stuff in the body")
                                         "\n"))
                     (path (docsim--test-file content ".md")))
                (expect (docsim--get-title-function-default path) :to-equal "Title of the document!")))

          (it "returns the #+title: when given an Org document"
              (let* ((content (mapconcat 'identity
                             '("#+title: Title of the document!"
                               "#+date: today!"
                               ""
                               "some stuff in the body")
                             "\n"))
                     (path (docsim--test-file content ".org")))
                (expect (docsim--get-title-function-default path) :to-equal "Title of the document!")))

          (it "returns nil if it can't parse a title"
              (let* ((path (docsim--test-file "no title in here" ".txt")))
                (expect (docsim--get-title-function-default path) :to-be nil))))

(describe "docsim--relative-path"
          (it "handles tilde expansion"
              (let* ((docsim-search-paths '("~/documents/notes"
                                            "~/documents/blog")))
                (expect (docsim--relative-path "~/documents/notes/foo/bar.org")
                        :to-equal "foo/bar.org")))

          (it "handles absolute paths"
              (let* ((docsim-search-paths '("~/documents/notes"
                                            "~/documents/blog")))
                (expect (docsim--relative-path (expand-file-name "~/documents/blog/_posts/docsim.md"))
                        :to-equal "_posts/docsim.md"))))

(defvar-local plain-lines '("Here's some content! Doesn't really matter what's"
                            "in it. Though let's make sure a line starts with"
                            "title just in case."))

(describe "docsim--search-result-to-org"
          (it "can handle a user-specified function"
              (cl-flet ((get-title-function (path) "FAKE TITLE"))

                (let ((docsim-get-title-function #'get-title-function)
                      (path (docsim--test-file "#+title: real title" ".org"))
                      (docsim-show-scores nil))
                  (expect (docsim--search-result-to-org (cons path "0.4242"))
                          :to-equal (format "- [[file:%s][FAKE TITLE]]" path)))))

          (describe "displaying the relative path to a plain text file"
                    (it "shows scores if docsim-show-scores is t"
                        (let* ((content (mapconcat 'identity plain-lines "\n"))
                               (path (docsim--test-file content ".txt"))
                               (docsim-search-paths (list (file-name-directory path)))
                               (plain-search-result (cons path "0.4242"))
                               (docsim-show-scores t))
                          (expect (docsim--search-result-to-org plain-search-result)
                                  :to-equal
                                  (format "- 0.4242 :: [[file:%s][%s]]"
                                          path
                                          (file-name-nondirectory path)))))

                    (it "doesn't show scores if docsim-show-scores is nil"
                        (let* ((content (mapconcat 'identity plain-lines "\n"))
                               (path (docsim--test-file content ".txt"))
                               (docsim-search-paths (list (file-name-directory path)))
                               (plain-search-result (cons path "0.4242"))
                               (docsim-show-scores nil))
                          (expect (docsim--search-result-to-org plain-search-result)
                                  :to-equal
                                  (format "- [[file:%s][%s]]"
                                          path
                                          (file-name-nondirectory path))))))

          (describe "displaying the title of an Org file"
                    (it "shows scores if docsim-show-scores is t"
                        (let* ((content (mapconcat 'identity (cons "#+title: It's an Org file!" plain-lines) "\n"))
                               (path (docsim--test-file content ".org"))
                               (org-search-result (cons path "0.4242"))
                               (docsim-show-scores nil))
                          (expect (docsim--search-result-to-org org-search-result)
                                  :to-equal (format "- [[file:%s][It's an Org file!]]" path))))

                    (it "doesn't show scores if docsim-show-scores is nil"
                        (let* ((content (mapconcat 'identity (cons "#+title: It's an Org file!" plain-lines) "\n"))
                               (path (docsim--test-file content ".org"))
                               (org-search-result (cons path "0.4242"))
                               (docsim-show-scores t))
                          (expect (docsim--search-result-to-org org-search-result)
                                  :to-equal
                                  (format "- 0.4242 :: [[file:%s][It's an Org file!]]" path))))))

          (describe "displaying the title of a file with YAML frontmatter"
                    (it "shows scores if docsim-show-scores is t"
                        (let* ((content (mapconcat 'identity
                                                   (append '("---"
                                                             "title: It's a Markdown file!"
                                                             "---")
                                                           plain-lines)
                                                   "\n"))
                               (path (docsim--test-file content ".md"))
                               (markdown-search-result (cons path "0.4242"))
                               (docsim-show-scores t)
                               (docsim-show-titles t))
                          (expect (docsim--search-result-to-org markdown-search-result)
                                  :to-equal
                                  (format "- 0.4242 :: [[file:%s][It's a Markdown file!]]" path))))

                    (it "doesn't show scores if docsim-show-scores is nil"
                        (let* ((content (mapconcat 'identity
                                                   (append '("---"
                                                             "title: It's a Markdown file!"
                                                             "---")
                                                           plain-lines)
                                                   "\n"))
                               (path (docsim--test-file content ".md"))
                               (markdown-search-result (cons path "0.4242"))
                               (docsim-show-scores nil))
                          (expect (docsim--search-result-to-org markdown-search-result)
                                  :to-equal
                                  (format "- [[file:%s][It's a Markdown file!]]" path))))

                    (it "shows relative paths instead of titles if docsim-show-titles is nil"
                        (let* ((content (mapconcat 'identity
                                                   (append '("---"
                                                             "title: It's a Markdown file!"
                                                             "---")
                                                           plain-lines)
                                                   "\n"))
                               (path (docsim--test-file content ".md"))
                               (markdown-search-result (cons path "0.4242"))
                               (docsim-show-scores t)
                               (docsim-show-titles nil)
                               (docsim-search-paths (list (file-name-directory path))))
                          (expect (docsim--search-result-to-org markdown-search-result)
                                  :to-equal
                                  (format "- 0.4242 :: [[file:%s][%s]]"
                                          path
                                          (docsim--relative-path (expand-file-name path)))))))

(defvar-local text-with-denote-links
    (mapconcat 'identity
               '("This note [[denote:20230531T143312][has many]] Denote"
                 "links [[denote:20210426T185629]] in [[denote:92830384T918347][it]]."
                 "And some things 10384718T204817 that aren't!")
               "\n"))

(describe "docsim--denote-ids-in-string"
          (it "returns IDs of linked Denote notes in a string"
              (expect (docsim--denote-ids-in-string text-with-denote-links)
                      :to-equal
                      '("92830384T918347" "20210426T185629" "20230531T143312"))))

(describe "docsim--denote-ids-in-buffer"
          (it "returns IDs of linked Denote notes in a buffer"
              (with-temp-buffer
                (insert text-with-denote-links)
                (expect (docsim--denote-ids-in-buffer (current-buffer))
                        :to-equal
                        '("92830384T918347" "20210426T185629" "20230531T143312")))))

(describe "docsim--remove-denote-links"
          (it "filters search results to remove already-linked Denote notes"
              (let* ((docsim-omit-denote-links t)
                     (path (docsim--test-file text-with-denote-links ".org"))
                     (search-result-linked '("/tmp/foo/20210426T185629--linked-note.org" . "0.4242"))
                     (search-result-unlinked '("/tmp/foo/20201114T143209--unlinked-note.org" . "0.4242")))
                (with-temp-buffer
                  (insert text-with-denote-links)
                  (expect (docsim--remove-denote-links (current-buffer)
                                                       (list search-result-linked search-result-unlinked))
                          :to-equal
                          (list search-result-unlinked))))))

(describe "docsim--limit-results"
          (it "returns the number of results supplied if given enough"
              (let ((docsim-limit 3))
                (expect (docsim--limit-results '(("a" . "0.4242")
                                                 ("b" . "0.4242")
                                                 ("c" . "0.4242")
                                                 ("d" . "0.4242")
                                                 ("e" . "0.4242")
                                                 ("f" . "0.4242")
                                                 ("g" . "0.4242")))
                        :to-equal '(("a" . "0.4242")
                                    ("b" . "0.4242")
                                    ("c" . "0.4242")))))

          (it "returns all the results if there aren't enough"
              (let ((docsim-limit 3))
                (expect (docsim--limit-results '(("a" . "0.4242") ("b" . "0.4242")))
                        :to-equal '(("a" . "0.4242") ("b" . "0.4242")))))

          (it "returns an empty list if given one"
              (let ((docsim-limit 3))
                (expect (docsim--limit-results '()) :to-equal '())))

          (it "returns all the results if docsim-limit is nil"
              (let ((docsim-limit nil))
                (expect (docsim--limit-results '(("a" . "0.4242")
                                                 ("b" . "0.4242")
                                                 ("c" . "0.4242")
                                                 ("d" . "0.4242")
                                                 ("e" . "0.4242")
                                                 ("f" . "0.4242")
                                                 ("g" . "0.4242")))
                        :to-equal
                        '(("a" . "0.4242")
                          ("b" . "0.4242")
                          ("c" . "0.4242")
                          ("d" . "0.4242")
                          ("e" . "0.4242")
                          ("f" . "0.4242")
                          ("g" . "0.4242"))))))

(describe "docsim--parse-search-result"
          (it "parses a \"regular\" line of docsim output"
              (expect (docsim--parse-search-result "0.4242\t/tmp/foo/bar.org")
                      :to-equal
                      (cons "/tmp/foo/bar.org" "0.4242")))

          (it "parses an unexpected score length"
              (expect (docsim--parse-search-result "0.42424242\t/tmp/foo/Hello Foo.org")
                      :to-equal
                      (cons "/tmp/foo/Hello Foo.org" "0.42424242")))

          (it "parses a path with a space in it"
              (expect (docsim--parse-search-result "0.4242\t/tmp/foo/Hello Foo.org")
                      :to-equal
                      (cons "/tmp/foo/Hello Foo.org" "0.4242")))

          (it "parses a path with a tab in it"
              (expect (docsim--parse-search-result "0.4242\t/tmp/foo/Hello\tFoo.org")
                      :to-equal
                      (cons "/tmp/foo/Hello\tFoo.org" "0.4242"))))

(describe "docsim--quote-path"
          (it "wraps a path in quotes"
              (expect (docsim--quote-path "/usr/local/foo")
                      :to-equal
                      "\"/usr/local/foo\""))

          (it "handles spaces in filenames"
              (expect (docsim--quote-path "/usr/local/Hello World")
                      :to-equal
                      "\"/usr/local/Hello World\"")))

(provide 'docsim-test)

;;; docsim-test.el ends here

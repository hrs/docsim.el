;;; docsim-test.el --- tests for docsim.el

;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Some minimal tests for a few trickier bits of logic.

;;; Code:

(load-file "docsim.el")

(defun docsim--test-file (content suffix)
  (let ((temp-file (make-temp-file "docsim-test-" nil suffix)))
    (with-temp-file temp-file
      (insert content)
      temp-file)))

(ert-deftest docsim--get-title-markdown-yaml-test ()
  (with-temp-buffer
    (insert "---\n"
            "title: Title of the document!\n"
            "date: today!\n"
            "---\n"
            "some stuff in the body\n")
    (should (equal (docsim--get-title-markdown-yaml)
                   "Title of the document!")))

  (with-temp-buffer
    (insert "---\n"
            "date: today!\n"
            "---\n"
            "title: This isn't in the metadata block!\n"
            "some stuff in the body\n")
    (should (equal (docsim--get-title-markdown-yaml) nil))))

(ert-deftest docsim--get-title-org-test ()
  (with-temp-buffer
    (insert "#+title: Title of the document!\n"
            "#+date: today!\n"
            "some stuff in the body\n")
    (should (equal (docsim--get-title-org)
                   "Title of the document!")))

  (with-temp-buffer
    (insert "#+author: It's me!\n"
            "#+date: today!\n"
            "some stuff in the body\n")
    (should (equal (docsim--get-title-org) nil))))

(ert-deftest docsim--get-title-function-default-test ()
  ;; Gets title from YAML metadata in a Markdown file.
  (let* ((content (mapconcat 'identity
                             '("---"
                               "title: Title of the document!"
                               "date: today!"
                               "---"
                               "some stuff in the body")
                             "\n"))
         (path (docsim--test-file content ".md")))
    (should (equal (docsim--get-title-function-default path)
                   "Title of the document!")))

  ;; Gets title from an Org file.
  (let* ((content (mapconcat 'identity
                             '("#+title: Title of the document!"
                               "#+date: today!"
                               ""
                               "some stuff in the body")
                             "\n"))
         (path (docsim--test-file content ".org")))
    (should (equal (docsim--get-title-function-default path)
                   "Title of the document!")))

  ;; Returns nil if it can't parse a title.
  (let* ((path (docsim--test-file "no title in here" ".txt")))
    (should (equal (docsim--get-title-function-default path)
                   nil))))

(ert-deftest docsim--get-title-function-custom-test ()
  (cl-flet ((get-title-function (path) "FAKE TITLE"))

    (let ((docsim-get-title-function #'get-title-function)
          (path (docsim--test-file "#+title: real title" ".org"))
          (docsim-show-scores nil))

      (should (equal (docsim--search-result-to-org (cons path "0.4242"))
                     (format "- [[file:%s][FAKE TITLE]]" path))))))

(defvar-local plain-lines '("Here's some content! Doesn't really matter what's"
                            "in it. Though let's make sure a line starts with"
                            "title just in case."))

(ert-deftest docsim--search-result-to-org-with-plain-text-test ()
  (let* ((content (mapconcat 'identity plain-lines "\n"))
         (path (docsim--test-file content ".txt"))
         (docsim-search-paths (list (file-name-directory path)))
         (plain-search-result (cons path "0.4242")))

    (let ((docsim-show-scores nil))
      (should (equal (docsim--search-result-to-org plain-search-result)
                     (format "- [[file:%s][%s]]"
                             path
                             (file-name-nondirectory path)))))

    (let ((docsim-show-scores t))
      (should (equal (docsim--search-result-to-org plain-search-result)
                     (format "- 0.4242 :: [[file:%s][%s]]"
                             path
                             (file-name-nondirectory path)))))))

(ert-deftest docsim--relative-path-test ()
   (let* ((docsim-search-paths '("~/documents/notes"
                                 "~/documents/blog")))
     (should (equal (docsim--relative-path "~/documents/notes/foo/bar.org")
                    "foo/bar.org"))

     (should (equal (docsim--relative-path (expand-file-name "~/documents/blog/_posts/docsim.md"))
                    "_posts/docsim.md"))))

(ert-deftest docsim--search-result-to-org-with-org-title-test ()
  (let* ((content (mapconcat 'identity (cons "#+title: It's an Org file!" plain-lines) "\n"))
         (path (docsim--test-file content ".org"))
         (org-search-result (cons path "0.4242")))

    (let ((docsim-show-scores nil))
      (should (equal (docsim--search-result-to-org org-search-result)
                     (format "- [[file:%s][It's an Org file!]]" path))))

    (let ((docsim-show-scores t))
      (should (equal (docsim--search-result-to-org org-search-result)
                     (format "- 0.4242 :: [[file:%s][It's an Org file!]]" path))))))

(ert-deftest docsim--search-result-to-org-with-markdown-yaml-title-test ()
  (let* ((content (mapconcat 'identity
                             (append '("---"
                                       "title: It's a Markdown file!"
                                       "---")
                                     plain-lines)
                             "\n"))
         (path (docsim--test-file content ".md"))
         (markdown-search-result (cons path "0.4242")))

    (let ((docsim-show-scores nil))
      (should (equal (docsim--search-result-to-org markdown-search-result)
                     (format "- [[file:%s][It's a Markdown file!]]" path))))

    (let ((docsim-show-scores t)
          (docsim-show-titles t))
      (should (equal (docsim--search-result-to-org markdown-search-result)
                     (format "- 0.4242 :: [[file:%s][It's a Markdown file!]]" path))))

    (let ((docsim-show-scores t)
          (docsim-show-titles nil)
          (docsim-search-paths (list (file-name-directory path))))
      (should (equal (docsim--search-result-to-org markdown-search-result)
                     (format "- 0.4242 :: [[file:%s][%s]]"
                             path
                             (docsim--relative-path (expand-file-name path))))))))

(defvar-local text-with-denote-links
    (mapconcat 'identity
               '("This note [[denote:20230531T143312][has many]] Denote"
                 "links [[denote:20210426T185629]] in [[denote:92830384T918347][it]]."
                 "And some things 10384718T204817 that aren't!")
               "\n"))

(ert-deftest docsim--denote-ids-in-string-test ()
  (should (equal (docsim--denote-ids-in-string text-with-denote-links)
                 '("92830384T918347" "20210426T185629" "20230531T143312"))))

(ert-deftest docsim--denote-ids-in-buffer-test ()
  (with-temp-buffer
    (insert text-with-denote-links)
    (should (equal (docsim--denote-ids-in-buffer (current-buffer))
                   '("92830384T918347" "20210426T185629" "20230531T143312")))))

(ert-deftest docsim--remove-denote-links-test ()
  (let* ((docsim-omit-denote-links t)
         (path (docsim--test-file text-with-denote-links ".org"))
         (search-result-linked '("/tmp/foo/20210426T185629--linked-note.org" . "0.4242"))
         (search-result-unlinked '("/tmp/foo/20201114T143209--unlinked-note.org" . "0.4242")))
    (with-temp-buffer
      (insert text-with-denote-links)
    (should (equal (docsim--remove-denote-links (current-buffer)
                                                (list search-result-linked search-result-unlinked))
                   (list search-result-unlinked))))))

(ert-deftest docsim--limit-results-test ()
  (let ((docsim-limit 3))
    (should (equal (docsim--limit-results '(("a" . "0.4242")
                                            ("b" . "0.4242")
                                            ("c" . "0.4242")
                                            ("d" . "0.4242")
                                            ("e" . "0.4242")
                                            ("f" . "0.4242")
                                            ("g" . "0.4242")))
                   '(("a" . "0.4242") ("b" . "0.4242") ("c" . "0.4242"))))

    (should (equal (docsim--limit-results '(("a" . "0.4242") ("b" . "0.4242")))
                   '(("a" . "0.4242") ("b" . "0.4242"))))

    (should (equal (docsim--limit-results '())
                   '())))

  (let ((docsim-limit nil))
    (should (equal (docsim--limit-results '(("a" . "0.4242")
                                            ("b" . "0.4242")
                                            ("c" . "0.4242")
                                            ("d" . "0.4242")
                                            ("e" . "0.4242")
                                            ("f" . "0.4242")
                                            ("g" . "0.4242")))
                   '(("a" . "0.4242")
                     ("b" . "0.4242")
                     ("c" . "0.4242")
                     ("d" . "0.4242")
                     ("e" . "0.4242")
                     ("f" . "0.4242")
                     ("g" . "0.4242"))))))

(ert-deftest docsim--parse-search-result-test ()
  ;; A "regular" line of docsim output.
  (should (equal (docsim--parse-search-result "0.4242\t/tmp/foo/bar.org")
                 (cons "/tmp/foo/bar.org" "0.4242")))

  ;; An unexpected score length.
  (should (equal (docsim--parse-search-result "0.42424242\t/tmp/foo/Hello Foo.org")
                 (cons "/tmp/foo/Hello Foo.org" "0.42424242")))

  ;; A path with a space in it.
  (should (equal (docsim--parse-search-result "0.4242\t/tmp/foo/Hello Foo.org")
                 (cons "/tmp/foo/Hello Foo.org" "0.4242")))

  ;; A path with a tab in it.
  (should (equal (docsim--parse-search-result "0.4242\t/tmp/foo/Hello\tFoo.org")
                 (cons "/tmp/foo/Hello\tFoo.org" "0.4242"))))

(ert-deftest docsim--quote-path-test ()
  (should (equal (docsim--quote-path "/usr/local/foo")
                 "\"/usr/local/foo\""))

  (should (equal (docsim--quote-path "/usr/local/Hello World")
                 "\"/usr/local/Hello World\"")))

(provide 'docsim-test)

;;; docsim-test.el ends here

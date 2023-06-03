;;; docsim-test.el --- tests for docsim.el

;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Some minimal tests for a few trickier bits of logic.

;;; Code:

(load-file "docsim.el")

(defun docsim--test-file (content)
  (let ((temp-file (make-temp-file "docsim-test")))
    (with-temp-file temp-file
      (insert content)
      temp-file)))

(defvar-local plain-lines '("Here's some content! Doesn't really matter what's"
                            "in it. Though let's make sure a line starts with"
                            "title just in case."))

(ert-deftest docsim--record-to-org-with-plain-text-test ()
  (let* ((content (mapconcat 'identity plain-lines "\n"))
         (path (docsim--test-file content))
         (plain-record (cons path "0.4242")))

    (let ((docsim-show-scores nil))
      (should (equal (docsim--record-to-org plain-record)
                     (format "- [[file:%s][%s]]" path path))))

    (let ((docsim-show-scores t))
      (should (equal (docsim--record-to-org plain-record)
                     (format "- 0.4242 :: [[file:%s][%s]]" path path))))))

(ert-deftest docsim--record-to-org-with-org-title-test ()
  (let* ((content (mapconcat 'identity (cons "#+title: It's an Org file!" plain-lines) "\n"))
         (path (docsim--test-file content))
         (org-record (cons path "0.4242")))

    (let ((docsim-show-scores nil))
      (should (equal (docsim--record-to-org org-record)
                     (format "- [[file:%s][It's an Org file!]]" path))))

    (let ((docsim-show-scores t))
      (should (equal (docsim--record-to-org org-record)
                     (format "- 0.4242 :: [[file:%s][It's an Org file!]]" path))))))

(ert-deftest docsim--record-to-org-with-markdown-yaml-title-test ()
  (let* ((content (mapconcat 'identity
                             (append '("---"
                                       "title: It's a Markdown file!"
                                       "---")
                                     plain-lines)
                             "\n"))
         (path (docsim--test-file content))
         (markdown-record (cons path "0.4242")))

    (let ((docsim-show-scores nil))
      (should (equal (docsim--record-to-org markdown-record)
                     (format "- [[file:%s][It's a Markdown file!]]" path))))

    (let ((docsim-show-scores t))
      (should (equal (docsim--record-to-org markdown-record)
                     (format "- 0.4242 :: [[file:%s][It's a Markdown file!]]" path))))))

(defvar-local text-with-denote-links
    (mapconcat 'identity
               '("This note [[denote:20230531T143312][has many]] Denote"
                 "links [[denote:20210426T185629]] in [[denote:92830384T918347][it]]."
                 "And some things 10384718T204817 that aren't!")
               "\n"))

(ert-deftest docsim--denote-ids-in-string-test ()
  (should (equal (docsim--denote-ids-in-string text-with-denote-links)
                 '("92830384T918347" "20210426T185629" "20230531T143312"))))

(ert-deftest docsim--denote-ids-in-file-test ()
  (let ((path (docsim--test-file text-with-denote-links)))
    (should (equal (docsim--denote-ids-in-file path)
                   '("92830384T918347" "20210426T185629" "20230531T143312")))))

(ert-deftest docsim--remove-denote-links-test ()
  (let* ((docsim-omit-denote-links t)
         (path (docsim--test-file text-with-denote-links))
         (record-linked (cons "/tmp/foo/20210426T185629--linked-note.org" 0.0))
         (record-unlinked (cons "/tmp/foo/20201114T143209--unlinked-note.org" 0.0)))
    (should (equal (docsim--remove-denote-links path (list record-linked record-unlinked))
                   (list record-unlinked)))))

(ert-deftest docsim--limit-results-test ()
  (let ((docsim-limit 3))
    (should (equal (docsim--limit-results '(a b c d e f g))
                   '(a b c)))

    (should (equal (docsim--limit-results '(a b))
                   '(a b)))

    (should (equal (docsim--limit-results '())
                   '())))

  (let ((docsim-limit nil))
    (should (equal (docsim--limit-results '(a b c d e f g))
                   '(a b c d e f g)))))

(ert-deftest docsim--parse-record-test ()
  ;; A "regular" line of docsim output.
  (should (equal (docsim--parse-record "0.4242\t/tmp/foo/bar.org")
                 (cons "/tmp/foo/bar.org" "0.4242")))

  ;; An unexpected score length.
  (should (equal (docsim--parse-record "0.42424242\t/tmp/foo/Hello Foo.org")
                 (cons "/tmp/foo/Hello Foo.org" "0.42424242")))

  ;; A path with a space in it.
  (should (equal (docsim--parse-record "0.4242\t/tmp/foo/Hello Foo.org")
                 (cons "/tmp/foo/Hello Foo.org" "0.4242")))

  ;; A path with a tab in it.
  (should (equal (docsim--parse-record "0.4242\t/tmp/foo/Hello\tFoo.org")
                 (cons "/tmp/foo/Hello\tFoo.org" "0.4242"))))

(ert-deftest docsim--quote-path-test ()
  (should (equal (docsim--quote-path "/tmp/foo")
                 "\"/tmp/foo\""))

  (should (equal (docsim--quote-path "/tmp/Hello World")
                 "\"/tmp/Hello World\"")))

(ert-deftest docsim--shell-command-test ()
  (let ((docsim-executable "docsim")
        (docsim-search-paths '("/tmp/foo" "/tmp/bar")))

    (let ((docsim-assume-english nil)
          (docsim-stoplist-path nil))
      (should (equal (docsim--shell-command "/tmp/query")
                     "docsim --best-first --omit-query --show-scores --no-stemming --no-stoplist --query \"/tmp/query\" \"/tmp/foo\" \"/tmp/bar\"")))

    (let ((docsim-assume-english nil)
          (docsim-stoplist-path "/tmp/stoplist"))
      (should (equal (docsim--shell-command "/tmp/query")
                     "docsim --best-first --omit-query --show-scores --no-stemming --stoplist \"/tmp/stoplist\" --query \"/tmp/query\" \"/tmp/foo\" \"/tmp/bar\"")))

    (let ((docsim-assume-english t)
          (docsim-stoplist-path nil))
      (should (equal (docsim--shell-command "/tmp/query")
                     "docsim --best-first --omit-query --show-scores --query \"/tmp/query\" \"/tmp/foo\" \"/tmp/bar\"")))

    (let ((docsim-assume-english t)
          (docsim-stoplist-path "/tmp/stoplist"))
      (should (equal (docsim--shell-command "/tmp/query")
                     "docsim --best-first --omit-query --show-scores --stoplist \"/tmp/stoplist\" --query \"/tmp/query\" \"/tmp/foo\" \"/tmp/bar\"")))))

(provide 'docsim-test)

;;; docsim-test.el ends here

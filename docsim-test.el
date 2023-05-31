;;; docsim-test.el --- tests for docsim.el

;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Some minimal tests for a few trickier bits of logic.

;;; Code:

(load-file "docsim.el")

(ert-deftest docsim--record-to-org-test ()
  (let ((plain-lines '("Here's some content! Doesn't really matter what's"
                       "in it. Though let's make sure a line starts with"
                       "title just in case."))
        (score "0.4242"))

    ;; A plain text file without any interesting features.
    (let* ((content (s-join "\n" plain-lines))
           (path (make-temp-file "docsim-test" nil nil content))
           (plain-record (make-docsim--record :path path :score score)))

      (let ((docsim-show-scores nil))
        (should (equal (docsim--record-to-org plain-record)
                       (format "- [[file:%s][%s]]" path path))))

      (let ((docsim-show-scores t))
        (should (equal (docsim--record-to-org plain-record)
                       (format "- 0.4242 :: [[file:%s][%s]]" path path)))))

    ;; An Org document with a "#+title" keyword.
    (let* ((content (s-join "\n" (cons "#+title: It's an Org file!" plain-lines)))
           (path (make-temp-file "docsim-test" nil nil content))
           (org-record (make-docsim--record :path path :score score)))

      (let ((docsim-show-scores nil))
        (should (equal (docsim--record-to-org org-record)
                       (format "- [[file:%s][It's an Org file!]]" path))))

      (let ((docsim-show-scores t))
        (should (equal (docsim--record-to-org org-record)
                       (format "- 0.4242 :: [[file:%s][It's an Org file!]]" path)))))

    ;; A Markdown file with some YAML front matter.
    (let* ((content (s-join "\n" (append '("---"
                                           "title: It's a Markdown file!"
                                           "---")
                                         plain-lines)))
           (path (make-temp-file "docsim-test" nil nil content))
           (markdown-record (make-docsim--record :path path :score score)))

      (let ((docsim-show-scores nil))
        (should (equal (docsim--record-to-org markdown-record)
                       (format "- [[file:%s][It's a Markdown file!]]" path))))

      (let ((docsim-show-scores t))
        (should (equal (docsim--record-to-org markdown-record)
                       (format "- 0.4242 :: [[file:%s][It's a Markdown file!]]" path)))))))


(ert-deftest docsim--denote-ids-in-string-test ()
  (let ((content (s-join "\n" '("This note [[denote:20230531T143312][has many]] Denote"
                                "links [[denote:20210426T185629]] in [[denote:92830384T918347][it]]."
                                "And some things 10384718T204817 that aren't!"))))
    (should (equal (docsim--denote-ids-in-string content)
                   '("20230531T143312" "20210426T185629" "92830384T918347")))))


(ert-deftest docsim--denote-ids-in-file-test ()
  (let* ((content (s-join "\n" '("This note [[denote:20230531T143312][has many]] Denote"
                                 "links [[denote:20210426T185629]] in [[denote:92830384T918347][it]]."
                                 "And some things 10384718T204817 that aren't!")))
         (path (make-temp-file "docsim-test" nil nil content)))
    (should (equal (docsim--denote-ids-in-file path)
                   '("20230531T143312" "20210426T185629" "92830384T918347")))))


(ert-deftest docsim--remove-denote-links-test ()
  (let* ((content (s-join "\n" '("This note [[denote:20230531T143312][has many]] Denote"
                                 "links [[denote:20210426T185629]] in [[denote:92830384T918347][it]]."
                                 "And some things 10384718T204817 that aren't!")))
         (file-name (make-temp-file "docsim-test" nil nil content))
         (record-linked (make-docsim--record
                         :path "/tmp/foo/20210426T185629--linked-note.org"
                         :score 0.0))
         (record-unlinked (make-docsim--record
                           :path "/tmp/foo/20201114T143209--unlinked-note.org"
                           :score 0.0)))
  (should (equal (docsim--remove-denote-links file-name (list record-linked record-unlinked))
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
  (let ((docsim-show-scores t))
    (let ((record (docsim--parse-record "0.4242\t/tmp/foo/bar.org")))
      (should (equal (docsim--record-score record)
                     "0.4242"))
      (should (equal (docsim--record-path record)
                     "/tmp/foo/bar.org"))))

  (let ((record (docsim--parse-record "0.0000\t/tmp/foo/Hello Foo.org")))
    (should (equal (docsim--record-score record)
                   "0.0000"))
    (should (equal (docsim--record-path record)
                   "/tmp/foo/Hello Foo.org"))))


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

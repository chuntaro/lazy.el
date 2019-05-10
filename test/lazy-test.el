;;; -*- lexical-binding: t; -*-

;; $ cd /path/to/lazy.el
;; $ emacs -L . --batch -l ert -l test/lazy-test.el -f ert-run-tests-batch-and-exit

(require 'lazy)

(ert-deftest lz-test-error-check ()
  (should (null (lz-into-list (lz-range 10 5))))

  (should (null (lz-into-list (lz-take (lz-range) -1))))

  (should (null (lz-into-list (lz-take (lz-drop (lz-range) -1) 0))))

  (should (lz-stream-p (lz-range)))

  (should (null (lz-stream-p [1]))))

(ert-deftest lz-test-append ()
  (should (equal (lz-into-list (lz-append (lz-take (lz-range) 3) (lz-take (lz-range) 3)))
                 '(0 1 2 0 1 2)))

  (should (equal (lz-into-list (lz-take (lz-append (lz-range 0 3) (lz-range)) 6))
                 '(0 1 2 0 1 2)))

  (should (equal (lz-into-list (lz-append))
                 nil)))

(ert-deftest lz-test-pop ()
  (should (equal (let ((r (lz-range))) (list (lz-pop r) (lz-pop r) (lz-pop r)))
                 '(0 1 2))))

(ert-deftest lz-test-subseq ()
  (should (equal (lz-into-list (lz-subseq (lz-range) 5 10))
                 '(5 6 7 8 9))))

(ert-deftest lz-test-mapn ()
  (should (equal (lz-into-list (lz-take (lz-mapn #'string
                                                 (lz-range ?A)
                                                 (lz-range ?a)
                                                 (lz-range ?0))
                                        10))
                 '("Aa0" "Bb1" "Cc2" "Dd3" "Ee4" "Ff5" "Gg6" "Hh7" "Ii8" "Jj9"))))

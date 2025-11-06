;;; h5-test.el --- Unit tests for h5-io.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;;; Commentary:

;; Unit tests for h5-io.el using test data from scigolib/hdf5

;;; Code:

(require 'h5-io)
(require 'ert)

(defvar h5-test-data-dir
  (expand-file-name "testdata" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing HDF5 test files.")

;;; Test Utilities

(defun h5-test-list-all-objects (filename)
  "List all groups and datasets in FILENAME.
Returns a list of (path . type) pairs where type is 'group or 'dataset."
  (let ((result nil)
        (file (h5-io-open (expand-file-name filename h5-test-data-dir) nil)))
    (unwind-protect
        (h5-io-walk file
                   (lambda (path obj)
                     (cond
                      ((h5-io-group-p obj)
                       (push (cons path 'group) result))
                      ((h5-io-dataset-p obj)
                       (push (cons path 'dataset) result)))))
      (h5-io-close file))
    (nreverse result)))

;;; Tests

(ert-deftest h5-test-simple ()
  "Test reading simple.h5 file."
  (let ((objects (h5-test-list-all-objects "simple.h5")))
    (should (member '("/" . group) objects))
    (message "simple.h5 objects: %S" objects)))

(ert-deftest h5-test-with-groups ()
  "Test reading with_groups.h5 file."
  (let ((objects (h5-test-list-all-objects "with_groups.h5")))
    (should (member '("/" . group) objects))
    ;; Expected groups based on Go library testdata
    (message "with_groups.h5 objects: %S" objects)))

(ert-deftest h5-test-with-attributes ()
  "Test reading with_attributes.h5 file."
  (let ((objects (h5-test-list-all-objects "with_attributes.h5")))
    (should (member '("/" . group) objects))
    (message "with_attributes.h5 objects: %S" objects)))

(ert-deftest h5-test-v0 ()
  "Test reading v0.h5 (version 0 superblock)."
  (let* ((file (h5-io-open (expand-file-name "v0.h5" h5-test-data-dir) nil))
         (sb (h5-io-file-superblock file)))
    (unwind-protect
        (progn
          (should (eq (h5-io-superblock-version sb) 0))
          (message "v0.h5 superblock version: %d" (h5-io-superblock-version sb)))
      (h5-io-close file))))

;;; Run all tests

(defun h5-test-run-all ()
  "Run all h5-io tests."
  (interactive)
  (ert-run-tests-interactively "^h5-test-"))

(provide 'h5-test)
;;; h5-test.el ends here

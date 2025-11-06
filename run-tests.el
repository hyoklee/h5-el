#!/usr/bin/emacs --script
;;; run-tests.el --- Run h5-io tests from command line

(add-to-list 'load-path (file-name-directory load-file-name))
(require 'h5-io)
(require 'h5-test)

(message "\n=== Running H5-IO Tests ===\n")

;; Test 1: Simple file
(message "Test 1: simple.h5")
(condition-case err
    (let ((file (h5-io-open (expand-file-name "testdata/simple.h5" (file-name-directory load-file-name)) nil)))
      (message "  Superblock version: %d" (h5-io-superblock-version (h5-io-file-superblock file)))
      (message "  Root group: %s" (if (h5-io-file-root-group file) "found" "not found"))
      (message "  Walking file...")
      (h5-io-walk file
                 (lambda (path obj)
                   (message "    %s: %s" path (type-of obj))))
      (h5-io-close file)
      (message "  SUCCESS"))
  (error
   (message "  FAILED: %s" (error-message-string err))))

;; Test 2: with_groups.h5
(message "\nTest 2: with_groups.h5")
(condition-case err
    (let ((file (h5-io-open (expand-file-name "testdata/with_groups.h5" (file-name-directory load-file-name)) nil)))
      (message "  Superblock version: %d" (h5-io-superblock-version (h5-io-file-superblock file)))
      (message "  Walking file...")
      (h5-io-walk file
                 (lambda (path obj)
                   (message "    %s: %s" path (type-of obj))))
      (h5-io-close file)
      (message "  SUCCESS"))
  (error
   (message "  FAILED: %s" (error-message-string err))))

;; Test 3: v0.h5
(message "\nTest 3: v0.h5")
(condition-case err
    (let ((file (h5-io-open (expand-file-name "testdata/v0.h5" (file-name-directory load-file-name)) nil)))
      (message "  Superblock version: %d" (h5-io-superblock-version (h5-io-file-superblock file)))
      (message "  Walking file...")
      (h5-io-walk file
                 (lambda (path obj)
                   (message "    %s: %s" path (type-of obj))))
      (h5-io-close file)
      (message "  SUCCESS"))
  (error
   (message "  FAILED: %s" (error-message-string err))))

(message "\n=== Tests Complete ===\n")

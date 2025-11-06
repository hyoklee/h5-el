;;; test-h5-io.el --- Test script for h5-io.el -*- lexical-binding: t; -*-

;; Simple test to verify HDF5 read/write works correctly

(require 'h5-io)

(defun test-h5-io-write-read ()
  "Test creating and reading back an HDF5 file."
  (let ((test-file "/tmp/test-output.h5"))
    ;; Clean up any existing file
    (when (file-exists-p test-file)
      (delete-file test-file))

    ;; Create and write file
    (message "Creating HDF5 file: %s" test-file)
    (let* ((file (h5-io-create test-file))
           (ds (h5-io-create-dataset file "/temperature" '(10))))

      (h5-io-write-dataset file ds
        (list 20.0 20.1 20.2 20.3 20.4 20.5 20.6 20.7 20.8 20.9))

      (h5-io-write-attribute ds "units" "Celsius")

      (h5-io-save file)
      (h5-io-close file)
      (message "File created successfully"))

    ;; Read file back
    (message "Reading HDF5 file: %s" test-file)
    (condition-case err
        (let ((file (h5-io-open test-file)))
          (message "File opened successfully!")

          (let ((sb (h5-io-file-superblock file)))
            (message "  Superblock version: %d" (h5-io-superblock-version sb))
            (message "  Size of offsets: %d" (h5-io-superblock-size-of-offsets sb))
            (message "  Size of lengths: %d" (h5-io-superblock-size-of-lengths sb)))

          (h5-io-walk file
                     (lambda (path obj)
                       (cond
                        ((h5-io-group-p obj)
                         (message "  Group: %s" path))
                        ((h5-io-dataset-p obj)
                         (message "  Dataset: %s" path)))))

          (h5-io-close file)
          (message "SUCCESS: File read successfully!"))
      (error
       (message "ERROR: %s" (error-message-string err))
       (signal (car err) (cdr err))))))

;; Run the test
(test-h5-io-write-read)

(provide 'test-h5-io)
;;; test-h5-io.el ends here

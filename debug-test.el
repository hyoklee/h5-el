;;; debug-test.el --- Debug test for HDF5 file reading

(add-to-list 'load-path default-directory)
(require 'h5-io)

(defun debug-test-file (filename)
  "Debug test for FILENAME."
  (message "========================================")
  (message "Testing: %s" filename)
  (message "========================================")

  (let ((file (h5-io-open (expand-file-name filename "testdata/"))))
    (unwind-protect
        (progn
          ;; Check superblock
          (message "Superblock: %S" (h5-io-file-superblock file))

          ;; Check root group
          (let ((root (h5-io-file-root-group file)))
            (message "Root group: %S" root)
            (when root
              (message "Root group name: %s" (h5-io-group-name root))
              (message "Root group children: %S" (h5-io-group-children root))
              (message "Number of children: %d" (length (h5-io-group-children root)))))

          ;; Walk all objects
          (message "\nWalking all objects:")
          (h5-io-walk file
                     (lambda (path obj)
                       (cond
                        ((h5-io-group-p obj)
                         (message "  Group: %s (children: %d)"
                                 path
                                 (length (h5-io-group-children obj))))
                        ((h5-io-dataset-p obj)
                         (let* ((ds (h5-io-dataset-dataspace obj))
                                (rank (if ds (h5-io-dataspace-rank ds) 0))
                                (dims (if ds (h5-io-dataspace-dimensions ds) nil)))
                           (message "  Dataset: %s (rank: %d, dims: %S)"
                                   path rank dims)))))))
      (h5-io-close file))))

;; Test all files
(debug-test-file "simple.h5")
(debug-test-file "with_groups.h5")
(debug-test-file "with_attributes.h5")
(debug-test-file "v0.h5")

(message "\nAll tests completed!")

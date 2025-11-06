;;; verify-signature.el --- Verify HDF5 signature is written correctly -*- lexical-binding: t; -*-

(require 'h5-io)

(defun verify-h5-signature ()
  "Create a test file and verify the HDF5 signature is correct."
  (let ((test-file "/tmp/verify-signature.h5"))
    ;; Clean up
    (when (file-exists-p test-file)
      (delete-file test-file))

    ;; Create minimal file
    (let ((file (h5-io-create test-file)))
      (h5-io-save file)
      (h5-io-close file))

    ;; Read back and check signature
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally test-file)

      (let ((bytes (cl-loop for i from 1 to 8
                           collect (char-after i))))
        (message "Expected signature: %s" h5-io-signature)
        (message "Actual bytes:       %s" bytes)

        (if (equal bytes h5-io-signature)
            (message "✓ SUCCESS: Signature matches!")
          (message "✗ FAILED: Signature does not match"))

        ;; Also show as hex
        (message "Hex dump of first 16 bytes:")
        (cl-loop for i from 1 to 16
                do (message "  Byte %2d: 0x%02X  %3d  '%c'"
                          (1- i)
                          (char-after i)
                          (char-after i)
                          (if (and (>= (char-after i) 32)
                                   (<= (char-after i) 126))
                              (char-after i)
                            ?.)))))))

(verify-h5-signature)

(provide 'verify-signature)
;;; verify-signature.el ends here

;;; h5-examples.el --- Examples for using h5-io.el -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Hyokyung Lee
;; Author: Hyokyung Lee
;; Keywords: hdf5, examples

;;; Commentary:

;; This file contains examples demonstrating how to use h5-io.el
;; for reading and writing HDF5 files in pure Emacs Lisp.
;;
;; Based on the Go scigolib/hdf5 library examples.

;;; Code:

(require 'h5-io)

;;; ============================================================================
;;; Example 1: Creating a Simple HDF5 File
;;; ============================================================================

(defun h5-example-create-simple-file ()
  "Create a simple HDF5 file with a dataset.
Similar to the Go library's simple write example."
  (interactive)
  (let* ((filename "simple-output.h5")
         (file (h5-io-create filename))
         ;; Create a 1D dataset with 100 elements
         (ds (h5-io-create-dataset file "/temperature" '(100))))

    ;; Generate some sample data (temperatures from 20.0 to 29.9)
    (let ((data (cl-loop for i from 0 below 100
                        collect (+ 20.0 (* i 0.1)))))
      (h5-io-write-dataset file ds data))

    ;; Add attributes
    (h5-io-write-attribute ds "units" "Celsius")
    (h5-io-write-attribute ds "description" "Temperature measurements")

    ;; Save and close
    (h5-io-save file)
    (h5-io-close file)

    (message "Created HDF5 file: %s" filename)))

;;; ============================================================================
;;; Example 2: Creating Multi-dimensional Dataset
;;; ============================================================================

(defun h5-example-create-2d-dataset ()
  "Create an HDF5 file with a 2D dataset.
Similar to matrix data in scientific computing."
  (interactive)
  (let* ((filename "matrix-output.h5")
         (file (h5-io-create filename))
         ;; Create a 10x10 2D dataset
         (ds (h5-io-create-dataset file "/matrix" '(10 10))))

    ;; Generate a 10x10 matrix (flattened)
    (let ((data (cl-loop for i from 0 below 100
                        collect (* i 0.5))))
      (h5-io-write-dataset file ds data))

    ;; Add attributes
    (h5-io-write-attribute ds "type" "matrix")
    (h5-io-write-attribute ds "dimensions" "10x10")

    ;; Save and close
    (h5-io-save file)
    (h5-io-close file)

    (message "Created 2D dataset in: %s" filename)))

;;; ============================================================================
;;; Example 3: Reading HDF5 Files
;;; ============================================================================

(defun h5-example-read-file (filename)
  "Read and display information from HDF5 FILENAME.
Similar to the Go library's read example."
  (interactive "fSelect HDF5 file: ")
  (let ((file (h5-io-open filename)))
    (unwind-protect
        (progn
          ;; Display superblock information
          (let ((sb (h5-io-file-superblock file)))
            (message "=== HDF5 File Information ===")
            (message "File: %s" filename)
            (message "Superblock version: %d" (h5-io-superblock-version sb))
            (message "Size of offsets: %d bytes"
                    (h5-io-superblock-size-of-offsets sb))
            (message "Size of lengths: %d bytes"
                    (h5-io-superblock-size-of-lengths sb))
            (message "Base address: 0x%X"
                    (h5-io-superblock-base-address sb))
            (message "End of file: 0x%X"
                    (h5-io-superblock-end-of-file-address sb)))

          ;; Walk through all objects
          (message "\n=== Objects ===")
          (h5-io-walk file
                     (lambda (path obj)
                       (cond
                        ((h5-io-group-p obj)
                         (message "Group: %s" path))
                        ((h5-io-dataset-p obj)
                         (message "Dataset: %s" path)
                         (let ((ds-type (h5-io-dataset-datatype obj))
                               (ds-space (h5-io-dataset-dataspace obj)))
                           (message "  Type: %s (size: %d)"
                                   (h5-io-datatype-class ds-type)
                                   (h5-io-datatype-size ds-type))
                           (message "  Dimensions: %s"
                                   (h5-io-dataspace-dimensions ds-space))))))))

      ;; Always close the file
      (h5-io-close file))))

;;; ============================================================================
;;; Example 4: Walking File Structure
;;; ============================================================================

(defun h5-example-walk-file (filename)
  "Walk through all objects in HDF5 FILENAME.
Similar to the Go library's Walk function example."
  (interactive "fSelect HDF5 file: ")
  (let ((file (h5-io-open filename))
        (group-count 0)
        (dataset-count 0))
    (unwind-protect
        (progn
          (h5-io-walk file
                     (lambda (path obj)
                       (cond
                        ((h5-io-group-p obj)
                         (setq group-count (1+ group-count))
                         (message "📁 Group: %s" path))
                        ((h5-io-dataset-p obj)
                         (setq dataset-count (1+ dataset-count))
                         (message "📊 Dataset: %s" path)))))

          (message "\nSummary:")
          (message "  Groups: %d" group-count)
          (message "  Datasets: %d" dataset-count))

      (h5-io-close file))))

;;; ============================================================================
;;; Example 5: Creating File with Multiple Datasets
;;; ============================================================================

(defun h5-example-create-multiple-datasets ()
  "Create an HDF5 file with multiple datasets and groups."
  (interactive)
  (let* ((filename "multi-dataset.h5")
         (file (h5-io-create filename)))

    ;; Create temperature dataset
    (let ((temp-ds (h5-io-create-dataset file "/temperature" '(50))))
      (h5-io-write-dataset file temp-ds
                          (cl-loop for i from 0 below 50
                                  collect (+ 20.0 (random 10))))
      (h5-io-write-attribute temp-ds "units" "Celsius"))

    ;; Create humidity dataset
    (let ((humid-ds (h5-io-create-dataset file "/humidity" '(50))))
      (h5-io-write-dataset file humid-ds
                          (cl-loop for i from 0 below 50
                                  collect (+ 40.0 (random 30))))
      (h5-io-write-attribute humid-ds "units" "percent"))

    ;; Create pressure dataset
    (let ((press-ds (h5-io-create-dataset file "/pressure" '(50))))
      (h5-io-write-dataset file press-ds
                          (cl-loop for i from 0 below 50
                                  collect (+ 1000.0 (random 50))))
      (h5-io-write-attribute press-ds "units" "hPa"))

    ;; Save and close
    (h5-io-save file)
    (h5-io-close file)

    (message "Created multi-dataset HDF5 file: %s" filename)))

;;; ============================================================================
;;; Example 6: Working with Different Data Types
;;; ============================================================================

(defun h5-example-different-datatypes ()
  "Create datasets with different data types."
  (interactive)
  (let* ((filename "datatypes-output.h5")
         (file (h5-io-create filename)))

    ;; Float64 dataset
    (let ((float-ds (h5-io-create-dataset file "/float_data" '(10)
                                         (h5-io--make-float64-datatype))))
      (h5-io-write-dataset file float-ds
                          (cl-loop for i from 0 below 10
                                  collect (* i 3.14159))))

    ;; Int32 dataset (would need full implementation)
    ;; This is a simplified example showing the API
    (let ((int-ds (h5-io-create-dataset file "/int_data" '(10)
                                       (h5-io--make-int32-datatype))))
      (h5-io-write-dataset file int-ds
                          (cl-loop for i from 0 below 10
                                  collect i)))

    ;; Save and close
    (h5-io-save file)
    (h5-io-close file)

    (message "Created multi-type HDF5 file: %s" filename)))

;;; ============================================================================
;;; Example 7: Batch Processing
;;; ============================================================================

(defun h5-example-batch-process-files (directory)
  "Process all HDF5 files in DIRECTORY.
Lists datasets and their attributes."
  (interactive "DSelect directory: ")
  (let ((h5-files (directory-files directory t "\\.h5\\'")))
    (dolist (file h5-files)
      (message "\n=== Processing: %s ===" file)
      (condition-case err
          (let ((h5-file (h5-io-open file)))
            (h5-io-walk h5-file
                       (lambda (path obj)
                         (when (h5-io-dataset-p obj)
                           (message "Dataset: %s" path))))
            (h5-io-close h5-file))
        (error
         (message "Error processing %s: %s" file (error-message-string err)))))))

;;; ============================================================================
;;; Example 8: API Comparison with Go Library
;;; ============================================================================

(defun h5-example-api-comparison ()
  "Demonstrate API similarity to Go scigolib/hdf5 library.

Go equivalent:
  file, err := hdf5.Open(\"data.h5\")
  defer file.Close()

  file.Walk(func(path string, obj hdf5.Object) {
    switch v := obj.(type) {
    case *hdf5.Group:
      fmt.Printf(\"Group: %s\\n\", path)
    case *hdf5.Dataset:
      fmt.Printf(\"Dataset: %s\\n\", path)
    }
  })"

  ;; Emacs Lisp equivalent:
  (let ((file (h5-io-open "data.h5")))
    (unwind-protect
        (h5-io-walk file
                   (lambda (path obj)
                     (cond
                      ((h5-io-group-p obj)
                       (message "Group: %s" path))
                      ((h5-io-dataset-p obj)
                       (message "Dataset: %s" path)))))
      (h5-io-close file))))

;;; ============================================================================
;;; Running Examples
;;; ============================================================================

(defun h5-example-run-all ()
  "Run all HDF5 examples.
Creates several demonstration files."
  (interactive)
  (message "Running HDF5 examples...")
  (h5-example-create-simple-file)
  (h5-example-create-2d-dataset)
  (h5-example-create-multiple-datasets)
  (h5-example-different-datatypes)
  (message "\nAll examples completed!"))

(provide 'h5-examples)
;;; h5-examples.el ends here

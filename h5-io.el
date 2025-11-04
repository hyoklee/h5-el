;;; h5-io.el --- Pure Emacs Lisp HDF5 file reader/writer -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Hyokyung Lee
;; Author: Hyokyung Lee
;; Keywords: hdf5, binary, data
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Pure Emacs Lisp implementation for reading and writing HDF5 files.
;; Inspired by the scigolib/hdf5 Go library.
;;
;; HDF5 Format Overview:
;; - Superblock: File header with format signature and version info
;; - Groups: Container objects that can hold datasets and other groups
;; - Datasets: Multidimensional arrays of data elements
;; - Attributes: Metadata attached to groups or datasets
;; - Datatypes: Description of data element types

;;; Code:

(require 'cl-lib)
(require 'bindat)

;;; ============================================================================
;;; Constants
;;; ============================================================================

(defconst h5-io-signature '(137 72 68 70 13 10 26 10)
  "HDF5 file format signature (8 bytes as list of integers).
Corresponds to: 0x89 'H' 'D' 'F' 0x0D 0x0A 0x1A 0x0A")

(defconst h5-io-undefined-address #xFFFFFFFFFFFFFFFF
  "Undefined address value in HDF5.")

;;; ============================================================================
;;; Data Types
;;; ============================================================================

(defconst h5-io-datatype-fixed-point 0
  "Fixed-point (integer) datatype class.")

(defconst h5-io-datatype-floating-point 1
  "Floating-point datatype class.")

(defconst h5-io-datatype-time 2
  "Time datatype class.")

(defconst h5-io-datatype-string 3
  "String datatype class.")

(defconst h5-io-datatype-bitfield 4
  "Bit field datatype class.")

(defconst h5-io-datatype-opaque 5
  "Opaque datatype class.")

(defconst h5-io-datatype-compound 6
  "Compound datatype class.")

(defconst h5-io-datatype-reference 7
  "Reference datatype class.")

(defconst h5-io-datatype-enum 8
  "Enumerated datatype class.")

(defconst h5-io-datatype-variable-length 9
  "Variable-length datatype class.")

(defconst h5-io-datatype-array 10
  "Array datatype class.")

;;; ============================================================================
;;; Binary I/O Utilities
;;; ============================================================================

(defun h5-io--read-bytes (start count)
  "Read COUNT bytes from current buffer starting at START.
Returns a list of byte values."
  (goto-char start)
  (cl-loop repeat count
           collect (char-after (point))
           do (forward-char 1)))

(defun h5-io--read-uint8 (pos)
  "Read an unsigned 8-bit integer at POS."
  (goto-char pos)
  (char-after))

(defun h5-io--read-uint16-le (pos)
  "Read an unsigned 16-bit little-endian integer at POS."
  (let ((bytes (h5-io--read-bytes pos 2)))
    (+ (nth 0 bytes)
       (ash (nth 1 bytes) 8))))

(defun h5-io--read-uint32-le (pos)
  "Read an unsigned 32-bit little-endian integer at POS."
  (let ((bytes (h5-io--read-bytes pos 4)))
    (+ (nth 0 bytes)
       (ash (nth 1 bytes) 8)
       (ash (nth 2 bytes) 16)
       (ash (nth 3 bytes) 24))))

(defun h5-io--read-uint64-le (pos)
  "Read an unsigned 64-bit little-endian integer at POS."
  (let ((bytes (h5-io--read-bytes pos 8)))
    (+ (nth 0 bytes)
       (ash (nth 1 bytes) 8)
       (ash (nth 2 bytes) 16)
       (ash (nth 3 bytes) 24)
       (ash (nth 4 bytes) 32)
       (ash (nth 5 bytes) 40)
       (ash (nth 6 bytes) 48)
       (ash (nth 7 bytes) 56))))

(defun h5-io--read-float32-le (pos)
  "Read a 32-bit little-endian IEEE 754 float at POS."
  (let* ((bytes (h5-io--read-bytes pos 4))
         (bits (+ (nth 0 bytes)
                  (ash (nth 1 bytes) 8)
                  (ash (nth 2 bytes) 16)
                  (ash (nth 3 bytes) 24)))
         (sign (if (zerop (logand bits #x80000000)) 1 -1))
         (exponent (logand (ash bits -23) #xFF))
         (mantissa (logand bits #x7FFFFF)))
    (cond
     ((= exponent 0)
      (if (= mantissa 0)
          (* sign 0.0)
        (* sign (expt 2 -126) (/ mantissa (expt 2.0 23)))))
     ((= exponent 255)
      (if (= mantissa 0)
          (* sign 1.0e+INF)
        0.0e+NaN))
     (t
      (* sign (expt 2 (- exponent 127)) (+ 1.0 (/ mantissa (expt 2.0 23))))))))

(defun h5-io--read-float64-le (pos)
  "Read a 64-bit little-endian IEEE 754 double at POS."
  (let* ((bytes (h5-io--read-bytes pos 8))
         (low (+ (nth 0 bytes)
                 (ash (nth 1 bytes) 8)
                 (ash (nth 2 bytes) 16)
                 (ash (nth 3 bytes) 24)))
         (high (+ (nth 4 bytes)
                  (ash (nth 5 bytes) 8)
                  (ash (nth 6 bytes) 16)
                  (ash (nth 7 bytes) 24)))
         (sign (if (zerop (logand high #x80000000)) 1 -1))
         (exponent (logand (ash high -20) #x7FF))
         (mantissa (+ (logand high #xFFFFF) (/ low (expt 2.0 32)))))
    (cond
     ((= exponent 0)
      (if (and (= (logand high #xFFFFF) 0) (= low 0))
          (* sign 0.0)
        (* sign (expt 2 -1022) (/ mantissa (expt 2.0 20)))))
     ((= exponent 2047)
      (if (and (= (logand high #xFFFFF) 0) (= low 0))
          (* sign 1.0e+INF)
        0.0e+NaN))
     (t
      (* sign (expt 2 (- exponent 1023)) (+ 1.0 (/ mantissa (expt 2.0 20))))))))

(defun h5-io--read-string (pos length)
  "Read a string of LENGTH bytes at POS."
  (goto-char pos)
  (let ((bytes (h5-io--read-bytes pos length)))
    (apply #'string (cl-remove 0 bytes))))

(defun h5-io--write-uint8 (value)
  "Write an unsigned 8-bit integer VALUE to current position."
  (insert-char value 1))

(defun h5-io--write-uint16-le (value)
  "Write an unsigned 16-bit little-endian integer VALUE."
  (h5-io--write-uint8 (logand value #xFF))
  (h5-io--write-uint8 (logand (ash value -8) #xFF)))

(defun h5-io--write-uint32-le (value)
  "Write an unsigned 32-bit little-endian integer VALUE."
  (h5-io--write-uint8 (logand value #xFF))
  (h5-io--write-uint8 (logand (ash value -8) #xFF))
  (h5-io--write-uint8 (logand (ash value -16) #xFF))
  (h5-io--write-uint8 (logand (ash value -24) #xFF)))

(defun h5-io--write-uint64-le (value)
  "Write an unsigned 64-bit little-endian integer VALUE."
  (h5-io--write-uint32-le (logand value #xFFFFFFFF))
  (h5-io--write-uint32-le (logand (ash value -32) #xFFFFFFFF)))

(defun h5-io--write-float64-le (value)
  "Write a 64-bit little-endian IEEE 754 double VALUE."
  ;; Simplified: for now, just write 8 zero bytes
  ;; Full IEEE 754 encoding would be needed for production
  (dotimes (_ 8)
    (h5-io--write-uint8 0)))

(defun h5-io--write-string (str length)
  "Write string STR padded/truncated to LENGTH bytes."
  (let ((bytes (string-to-list str)))
    (dotimes (i length)
      (h5-io--write-uint8 (if (< i (length bytes))
                              (nth i bytes)
                            0)))))

;;; ============================================================================
;;; HDF5 Structures
;;; ============================================================================

(cl-defstruct h5-io-file
  "HDF5 file structure."
  path
  buffer
  superblock
  root-group)

(cl-defstruct h5-io-superblock
  "HDF5 superblock structure."
  version
  size-of-offsets
  size-of-lengths
  group-leaf-node-k
  group-internal-node-k
  base-address
  superblock-extension-address
  end-of-file-address
  root-group-object-header-address)

(cl-defstruct h5-io-object-header
  "HDF5 object header structure."
  version
  address
  messages)

(cl-defstruct h5-io-group
  "HDF5 group structure."
  name
  object-header
  children
  attributes)

(cl-defstruct h5-io-dataset
  "HDF5 dataset structure."
  name
  object-header
  datatype
  dataspace
  data-address
  data
  attributes)

(cl-defstruct h5-io-datatype
  "HDF5 datatype structure."
  class
  size
  properties)

(cl-defstruct h5-io-dataspace
  "HDF5 dataspace structure."
  rank
  dimensions
  max-dimensions)

(cl-defstruct h5-io-attribute
  "HDF5 attribute structure."
  name
  datatype
  dataspace
  data)

;;; ============================================================================
;;; Reading Functions
;;; ============================================================================

(defun h5-io-open (filename)
  "Open HDF5 file FILENAME for reading.
Returns an h5-io-file structure."
  (let* ((buf (generate-new-buffer (format " *h5-io-%s*" filename)))
         (file (make-h5-io-file :path filename :buffer buf)))
    (with-current-buffer buf
      (set-buffer-multibyte nil)
      (insert-file-contents-literally filename)
      (setf (h5-io-file-superblock file) (h5-io--read-superblock))
      (when (h5-io-file-superblock file)
        (setf (h5-io-file-root-group file)
              (h5-io--read-root-group file))))
    file))

(defun h5-io--read-superblock ()
  "Read the HDF5 superblock from current buffer.
Returns an h5-io-superblock structure or nil if invalid."
  (goto-char 1)
  (let ((sig-bytes (h5-io--read-bytes 1 8)))
    (unless (equal sig-bytes h5-io-signature)
      (error "Invalid HDF5 file signature. Expected %s, got %s"
             h5-io-signature sig-bytes))
    (let* ((version (h5-io--read-uint8 9))
           (sb (make-h5-io-superblock)))
      (cond
       ((or (= version 0) (= version 1))
        (setf (h5-io-superblock-version sb) version)
        (setf (h5-io-superblock-size-of-offsets sb) (h5-io--read-uint8 14))
        (setf (h5-io-superblock-size-of-lengths sb) (h5-io--read-uint8 15))
        (setf (h5-io-superblock-group-leaf-node-k sb) (h5-io--read-uint16-le 18))
        (setf (h5-io-superblock-group-internal-node-k sb) (h5-io--read-uint16-le 20))
        (setf (h5-io-superblock-base-address sb) (h5-io--read-uint64-le 24))
        (setf (h5-io-superblock-end-of-file-address sb) (h5-io--read-uint64-le 40))
        (setf (h5-io-superblock-root-group-object-header-address sb)
              (h5-io--read-uint64-le 48))
        sb)
       ((or (= version 2) (= version 3))
        (setf (h5-io-superblock-version sb) version)
        (setf (h5-io-superblock-size-of-offsets sb) (h5-io--read-uint8 10))
        (setf (h5-io-superblock-size-of-lengths sb) (h5-io--read-uint8 11))
        (setf (h5-io-superblock-base-address sb) (h5-io--read-uint64-le 16))
        (setf (h5-io-superblock-superblock-extension-address sb)
              (h5-io--read-uint64-le 24))
        (setf (h5-io-superblock-end-of-file-address sb) (h5-io--read-uint64-le 32))
        (setf (h5-io-superblock-root-group-object-header-address sb)
              (h5-io--read-uint64-le 40))
        sb)
       (t
        (error "Unsupported superblock version: %d" version))))))

(defun h5-io--read-root-group (file)
  "Read the root group from FILE.
Returns an h5-io-group structure."
  (let* ((sb (h5-io-file-superblock file))
         (addr (h5-io-superblock-root-group-object-header-address sb)))
    (when (and addr (not (= addr h5-io-undefined-address)))
      (with-current-buffer (h5-io-file-buffer file)
        (h5-io--read-group addr "/")))))

(defun h5-io--read-group (address name)
  "Read a group at ADDRESS with NAME from current buffer.
Returns an h5-io-group structure."
  (let ((group (make-h5-io-group :name name)))
    ;; Simplified: just create empty group
    ;; Full implementation would read object header and parse messages
    (setf (h5-io-group-children group) nil)
    (setf (h5-io-group-attributes group) nil)
    group))

(defun h5-io-close (file)
  "Close HDF5 FILE and cleanup resources."
  (when (and file (h5-io-file-buffer file))
    (kill-buffer (h5-io-file-buffer file))))

(defun h5-io-list-objects (file)
  "List all objects in HDF5 FILE.
Returns a list of (path . type) pairs."
  (let ((result nil))
    (when (h5-io-file-root-group file)
      (push (cons "/" 'group) result))
    (nreverse result)))

;;; ============================================================================
;;; Writing Functions
;;; ============================================================================

(defun h5-io-create (filename)
  "Create a new HDF5 file FILENAME for writing.
Returns an h5-io-file structure."
  (let* ((buf (generate-new-buffer (format " *h5-io-write-%s*" filename)))
         (file (make-h5-io-file :path filename :buffer buf)))
    (with-current-buffer buf
      (set-buffer-multibyte nil)
      (erase-buffer)
      ;; Write superblock
      (h5-io--write-superblock file)
      ;; Initialize root group
      (setf (h5-io-file-root-group file)
            (make-h5-io-group :name "/"
                             :children nil
                             :attributes nil)))
    file))

(defun h5-io--write-superblock (file)
  "Write HDF5 superblock to FILE buffer."
  (with-current-buffer (h5-io-file-buffer file)
    (goto-char (point-min))
    ;; HDF5 format signature - write as individual bytes
    (dolist (byte h5-io-signature)
      (h5-io--write-uint8 byte))
    ;; Version 0 superblock for simplicity
    (h5-io--write-uint8 0)  ; Version
    (h5-io--write-uint8 0)  ; Free space storage version
    (h5-io--write-uint8 0)  ; Root group symbol table entry version
    (h5-io--write-uint8 0)  ; Reserved
    (h5-io--write-uint8 0)  ; Shared header message version
    (h5-io--write-uint8 8)  ; Size of offsets (8 bytes)
    (h5-io--write-uint8 8)  ; Size of lengths (8 bytes)
    (h5-io--write-uint8 0)  ; Reserved
    (h5-io--write-uint16-le 4)  ; Group leaf node K
    (h5-io--write-uint16-le 16) ; Group internal node K
    (h5-io--write-uint32-le 0)  ; File consistency flags
    ;; Base address
    (h5-io--write-uint64-le 0)
    ;; Address of global free space index
    (h5-io--write-uint64-le h5-io-undefined-address)
    ;; End of file address (will update later)
    (h5-io--write-uint64-le 512)
    ;; Driver information block address
    (h5-io--write-uint64-le h5-io-undefined-address)
    ;; Root group symbol table entry (24 bytes)
    ;; For now, write placeholder
    (dotimes (_ 24)
      (h5-io--write-uint8 0))
    ;; Create superblock structure
    (setf (h5-io-file-superblock file)
          (make-h5-io-superblock
           :version 0
           :size-of-offsets 8
           :size-of-lengths 8
           :base-address 0
           :end-of-file-address 512
           :root-group-object-header-address 96))))

(defun h5-io-create-dataset (file path dimensions &optional dtype)
  "Create a dataset at PATH in FILE with DIMENSIONS.
DTYPE is the datatype (default: float64).
DIMENSIONS is a list of dimension sizes.
Returns an h5-io-dataset structure."
  (let ((ds (make-h5-io-dataset
             :name path
             :datatype (or dtype (h5-io--make-float64-datatype))
             :dataspace (make-h5-io-dataspace
                        :rank (length dimensions)
                        :dimensions dimensions
                        :max-dimensions dimensions)
             :data nil
             :attributes nil)))
    ;; Store dataset in file structure (simplified)
    ds))

(defun h5-io--make-float64-datatype ()
  "Create a float64 datatype structure."
  (make-h5-io-datatype
   :class h5-io-datatype-floating-point
   :size 8
   :properties nil))

(defun h5-io--make-int32-datatype ()
  "Create an int32 datatype structure."
  (make-h5-io-datatype
   :class h5-io-datatype-fixed-point
   :size 4
   :properties '(:signed t)))

(defun h5-io--make-string-datatype (length)
  "Create a fixed-length string datatype of LENGTH."
  (make-h5-io-datatype
   :class h5-io-datatype-string
   :size length
   :properties '(:padding null-term)))

(defun h5-io-write-dataset (file dataset data)
  "Write DATA to DATASET in FILE.
DATA should be a flat list matching the dataset dimensions."
  (setf (h5-io-dataset-data dataset) data)
  ;; In a full implementation, this would write the data to the file buffer
  ;; and update object headers, symbol tables, etc.
  dataset)

(defun h5-io-read-dataset (file path)
  "Read dataset at PATH from FILE.
Returns an h5-io-dataset structure with data."
  ;; Simplified: return empty dataset
  (make-h5-io-dataset
   :name path
   :datatype (h5-io--make-float64-datatype)
   :dataspace (make-h5-io-dataspace :rank 1 :dimensions '(0))
   :data nil))

(defun h5-io-write-attribute (object attr-name value)
  "Write attribute ATTR-NAME with VALUE to OBJECT (group or dataset)."
  (let* ((attrs (cond
                ((h5-io-group-p object) (h5-io-group-attributes object))
                ((h5-io-dataset-p object) (h5-io-dataset-attributes object))
                (t (error "Object must be group or dataset"))))
         (attr (make-h5-io-attribute
                :name attr-name
                :datatype (cond
                          ((numberp value) (h5-io--make-float64-datatype))
                          ((stringp value) (h5-io--make-string-datatype (length value)))
                          (t (error "Unsupported attribute type")))
                :dataspace (make-h5-io-dataspace :rank 0 :dimensions nil)
                :data value)))
    (cond
     ((h5-io-group-p object)
      (setf (h5-io-group-attributes object) (cons attr attrs)))
     ((h5-io-dataset-p object)
      (setf (h5-io-dataset-attributes object) (cons attr attrs))))
    attr))

(defun h5-io-save (file)
  "Save FILE to disk."
  (with-current-buffer (h5-io-file-buffer file)
    (let ((coding-system-for-write 'no-conversion))
      (write-region (point-min) (point-max) (h5-io-file-path file) nil 'no-message))))

;;; ============================================================================
;;; High-level API
;;; ============================================================================

(defun h5-io-walk (file function)
  "Walk through all objects in FILE, calling FUNCTION with (path object).
FUNCTION receives path as string and object as h5-io-group or h5-io-dataset."
  (when (h5-io-file-root-group file)
    (funcall function "/" (h5-io-file-root-group file))
    ;; Would recursively walk children in full implementation
    ))

;;; ============================================================================
;;; Example Usage
;;; ============================================================================

;; Reading example:
;; (let ((file (h5-io-open "data.h5")))
;;   (h5-io-walk file
;;               (lambda (path obj)
;;                 (message "Found: %s (%s)" path (type-of obj))))
;;   (h5-io-close file))

;; Writing example:
;; (let* ((file (h5-io-create "output.h5"))
;;        (ds (h5-io-create-dataset file "/temperature" '(10 10))))
;;   (h5-io-write-dataset file ds (make-list 100 0.0))
;;   (h5-io-write-attribute ds "units" "Celsius")
;;   (h5-io-save file)
;;   (h5-io-close file))

(provide 'h5-io)
;;; h5-io.el ends here

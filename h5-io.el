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

(defconst h5-io-signature "\211HDF\r\n\032\n"
  "HDF5 file format signature (8 bytes).")

(defconst h5-io-undefined-address #xFFFFFFFFFFFFFFFF
  "Undefined address value in HDF5.")

;;; ============================================================================
;;; Object Header Message Types
;;; ============================================================================

(defconst h5-io-msg-nil #x0000 "NIL message.")
(defconst h5-io-msg-dataspace #x0001 "Dataspace message.")
(defconst h5-io-msg-link-info #x0002 "Link info message.")
(defconst h5-io-msg-datatype #x0003 "Datatype message.")
(defconst h5-io-msg-fill-value-old #x0004 "Old fill value message.")
(defconst h5-io-msg-fill-value #x0005 "Fill value message.")
(defconst h5-io-msg-link #x0006 "Link message.")
(defconst h5-io-msg-external-files #x0007 "External data files message.")
(defconst h5-io-msg-data-layout #x0008 "Data layout message.")
(defconst h5-io-msg-bogus #x0009 "Bogus message.")
(defconst h5-io-msg-group-info #x000A "Group info message.")
(defconst h5-io-msg-filter-pipeline #x000B "Filter pipeline message.")
(defconst h5-io-msg-attribute #x000C "Attribute message.")
(defconst h5-io-msg-object-comment #x000D "Object comment message.")
(defconst h5-io-msg-object-modification-time-old #x000E "Old modification time.")
(defconst h5-io-msg-shared-message-table #x000F "Shared message table.")
(defconst h5-io-msg-object-header-continuation #x0010 "Object header continuation.")
(defconst h5-io-msg-symbol-table #x0011 "Symbol table message.")
(defconst h5-io-msg-object-modification-time #x0012 "Object modification time.")
(defconst h5-io-msg-btree-k-values #x0013 "B-tree k values message.")
(defconst h5-io-msg-driver-info #x0014 "Driver info message.")
(defconst h5-io-msg-attribute-info #x0015 "Attribute info message.")
(defconst h5-io-msg-object-reference-count #x0016 "Object reference count.")

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

(cl-defstruct h5-io-symbol-table-entry
  "HDF5 symbol table entry structure."
  link-name-offset
  object-header-address
  cache-type
  scratch-pad)

(cl-defstruct h5-io-local-heap
  "HDF5 local heap structure."
  address
  data-segment-size
  offset-to-head-free-list
  address-of-data-segment)

;;; ============================================================================
;;; Reading Functions
;;; ============================================================================

(defun h5-io-open (filename &optional max-bytes)
  "Open HDF5 file FILENAME for reading.
Returns an h5-io-file structure.

Optional MAX-BYTES limits how many bytes to read from the file.
If nil, reads only the first 1MB (1048576 bytes) by default.
This prevents loading huge files entirely into memory.
Use a larger value if you need to access data deeper in the file.

For initial display of superblock and top-level groups, 1MB is usually sufficient."
  (let* ((buf (generate-new-buffer (format " *h5-io-%s*" filename)))
         (file (make-h5-io-file :path filename :buffer buf))
         (read-size (or max-bytes 1048576)))  ; Default 1MB
    (message "h5-io-open: Reading %s bytes from %s" read-size filename)
    (with-current-buffer buf
      (set-buffer-multibyte nil)
      ;; Only read the first portion of the file, not the entire file
      (insert-file-contents-literally filename nil 0 read-size)
      (message "h5-io-open: Buffer size after read: %d bytes" (buffer-size))
      (setf (h5-io-file-superblock file) (h5-io--read-superblock))
      (when (h5-io-file-superblock file)
        (setf (h5-io-file-root-group file)
              (h5-io--read-root-group file))))
    file))

(defun h5-io--read-superblock ()
  "Read the HDF5 superblock from current buffer.
Returns an h5-io-superblock structure or nil if invalid."
  (goto-char 1)
  (let ((sig (buffer-substring 1 9)))
    (unless (string= sig h5-io-signature)
      (error "Invalid HDF5 file signature"))
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
      (h5-io--read-group addr "/" file))))

(defun h5-io--read-local-heap (address)
  "Read local heap at ADDRESS.
Returns an h5-io-local-heap structure."
  (goto-char (1+ address))  ; +1 for 1-indexed Emacs positions
  (let ((signature (buffer-substring (point) (+ (point) 4))))
    (unless (string= signature "HEAP")
      (error "Invalid local heap signature at %d" address))
    (let ((heap (make-h5-io-local-heap :address address)))
      (setf (h5-io-local-heap-data-segment-size heap)
            (h5-io--read-uint64-le (+ address 9)))
      (setf (h5-io-local-heap-offset-to-head-free-list heap)
            (h5-io--read-uint64-le (+ address 17)))
      (setf (h5-io-local-heap-address-of-data-segment heap)
            (h5-io--read-uint64-le (+ address 25)))
      heap)))

(defun h5-io--read-string-from-heap (heap offset)
  "Read null-terminated string from HEAP at OFFSET."
  (when (and heap offset)
    (let* ((data-addr (h5-io-local-heap-address-of-data-segment heap))
           (str-pos (+ data-addr offset 1))
           (chars nil))
      (goto-char str-pos)
      (while (and (< (point) (point-max))
                  (not (zerop (char-after))))
        (push (char-after) chars)
        (forward-char 1))
      (apply #'string (nreverse chars)))))

(defun h5-io--read-symbol-table-entry (pos heap)
  "Read symbol table entry at POS using HEAP for names.
Returns (name . address) pair."
  (let* ((link-name-offset (h5-io--read-uint64-le pos))
         (obj-header-addr (h5-io--read-uint64-le (+ pos 8)))
         (name (h5-io--read-string-from-heap heap link-name-offset)))
    (when (and name (not (= obj-header-addr h5-io-undefined-address)))
      (cons name obj-header-addr))))

(defun h5-io--read-object-header-v1 (address)
  "Read object header version 1 at ADDRESS.
Returns a list of messages as (type . data) pairs."
  (goto-char (1+ address))
  (let* ((version (h5-io--read-uint8 (1+ address)))
         (num-messages (h5-io--read-uint16-le (+ address 3)))
         (obj-ref-count (h5-io--read-uint32-le (+ address 5)))
         (obj-header-size (h5-io--read-uint32-le (+ address 9)))
         (messages nil)
         (pos (+ address 16)))  ; Start of messages (after 16-byte header)

    (dotimes (_ num-messages)
      (when (< pos (point-max))
        (let* ((msg-type (h5-io--read-uint16-le pos))
               (msg-size (h5-io--read-uint16-le (+ pos 2)))
               (msg-flags (h5-io--read-uint8 (+ pos 4)))
               (msg-data-pos (+ pos 8))
               (msg-data (cons msg-type msg-data-pos)))
          (push msg-data messages)
          (setq pos (+ msg-data-pos msg-size))
          ;; Align to 8-byte boundary
          (setq pos (+ pos (mod (- 8 (mod pos 8)) 8))))))
    (nreverse messages)))

(defun h5-io--read-object-header-v2 (address)
  "Read object header version 2 at ADDRESS.
Returns a list of messages as (type . data-position) pairs.
Object header v2 format (HDF5 >= 1.8):
- Signature: 'OHDR' (4 bytes)
- Version: 2 (1 byte)
- Flags: (1 byte)
  Bits 0-1: Size of 'size of chunk 0' field (00=1, 01=2, 10=4, 11=8 bytes)
  Bit 2: Track attribute creation order
  Bit 3: Index attribute creation order
  Bit 4: Store non-default attribute phase change values
  Bit 5: Store access/modification/change/birth times
- Optional fields based on flags
- Size of chunk 0
- Messages (packed, no 8-byte alignment)
- Checksum (4 bytes)"
  (goto-char (1+ address))

  ;; Read and validate OHDR signature
  (let ((sig-bytes (list
                    (h5-io--read-uint8 (1+ address))
                    (h5-io--read-uint8 (+ address 2))
                    (h5-io--read-uint8 (+ address 3))
                    (h5-io--read-uint8 (+ address 4)))))
    (unless (equal sig-bytes '(79 72 68 82))  ; 'O' 'H' 'D' 'R'
      (error "Invalid OHDR signature at address %d" address)))

  (let* ((version (h5-io--read-uint8 (+ address 5)))
         (flags (h5-io--read-uint8 (+ address 6)))
         (pos (+ address 7))

         ;; Parse flag bits
         (size-of-chunk0-size (ash 1 (logand flags #x03)))  ; Bits 0-1: 00=1, 01=2, 10=4, 11=8
         (track-attr-order (not (zerop (logand flags #x04))))      ; Bit 2
         (index-attr-order (not (zerop (logand flags #x08))))      ; Bit 3
         (store-phase-change (not (zerop (logand flags #x10))))    ; Bit 4
         (store-times (not (zerop (logand flags #x20)))))          ; Bit 5

    ;; Skip optional timestamp fields if present (4 bytes each)
    (when store-times
      (setq pos (+ pos 16)))  ; access, modification, change, birth times

    ;; Skip optional attribute phase change values if present (2 bytes each)
    (when store-phase-change
      (setq pos (+ pos 4)))   ; max-compact, min-dense

    ;; Read size of chunk 0
    (let ((chunk0-size (cond
                        ((= size-of-chunk0-size 1) (h5-io--read-uint8 pos))
                        ((= size-of-chunk0-size 2) (h5-io--read-uint16-le pos))
                        ((= size-of-chunk0-size 4) (h5-io--read-uint32-le pos))
                        ((= size-of-chunk0-size 8) (h5-io--read-uint64-le pos))
                        (t (error "Invalid size-of-chunk0-size: %d" size-of-chunk0-size)))))
      (setq pos (+ pos size-of-chunk0-size))

      ;; Read messages until we reach the checksum (4 bytes before end of chunk)
      (let ((messages nil)
            (chunk-end (+ (1+ address) 4 2 chunk0-size)))  ; signature(4) + version(1) + flags(1) + optional + size + chunk0-size

        ;; Adjust chunk-end for optional fields
        (when store-times
          (setq chunk-end (+ chunk-end 16)))
        (when store-phase-change
          (setq chunk-end (+ chunk-end 4)))
        (setq chunk-end (+ chunk-end size-of-chunk0-size))

        ;; Read messages (in v2, messages are packed without alignment)
        (while (< (+ pos 4) (- chunk-end 4))  ; Stop 4 bytes before end (checksum)
          (when (< pos (point-max))
            (let* ((msg-type (h5-io--read-uint8 pos))
                   (msg-size (h5-io--read-uint16-le (+ pos 1)))
                   (msg-flags (h5-io--read-uint8 (+ pos 3)))
                   (msg-data-pos (+ pos 4)))

              ;; Stop if we hit a NIL message (padding)
              (if (= msg-type h5-io-msg-nil)
                  (setq pos chunk-end)  ; Exit loop
                (progn
                  (push (cons msg-type msg-data-pos) messages)
                  (setq pos (+ msg-data-pos msg-size)))))))

        (nreverse messages)))))

(defun h5-io--find-message (messages msg-type)
  "Find first message of MSG-TYPE in MESSAGES."
  (cl-find-if (lambda (msg) (= (car msg) msg-type)) messages))

(defun h5-io--find-all-messages (messages msg-type)
  "Find all messages of MSG-TYPE in MESSAGES."
  (cl-remove-if-not (lambda (msg) (= (car msg) msg-type)) messages))

(defun h5-io--read-link-message (msg-pos)
  "Read link message at MSG-POS.
Link messages (HDF5 >= 1.8) store links to objects in groups.
Returns (name . address) pair for hard links, or nil for other link types.
Format:
- Version: 1 byte
- Flags: 1 byte
  Bits 0-1: Size of length field (00=1, 01=2, 10=4, 11=8 bytes)
  Bit 2: Creation order present
  Bit 3: Link type field present (if not set, default to hard link)
  Bit 4: Link name character set present
- Optional fields based on flags
- Link name length (variable size)
- Link name (variable length)
- Link information (depends on link type)"
  (let* ((version (h5-io--read-uint8 msg-pos))
         (flags (h5-io--read-uint8 (+ msg-pos 1)))
         (pos (+ msg-pos 2))

         ;; Parse flags
         (link-name-size-size (ash 1 (logand flags #x03)))  ; Bits 0-1
         (has-creation-order (not (zerop (logand flags #x04))))    ; Bit 2
         (has-link-type (not (zerop (logand flags #x08))))         ; Bit 3
         (has-charset (not (zerop (logand flags #x10)))))          ; Bit 4

    ;; Read link type (default to 0 = hard link if not present)
    (let ((link-type (if has-link-type
                        (prog1 (h5-io--read-uint8 pos)
                          (setq pos (+ pos 1)))
                      0)))

      ;; Skip creation order if present
      (when has-creation-order
        (setq pos (+ pos 8)))

      ;; Skip character set if present
      (when has-charset
        (setq pos (+ pos 1)))

      ;; Read link name length
      (let ((name-length (cond
                          ((= link-name-size-size 1) (h5-io--read-uint8 pos))
                          ((= link-name-size-size 2) (h5-io--read-uint16-le pos))
                          ((= link-name-size-size 4) (h5-io--read-uint32-le pos))
                          ((= link-name-size-size 8) (h5-io--read-uint64-le pos))
                          (t (error "Invalid link-name-size-size: %d" link-name-size-size)))))
        (setq pos (+ pos link-name-size-size))

        ;; Read link name
        (goto-char (1+ pos))
        (let ((name (buffer-substring (point) (+ (point) name-length))))
          (setq pos (+ pos name-length))

          ;; Read link information based on link type
          (cond
           ((= link-type 0)  ; Hard link
            (let ((obj-addr (h5-io--read-uint64-le pos)))
              (cons name obj-addr)))

           ((= link-type 1)  ; Soft link (symbolic link)
            ;; Soft links have: length (2 bytes) + target name
            ;; We don't follow soft links yet, so return nil
            nil)

           ((= link-type 64)  ; External link
            ;; External links have: external file info + target path
            ;; We don't follow external links yet, so return nil
            nil)

           (t
            (error "Unknown link type: %d" link-type)))))))))

(defun h5-io--read-symbol-table-message (msg-pos)
  "Read symbol table message at MSG-POS.
Returns (btree-address . heap-address) pair."
  (let ((btree-addr (h5-io--read-uint64-le msg-pos))
        (heap-addr (h5-io--read-uint64-le (+ msg-pos 8))))
    (cons btree-addr heap-addr)))

(defun h5-io--read-symbol-table-node (address heap)
  "Read symbol table node at ADDRESS using HEAP.
Returns list of (name . obj-address) pairs."
  (goto-char (1+ address))
  (let ((signature (buffer-substring (point) (+ (point) 4))))
    (unless (string= signature "SNOD")
      (error "Invalid symbol table node signature at %d" address))
    (let* ((version (h5-io--read-uint8 (+ address 5)))
           (num-symbols (h5-io--read-uint16-le (+ address 8)))
           (entries nil)
           (pos (+ address 16)))  ; Start of entries

      (dotimes (_ num-symbols)
        (when (< pos (point-max))
          (let ((entry (h5-io--read-symbol-table-entry pos heap)))
            (when entry
              (push entry entries))
            (setq pos (+ pos 40)))))  ; Each entry is 40 bytes
      (nreverse entries))))

(defun h5-io--read-group (address name file)
  "Read a group at ADDRESS with NAME from FILE.
Returns an h5-io-group structure with children populated.
Supports both object header v1 (HDF5 < 1.8) and v2 (HDF5 >= 1.8)."
  (with-current-buffer (h5-io-file-buffer file)
    (let ((group (make-h5-io-group :name name))
          (messages nil)
          (is-v2 nil))

      ;; Detect object header version by checking for "OHDR" signature
      (goto-char (1+ address))
      (let ((sig-bytes (list
                        (h5-io--read-uint8 (1+ address))
                        (h5-io--read-uint8 (+ address 2))
                        (h5-io--read-uint8 (+ address 3))
                        (h5-io--read-uint8 (+ address 4)))))
        (setq is-v2 (equal sig-bytes '(79 72 68 82))))  ; 'O' 'H' 'D' 'R'

      ;; Read object header based on version
      (setq messages (condition-case err
                         (if is-v2
                             (h5-io--read-object-header-v2 address)
                           (h5-io--read-object-header-v1 address))
                       (error
                        (message "Error reading object header at %d: %s" address (error-message-string err))
                        nil)))

      (when messages
        (if is-v2
            ;; HDF5 >= 1.8: Look for link messages (compact storage)
            (let ((link-msgs (h5-io--find-all-messages messages h5-io-msg-link)))
              (when link-msgs
                (let ((children-entries nil))
                  (dolist (link-msg link-msgs)
                    (condition-case nil
                        (let ((link-data (h5-io--read-link-message (cdr link-msg))))
                          (when link-data
                            (push link-data children-entries)))
                      (error nil)))
                  (setf (h5-io-group-children group) (nreverse children-entries)))))

          ;; HDF5 < 1.8: Look for symbol table message
          (let ((sym-table-msg (h5-io--find-message messages h5-io-msg-symbol-table)))
            (when sym-table-msg
              (let* ((sym-table-data (h5-io--read-symbol-table-message (cdr sym-table-msg)))
                     (btree-addr (car sym-table-data))
                     (heap-addr (cdr sym-table-data))
                     (heap (condition-case nil
                               (h5-io--read-local-heap heap-addr)
                             (error nil))))

                (when heap
                  ;; For simple groups, the btree-addr points to a symbol table node
                  (let ((children-entries (condition-case nil
                                              (h5-io--read-symbol-table-node btree-addr heap)
                                            (error nil))))
                    (setf (h5-io-group-children group) children-entries))))))))

      group)))

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
    ;; HDF5 format signature
    (insert h5-io-signature)
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
    (write-region (point-min) (point-max) (h5-io-file-path file) nil 'no-message)))

(defun h5-io--read-dataset (address name)
  "Read dataset at ADDRESS with NAME.
Returns an h5-io-dataset structure.
Supports both object header v1 (HDF5 < 1.8) and v2 (HDF5 >= 1.8)."
  (condition-case nil
      (let* (;; Detect object header version
             (sig-bytes (list
                         (h5-io--read-uint8 (1+ address))
                         (h5-io--read-uint8 (+ address 2))
                         (h5-io--read-uint8 (+ address 3))
                         (h5-io--read-uint8 (+ address 4))))
             (is-v2 (equal sig-bytes '(79 72 68 82)))  ; 'O' 'H' 'D' 'R'
             (messages (if is-v2
                          (h5-io--read-object-header-v2 address)
                        (h5-io--read-object-header-v1 address)))
             (dataset (make-h5-io-dataset :name name)))
        ;; Look for dataspace message
        (let ((dataspace-msg (h5-io--find-message messages h5-io-msg-dataspace)))
          (when dataspace-msg
            (setf (h5-io-dataset-dataspace dataset)
                  (h5-io--read-dataspace-message (cdr dataspace-msg)))))
        ;; Look for datatype message
        (let ((datatype-msg (h5-io--find-message messages h5-io-msg-datatype)))
          (when datatype-msg
            (setf (h5-io-dataset-datatype dataset)
                  (h5-io--read-datatype-message (cdr datatype-msg)))))
        dataset)
    (error nil)))

(defun h5-io--read-dataspace-message (msg-pos)
  "Read dataspace message at MSG-POS.
Returns an h5-io-dataspace structure."
  (let* ((version (h5-io--read-uint8 msg-pos))
         (dimensionality (h5-io--read-uint8 (+ msg-pos 1)))
         (flags (h5-io--read-uint8 (+ msg-pos 2)))
         (dataspace (make-h5-io-dataspace :rank dimensionality)))
    (when (> dimensionality 0)
      (let ((dims nil)
            (pos (+ msg-pos 8)))  ; Skip to dimension sizes
        (dotimes (i dimensionality)
          (push (h5-io--read-uint64-le pos) dims)
          (setq pos (+ pos 8)))
        (setf (h5-io-dataspace-dimensions dataspace) (nreverse dims))))
    dataspace))

(defun h5-io--read-datatype-message (msg-pos)
  "Read datatype message at MSG-POS.
Returns an h5-io-datatype structure."
  (let* ((class-and-version (h5-io--read-uint32-le msg-pos))
         (class (logand class-and-version #xFF))
         (datatype (make-h5-io-datatype :class class :size 0)))
    datatype))

;;; ============================================================================
;;; High-level API
;;; ============================================================================

(defun h5-io-walk (file function)
  "Walk through all objects in FILE, calling FUNCTION with (path object).
FUNCTION receives path as string and object as h5-io-group or h5-io-dataset."
  (when (h5-io-file-root-group file)
    (h5-io--walk-recursive file "/" (h5-io-file-root-group file) function (make-hash-table :test 'equal))))

(defun h5-io--walk-recursive (file path group function visited)
  "Recursively walk GROUP at PATH, calling FUNCTION for each object.
VISITED is a hash table to track visited addresses and avoid cycles."
  (funcall function path group)

  (when (h5-io-group-children group)
    (dolist (child (h5-io-group-children group))
      (let* ((child-name (car child))
             (child-addr (cdr child))
             (child-path (if (string= path "/")
                            (concat "/" child-name)
                          (concat path "/" child-name))))

        ;; Check if we've already visited this address (avoid cycles)
        (unless (gethash child-addr visited)
          (puthash child-addr t visited)

          ;; Try to read as group or dataset
          (with-current-buffer (h5-io-file-buffer file)
            (condition-case nil
                (let ((child-obj (h5-io--read-group child-addr child-name file)))
                  (when child-obj
                    (h5-io--walk-recursive file child-path child-obj function visited)))
              ;; If it's not a group, try as dataset
              (error
               (condition-case nil
                   (let ((dataset (h5-io--read-dataset child-addr child-name)))
                     (when dataset
                       (funcall function child-path dataset)))
                 (error nil))))))))))

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

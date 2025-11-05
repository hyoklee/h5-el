;;; h5-mode.el --- major mode for editing HDF5 file

;; Copyright (C) 2020-2023 Hyokyung Lee
;; Author: Hyokyung Lee
;; Updated: May 2023
;; Created: May 2020
;; Version: 0.0.1
;; Keywords: hdf5
;; URL: https://github.com/hyoklee/h5-el
;; Package-Requires: ((seq "2.20") (emacs "24.3"))

(eval-when-compile (require 'cl-lib))
(require 'arc-mode)
(require 'h5-io)
(defgroup h5 nil
  "Viewing & editing h5 file"
  :prefix "h5-"
  :group 'data)

(defvar h5-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map " " 'h5-next-line)
    (define-key map "C" 'h5-copy)
    ;; Group operations
    (define-key map (kbd "RET") 'h5-open-or-view)
    (define-key map "+" 'h5-create-group)
    (define-key map "d" 'h5-delete-group)
    (define-key map "s" 'h5-sort-group)
    ;; Image operations
    (define-key map "i" 'h5-view-as-image)
    (define-key map "e" 'h5-export-as-image)
    ;; Export operations
    (define-key map "E" 'h5-export-as-csv)
    ;; Search
    (define-key map (kbd "C-s") 'h5-search)
    ;; Quit
    (define-key map "q" 'quit-window))
  "Keymap for h5-mode.")

(define-derived-mode h5-mode special-mode "h5"
  "Major mode for viewing and editing HDF5 files.
Similar to Dired, this mode provides a tree-like view of HDF5 file contents.

Key bindings:
\\{h5-mode-map}"
  (and buffer-file-name
       (file-writable-p buffer-file-name)
       (setq buffer-read-only nil))
  ;; Auto-open HDF5 file if visiting a file
  (when (and buffer-file-name
             (not h5-current-file)
             (file-exists-p buffer-file-name))
    (condition-case err
        (progn
          (setq h5-current-file (h5-io-open buffer-file-name))
          (h5-display-file-structure h5-current-file))
      (error
       (message "Error opening HDF5 file: %s" (error-message-string err))))))

;;; Interactive Functions

(defvar-local h5-current-file nil
  "Currently opened HDF5 file handle.")

(defvar-local h5-objects-list nil
  "List of (line-number path object) tuples for objects in current buffer.")

(defun h5-open-or-view ()
  "Open a group, view dataset as text, or view attribute.
Behavior depends on the object type at point."
  (interactive)
  (let ((filename (or buffer-file-name
                      (read-file-name "HDF5 file: " nil nil t))))
    (if (not h5-current-file)
        (progn
          (setq h5-current-file (h5-io-open filename))
          (h5-display-file-structure h5-current-file))
      (h5-view-object-at-point))))

(defun h5-display-file-structure (file)
  "Display the structure of HDF5 FILE in current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq h5-objects-list nil)
    (insert (format "HDF5 File: %s\n\n" (h5-io-file-path file)))
    (when (h5-io-file-superblock file)
      (let ((sb (h5-io-file-superblock file)))
        (insert (format "Superblock Version: %d\n" (h5-io-superblock-version sb)))
        (insert (format "Size of offsets: %d\n" (h5-io-superblock-size-of-offsets sb)))
        (insert (format "Size of lengths: %d\n" (h5-io-superblock-size-of-lengths sb)))))
    (insert "\nObjects:\n")
    (h5-io-walk file
                (lambda (path obj)
                  (let ((line-num (line-number-at-pos)))
                    (cond
                     ((h5-io-group-p obj)
                      (insert (format "  [Group] %s\n" path))
                      (when (h5-io-group-attributes obj)
                        (dolist (attr (h5-io-group-attributes obj))
                          (insert (format "    Attr: %s = %s\n"
                                        (h5-io-attribute-name attr)
                                        (h5-io-attribute-data attr))))))
                     ((h5-io-dataset-p obj)
                      (insert (format "  [Dataset] %s" path))
                      (when (h5-io-dataset-dataspace obj)
                        (let ((dims (h5-io-dataspace-dimensions
                                    (h5-io-dataset-dataspace obj))))
                          (when dims
                            (insert (format " %s" dims)))))
                      (insert "\n")
                      (when (h5-io-dataset-attributes obj)
                        (dolist (attr (h5-io-dataset-attributes obj))
                          (insert (format "    Attr: %s = %s\n"
                                        (h5-io-attribute-name attr)
                                        (h5-io-attribute-data attr)))))))
                    ;; Track object for later retrieval
                    (push (list line-num path obj) h5-objects-list))))
    (setq h5-objects-list (nreverse h5-objects-list))
    (goto-char (point-min))))

(defun h5-get-object-at-point ()
  "Get the HDF5 object at point.
Returns (path . object) or nil if no object at point."
  (let ((current-line (line-number-at-pos))
        (found nil))
    (dolist (entry h5-objects-list)
      (when (and (not found) (>= current-line (car entry)))
        (setq found entry)))
    (when found
      (cons (cadr found) (caddr found)))))

(defun h5-view-object-at-point ()
  "View the object at point."
  (interactive)
  (let ((obj-info (h5-get-object-at-point)))
    (if obj-info
        (let ((path (car obj-info))
              (obj (cdr obj-info)))
          (cond
           ((h5-io-group-p obj)
            (message "Group: %s" path))
           ((h5-io-dataset-p obj)
            (h5-view-dataset path obj))
           (t
            (message "Unknown object type at point"))))
      (message "No HDF5 object at point"))))

(defun h5-view-dataset (path dataset)
  "View DATASET at PATH in a new buffer."
  (let* ((buf-name (format "*HDF5 Dataset: %s*" path))
         (buf (get-buffer-create buf-name))
         (data (h5-io-dataset-data dataset)))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Dataset: %s\n\n" path))
      (when (h5-io-dataset-dataspace dataset)
        (insert (format "Dimensions: %s\n"
                       (h5-io-dataspace-dimensions
                        (h5-io-dataset-dataspace dataset)))))
      (when (h5-io-dataset-datatype dataset)
        (insert (format "Datatype: class=%s size=%s\n"
                       (h5-io-datatype-class (h5-io-dataset-datatype dataset))
                       (h5-io-datatype-size (h5-io-dataset-datatype dataset)))))
      (insert "\nData:\n")
      (if data
          (insert (format "%S" data))
        (insert "No data available (dataset not yet read)"))
      (goto-char (point-min))
      (read-only-mode 1))
    (display-buffer buf)))

(defun h5-create-group ()
  "Create a new group in the HDF5 file."
  (interactive)
  (let ((group-name (read-string "Group name: ")))
    (if h5-current-file
        (progn
          (message "Creating group: %s" group-name)
          ;; In a full implementation, this would call h5-io functions
          ;; to create a new group in the file
          (message "Group creation not yet fully implemented"))
      (error "No HDF5 file is currently open"))))

(defun h5-delete-group ()
  "Delete a group from the HDF5 file."
  (interactive)
  (when (yes-or-no-p "Delete this group? ")
    (message "Deleting group...")
    ;; In a full implementation, this would call h5-io functions
    ;; to delete the group from the file
    (message "Group deletion not yet fully implemented")))

(defun h5-sort-group ()
  "Sort the current group's contents."
  (interactive)
  (message "Sorting group...")
  ;; In a full implementation, this would reorder the display
  (message "Group sorting not yet fully implemented"))

(defun h5-view-as-image ()
  "View the dataset at point as an image."
  (interactive)
  (if h5-current-file
      (let* ((obj-info (h5-get-object-at-point))
             (path (if obj-info
                       (car obj-info)
                     (read-string "Dataset path: ")))
             (dataset (h5-io-read-dataset h5-current-file path)))
        (if (and dataset (h5-io-dataset-data dataset))
            (progn
              (message "Viewing dataset %s as image..." path)
              ;; Image viewing requires converting numerical data to image format
              ;; This would typically require an external library or image creation
              (message "Image viewing requires additional image processing libraries"))
          (message "Cannot read dataset or no data available")))
    (error "No HDF5 file is currently open")))

(defun h5-export-as-image ()
  "Export the dataset at point as an image file."
  (interactive)
  (if h5-current-file
      (let* ((obj-info (h5-get-object-at-point))
             (path (if obj-info
                       (car obj-info)
                     (read-string "Dataset path: ")))
             (output-file (read-file-name "Export to: " nil nil nil "image.png"))
             (dataset (h5-io-read-dataset h5-current-file path)))
        (if (and dataset (h5-io-dataset-data dataset))
            (progn
              (message "Exporting dataset %s to %s..." path output-file)
              ;; Image export requires converting numerical data to image format
              ;; This would typically use libraries like ImageMagick or similar
              (message "Image export requires additional image processing libraries"))
          (message "Cannot read dataset or no data available")))
    (error "No HDF5 file is currently open")))

(defun h5-export-as-csv ()
  "Export the dataset at point as a CSV file."
  (interactive)
  (if h5-current-file
      (let* ((obj-info (h5-get-object-at-point))
             (path (if obj-info
                       (car obj-info)
                     (read-string "Dataset path: ")))
             (output-file (read-file-name "Export to: " nil nil nil "data.csv"))
             (dataset (h5-io-read-dataset h5-current-file path)))
        (if dataset
            (progn
              (message "Exporting dataset %s to %s..." path output-file)
              (with-temp-file output-file
                (insert (format "# CSV export from HDF5 dataset: %s\n" path))
                (when (h5-io-dataset-dataspace dataset)
                  (insert (format "# Dimensions: %s\n"
                                (h5-io-dataspace-dimensions
                                 (h5-io-dataset-dataspace dataset)))))
                (when (h5-io-dataset-datatype dataset)
                  (insert (format "# Datatype: class=%s size=%s\n"
                                (h5-io-datatype-class (h5-io-dataset-datatype dataset))
                                (h5-io-datatype-size (h5-io-dataset-datatype dataset)))))
                (insert "\n")
                ;; Write data
                (let ((data (h5-io-dataset-data dataset))
                      (dims (when (h5-io-dataset-dataspace dataset)
                             (h5-io-dataspace-dimensions
                              (h5-io-dataset-dataspace dataset)))))
                  (if data
                      (cond
                       ;; 1D data
                       ((or (null dims) (= (length dims) 1))
                        (dolist (value data)
                          (insert (format "%s\n" value))))
                       ;; 2D data
                       ((= (length dims) 2)
                        (let ((rows (car dims))
                              (cols (cadr dims)))
                          (dotimes (i rows)
                            (dotimes (j cols)
                              (let ((idx (+ (* i cols) j)))
                                (when (< idx (length data))
                                  (insert (format "%s" (nth idx data)))
                                  (when (< j (1- cols))
                                    (insert ",")))))
                            (insert "\n"))))
                       ;; N-D data (flatten to 1D)
                       (t
                        (insert "# Multi-dimensional data (flattened)\n")
                        (dolist (value data)
                          (insert (format "%s\n" value)))))
                    (insert "# No data available in dataset\n"))))
              (message "CSV export complete: %s" output-file))
          (message "Cannot read dataset")))
    (error "No HDF5 file is currently open")))

(defun h5-search ()
  "Search for values in the HDF5 file."
  (interactive)
  (if h5-current-file
      (let ((search-term (read-string "Search for: "))
            (results nil))
        (message "Searching for: %s" search-term)
        ;; Search through all objects
        (h5-io-walk h5-current-file
                   (lambda (path obj)
                     ;; Search in path name
                     (when (string-match-p search-term path)
                       (push (format "Path: %s" path) results))
                     ;; Search in attributes
                     (let ((attrs (cond
                                  ((h5-io-group-p obj) (h5-io-group-attributes obj))
                                  ((h5-io-dataset-p obj) (h5-io-dataset-attributes obj)))))
                       (when attrs
                         (dolist (attr attrs)
                           (when (or (string-match-p search-term (h5-io-attribute-name attr))
                                   (and (stringp (h5-io-attribute-data attr))
                                        (string-match-p search-term (h5-io-attribute-data attr))))
                             (push (format "Attribute %s in %s"
                                         (h5-io-attribute-name attr) path)
                                   results)))))))
        (if results
            (let ((buf (get-buffer-create "*HDF5 Search Results*")))
              (with-current-buffer buf
                (erase-buffer)
                (insert (format "Search results for: %s\n\n" search-term))
                (dolist (result (nreverse results))
                  (insert result "\n"))
                (goto-char (point-min))
                (read-only-mode 1))
              (display-buffer buf))
          (message "No results found for: %s" search-term)))
    (error "No HDF5 file is currently open")))

(defun h5-next-line ()
  "Move to next line in h5-mode."
  (interactive)
  (forward-line 1))

(defun h5-copy ()
  "Copy object in h5-mode."
  (interactive)
  (message "Copy not yet implemented"))

;; Cleanup
(defun h5-mode-cleanup ()
  "Cleanup when leaving h5-mode."
  (when h5-current-file
    (h5-io-close h5-current-file)
    (setq h5-current-file nil)))

(add-hook 'kill-buffer-hook 'h5-mode-cleanup)

;; Auto-mode for HDF5 files
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.h5\\'" . h5-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hdf5\\'" . h5-mode))

(put 'h5-mode 'mode-class 'special)
(provide 'h5-mode)
;;; h5-mode.el ends here

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
  (and buffer-file-name
       (file-writable-p buffer-file-name)
       (setq buffer-read-only nil)))

;;; Interactive Functions

(defvar h5-current-file nil
  "Currently opened HDF5 file handle.")

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
    (insert (format "HDF5 File: %s\n\n" (h5-io-file-path file)))
    (when (h5-io-file-superblock file)
      (insert (format "Superblock Version: %d\n"
                      (h5-io-superblock-version (h5-io-file-superblock file)))))
    (insert "\nObjects:\n")
    (h5-io-walk file
                (lambda (path obj)
                  (cond
                   ((h5-io-group-p obj)
                    (insert (format "  [Group] %s\n" path)))
                   ((h5-io-dataset-p obj)
                    (insert (format "  [Dataset] %s\n" path))))))))

(defun h5-view-object-at-point ()
  "View the object at point."
  (interactive)
  (message "Viewing object at point..."))

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
      (let ((dataset-path (read-string "Dataset path: ")))
        (let ((dataset (h5-io-read-dataset h5-current-file dataset-path)))
          (message "Viewing dataset %s as image..." dataset-path)
          ;; In a full implementation, this would convert the dataset
          ;; to an image and display it
          (message "Image viewing not yet fully implemented")))
    (error "No HDF5 file is currently open")))

(defun h5-export-as-image ()
  "Export the dataset at point as an image file."
  (interactive)
  (if h5-current-file
      (let* ((dataset-path (read-string "Dataset path: "))
             (output-file (read-file-name "Export to: " nil nil nil "image.png")))
        (message "Exporting dataset %s to %s..." dataset-path output-file)
        ;; In a full implementation, this would read the dataset
        ;; and export it as an image file
        (message "Image export not yet fully implemented"))
    (error "No HDF5 file is currently open")))

(defun h5-export-as-csv ()
  "Export the dataset at point as a CSV file."
  (interactive)
  (if h5-current-file
      (let* ((dataset-path (read-string "Dataset path: "))
             (output-file (read-file-name "Export to: " nil nil nil "data.csv")))
        (message "Exporting dataset %s to %s..." dataset-path output-file)
        (let ((dataset (h5-io-read-dataset h5-current-file dataset-path)))
          ;; In a full implementation, this would format the dataset
          ;; data as CSV and write it to the output file
          (with-temp-file output-file
            (insert "# CSV export from HDF5 dataset: " dataset-path "\n")
            ;; Add actual data writing here
            )
          (message "CSV export complete")))
    (error "No HDF5 file is currently open")))

(defun h5-search ()
  "Search for values in the HDF5 file."
  (interactive)
  (let ((search-term (read-string "Search for: ")))
    (message "Searching for: %s" search-term)
    ;; In a full implementation, this would search through
    ;; datasets and attributes for the search term
    (message "Search not yet fully implemented")))

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

(put 'h5-mode 'mode-class 'special)
(provide 'h5-mode)

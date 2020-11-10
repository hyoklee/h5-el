;;; h5-mode.el --- major mode for editing HDF5 file

;; Copyright (C) 2020 Hyokyung Lee
;; Author: Hyokyung Lee
;; Created: May 2020
;; Version: 0.0.1
;; Keywords: hdf5
;; URL: https://github.com/hyoklee/h5-el
;; Package-Requires: ((seq "2.20") (emacs "24.3"))

(eval-when-compile (require 'cl-lib))
(require 'arc-mode)
(defgroup h5 nil
  "Simple editing of h5 files."
  :prefix "h5-"
  :group 'data)

(defvar h5-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map " " 'h5-next-line)
    (define-key map "C" 'h5-copy)))

(define-derived-mode h5-mode special-mode "h5"
  (and buffer-file-name
       (file-writable-p buffer-file-name)
       (setq buffer-read-only nil)))
       
;;;
(put 'h5-mode 'mode-class 'special)
(provide 'h5-mode)

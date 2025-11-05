;;; sample-emacs-config.el --- Sample Emacs configuration for h5-mode

;; This file demonstrates how to configure h5-mode for automatic
;; activation when opening HDF5 files from dired mode.

;;; Commentary:

;; To use this configuration:
;; 1. Copy the relevant sections to your ~/.emacs or ~/.emacs.d/init.el
;; 2. Adjust the path to h5-el directory as needed
;; 3. Restart Emacs or evaluate the configuration

;;; Code:

;; ============================================================================
;; Basic Setup - Add h5-el to load path
;; ============================================================================

;; Replace "/path/to/h5-el" with the actual path where h5-el is installed
(add-to-list 'load-path "/path/to/h5-el")

;; Load h5-mode (this will also load h5-io automatically)
(require 'h5-mode)

;; ============================================================================
;; Automatic Mode Association
;; ============================================================================

;; These lines are already included in h5-mode.el, but shown here for reference
;; h5-mode will automatically activate for files with .h5 or .hdf5 extensions
;; (add-to-list 'auto-mode-alist '("\\.h5\\'" . h5-mode))
;; (add-to-list 'auto-mode-alist '("\\.hdf5\\'" . h5-mode))

;; ============================================================================
;; Dired Integration
;; ============================================================================

;; When you press RETURN on an HDF5 file in dired, it will automatically
;; open in h5-mode. This is already handled by auto-mode-alist above.

;; Optional: Add a custom dired binding to force h5-mode for any file
(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "C-c h")
     (lambda ()
       (interactive)
       (let ((file (dired-get-file-for-visit)))
         (find-file file)
         (h5-mode)))))

;; ============================================================================
;; Performance Settings
;; ============================================================================

;; CRITICAL: Partial File Reading (prevents loading entire large files)
;; By default, h5-mode reads only the first 1MB of the file.
;; This is essential for handling large files (e.g., 19GB) without
;; consuming all available memory.

;; Default: 1MB (recommended for most use cases)
;; (setq h5-initial-read-bytes (* 1024 1024))

;; For deeper metadata access, increase the read size:
;; (setq h5-initial-read-bytes (* 10 1024 1024))  ; 10MB

;; For ultra-fast loading, decrease the read size:
;; (setq h5-initial-read-bytes (* 512 1024))  ; 512KB

;; To read entire file (NOT recommended for large files):
;; (setq h5-initial-read-bytes nil)

;; View Depth Control
;; By default, h5-mode shows only top-level groups for faster loading.
;; You can customize this behavior:

;; Show only top-level (depth 1) by default - this is the default setting
;; (setq-default h5-show-depth 1)

;; Show 2 levels by default
;; (setq-default h5-show-depth 2)

;; Show all levels by default (may be slow for large files)
;; (setq-default h5-show-depth nil)

;; ============================================================================
;; Key Bindings Reference
;; ============================================================================

;; When in h5-mode, the following key bindings are available:
;;
;; Navigation:
;;   RET   - Open/view object at point (group, dataset, or attribute)
;;   SPC   - Move to next line
;;   q     - Quit window
;;
;; View Control:
;;   a     - Show all levels (full tree)
;;   1     - Show only top-level (depth 1)
;;   2     - Show up to depth 2
;;   3     - Show up to depth 3
;;   g     - Refresh display
;;
;; Group Operations:
;;   +     - Create group (not yet fully implemented)
;;   d     - Delete group (not yet fully implemented)
;;   s     - Sort group (not yet fully implemented)
;;
;; Dataset Operations:
;;   i     - View dataset as image (requires image libraries)
;;   e     - Export dataset as image (requires image libraries)
;;   E     - Export dataset as CSV
;;
;; Search:
;;   C-s   - Search for values in the file
;;
;; Other:
;;   C     - Copy object (not yet implemented)

;; ============================================================================
;; Example Usage
;; ============================================================================

;; 1. From dired:
;;    - Navigate to a directory with HDF5 files (C-x d)
;;    - Move cursor to an .h5 or .hdf5 file
;;    - Press RETURN
;;    - The file will open in h5-mode showing only top-level groups
;;
;; 2. To view more details:
;;    - Press 'a' to show all levels
;;    - Press '2' to show 2 levels
;;    - Press '1' to return to top-level view
;;
;; 3. To export a dataset:
;;    - Navigate to a dataset line
;;    - Press 'E' to export as CSV
;;    - Choose output filename
;;
;; 4. To search:
;;    - Press C-s
;;    - Enter search term
;;    - Results will be displayed in a new buffer

;; ============================================================================
;; Advanced Configuration
;; ============================================================================

;; Custom hook to run when entering h5-mode
(add-hook 'h5-mode-hook
          (lambda ()
            ;; Example: Set buffer to read-only by default
            (setq buffer-read-only t)
            ;; Example: Enable line numbers
            (when (fboundp 'display-line-numbers-mode)
              (display-line-numbers-mode 1))))

;; ============================================================================
;; Troubleshooting
;; ============================================================================

;; If h5-mode doesn't activate automatically:
;;
;; 1. Check that the file has .h5 or .hdf5 extension
;; 2. Verify h5-mode is in your load-path: M-x locate-library RET h5-mode
;; 3. Check auto-mode-alist: M-: auto-mode-alist
;; 4. Force h5-mode: M-x h5-mode
;;
;; If loading consumes too much memory or is too slow:
;;
;; 1. CRITICAL: By default, only 1MB is read - this should be fast
;; 2. Check h5-initial-read-bytes value: M-: h5-initial-read-bytes
;; 3. Reduce if needed: (setq h5-initial-read-bytes (* 512 1024))
;; 4. The default depth is 1 (top-level only) for faster display
;; 5. Only the beginning of the file is loaded, not the entire file
;;
;; If Emacs hangs or runs out of memory:
;;
;; 1. Make sure h5-initial-read-bytes is NOT nil (which reads entire file)
;; 2. Default value should be 1048576 (1MB)
;; 3. For 19GB files, 1MB is sufficient for superblock and top-level metadata
;;
;; If you see errors:
;;
;; 1. Ensure h5-io.el is in the same directory as h5-mode.el
;; 2. Check that the file is a valid HDF5 file
;; 3. Look for error messages in *Messages* buffer (C-h e)
;; 4. If metadata is deeper in file, increase h5-initial-read-bytes

;;; sample-emacs-config.el ends here
